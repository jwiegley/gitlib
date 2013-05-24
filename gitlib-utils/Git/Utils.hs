{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Git.Utils where

import           Control.Applicative
import qualified Control.Exception.Lifted as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as CList
import           Data.Function
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable hiding (mapM, forM, sequence)
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS hiding (null, concat)
import           Git
import           Prelude hiding (FilePath)

treeOid :: Repository m => Tree m -> m Text
treeOid t = renderObjOid <$> writeTree t

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Repository m => Text -> m ByteString
catBlob str =
    if len == 40
    then do
        oid <- parseOid str
        lookupBlob (Tagged oid) >>= blobToByteString

    else do
        obj <- lookupObject str
        case obj of
            BlobObj (ByOid oid) -> lookupBlob oid >>= blobToByteString
            BlobObj (Known x)   -> blobToByteString x
            _ -> failure (ObjectLookupFailed str len)
  where
    len = T.length str

catBlobUtf8 :: Repository m => Text -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobContentsToByteString :: Repository m => BlobContents m -> m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobContentsToByteString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

blobToByteString :: Repository m => Blob m -> m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

treeBlobEntries :: Repository m => Tree m -> m [(FilePath,TreeEntry m)]
treeBlobEntries tree =
    mconcat <$> traverseEntries tree go
  where
    go fp e@(BlobEntry _ PlainBlob) = return [(fp,e)]
    go fp e@(BlobEntry _ ExecutableBlob) = return [(fp,e)]
    go _ _ = return []

commitTreeEntry :: Repository m
                => Commit m
                -> FilePath
                -> m (Maybe (TreeEntry m))
commitTreeEntry c path =
    flip lookupEntry path =<< resolveTreeRef (commitTree c)

copyOid :: (Repository m, Repository (t m), MonadTrans t)
        => Oid m -> t m (Oid (t m))
copyOid = parseOid . renderOid

copyBlob :: (Repository m, Repository (t m), MonadTrans t)
         => BlobRef m
         -> HashSet Text
         -> t m (BlobOid (t m), HashSet Text)
copyBlob blobr needed = do
    let oid = unTagged (blobRefOid blobr)
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        bs <- lift $ blobToByteString
              =<< resolveBlobRef (ByOid (Tagged oid))
        boid <- createBlob (BlobString bs)

        let x = HashSet.delete sha needed
        return $ boid `seq` x `seq` (boid, x)

        else return (Tagged oid2, needed)

copyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
              => TreeEntry m
              -> HashSet Text
              -> t m (TreeEntry (t m), HashSet Text)
copyTreeEntry (BlobEntry oid kind) needed = do
    (b,needed') <- copyBlob (ByOid oid) needed
    return (BlobEntry b kind, needed')
copyTreeEntry (CommitEntry oid) needed = do
    coid <- parseOid (renderObjOid oid)
    return (CommitEntry (Tagged coid), needed)
copyTreeEntry (TreeEntry _) _ = error "This should never be called"

copyTree :: (Repository m, Repository (t m), MonadTrans t)
         => TreeRef m
         -> HashSet Text
         -> t m (TreeRef (t m), HashSet Text)
copyTree tr needed = do
    oid <- unTagged <$> lift (treeRefOid tr)
    let sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        tree    <- lift $ resolveTreeRef tr
        entries <- lift $ traverseEntries tree (curry return)
        tree2   <- newTree
        needed' <- foldM (doCopyTreeEntry tree2) needed entries
        toid    <- writeTree tree2

        let tref = ByOid toid
            x    = HashSet.delete sha needed'
        return $ tref `seq` x `seq` (tref, x)

        else return (ByOid (Tagged oid2), needed)
  where
    doCopyTreeEntry _ needed' (_,TreeEntry {}) = return needed'
    doCopyTreeEntry tree2 needed' (fp,ent) = do
        (ent2,needed'') <- copyTreeEntry ent needed'
        putTreeEntry tree2 fp ent2
        return needed''

copyCommit :: (Repository m, Repository (t m), MonadTrans t)
           => CommitRef m
           -> Maybe Text
           -> HashSet Text
           -> t m (CommitRef (t m), HashSet Text)
copyCommit cr mref needed = do
    let oid = unTagged (commitRefOid cr)
        sha = renderOid oid
    commit <- lift $ resolveCommitRef cr
    oid2   <- parseOid sha
    if HashSet.member sha needed
        then do
        let parents = commitParents commit
        (parentRefs,needed') <- foldM copyParent ([],needed) parents
        (tr,needed'') <- copyTree (commitTree commit) needed'

        commit <- createCommit (reverse parentRefs) tr
            (commitAuthor commit)
            (commitCommitter commit)
            (commitLog commit)
            mref

        let cref = commitRef $! commit
            x    = HashSet.delete sha needed''
        return $ cref `seq` x `seq` (cref, x)

        else return (ByOid (Tagged oid2), needed)
  where
    copyParent (prefs,needed') cref = do
        (cref2,needed'') <- copyCommit cref Nothing needed'
        let x = cref2 `seq` (cref2:prefs)
        return $ x `seq` needed'' `seq` (x,needed'')

-- | Given a list of objects (commit and top-level trees) return by
--   'missingObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
allMissingObjects :: Repository m => [Object m] -> m [Object m]
allMissingObjects objs =
    fmap concat . forM objs $ \obj -> case obj of
        TreeObj ref -> do
            tr       <- resolveTreeRef ref
            subobjss <- traverseEntries tr $ \_ ent ->
                return $ case ent of
                    Git.BlobEntry oid _ -> [Git.BlobObj (Git.ByOid oid)]
                    Git.TreeEntry tr'   -> [Git.TreeObj tr']
                    _ -> []
            return (obj:concat subobjss)
        _ -> return [obj]

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works.
genericPushCommit :: (Repository m, Repository (t m),
                      MonadTrans t, MonadIO (t m))
                  => CommitName m -> Text -> t m (CommitRef (t m))
genericPushCommit cname remoteRefName = do
    mrref    <- lookupRef remoteRefName
    commits1 <- lift $ traverseCommits crefToSha cname
    fastForward <- case mrref of
        Just rref -> do
            mrsha <- referenceSha rref
            case mrsha of
                Nothing -> failure (Git.PushNotFastForward $
                                    "Could not find SHA for " <> remoteRefName)
                Just rsha
                    | rsha `elem` commits1 -> do
                        roid <- lift $ parseOid rsha
                        return $ Just (Just (CommitObjectId (Tagged roid)))
                    | otherwise -> do
                        mapM_ (liftIO . putStrLn . T.unpack) commits1
                        failure (Git.PushNotFastForward $
                                 "SHA " <> rsha
                                        <> " not found in remote")
        Nothing -> return (Just Nothing)
    case fastForward of
        Nothing -> failure (Git.PushNotFastForward "unexpected")
        Just liftedMrref -> do
            objs <- lift $ allMissingObjects
                        =<< missingObjects liftedMrref cname
            shas <- mapM (\obj -> renderOid <$> lift (objectOid obj)) objs
            mref <- lift $ commitNameToRef cname
            case mref of
                Nothing -> failure (ReferenceLookupFailed (T.pack (show cname)))
                Just ref -> do
                    (cref,_) <- copyCommit ref Nothing (HashSet.fromList shas)
                    -- jww (2013-04-18): This is something the user must
                    -- decide to do
                    -- updateRef_ remoteRefName (RefObj cref)
                    return cref
  where
    referenceSha ref = do
        r <- referenceToRef Nothing (Just ref)
        return $ renderObjOid . commitRefOid <$> r

    crefToSha cref  = return (renderObjOid (commitRefOid cref))

commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
commitHistoryFirstParent c =
    case commitParents c of
        []    -> return [c]
        (p:_) -> do ps <- commitHistoryFirstParent =<< resolveCommitRef p
                    return (c:ps)

data PinnedEntry m = PinnedEntry
    { pinnedOid    :: Oid m
    , pinnedCommit :: Commit m
    , pinnedEntry  :: TreeEntry m
    }

identifyEntry :: Repository m => Commit m -> TreeEntry m -> m (PinnedEntry m)
identifyEntry co x = do
    oid <- case x of
        BlobEntry oid _ -> return (unTagged oid)
        TreeEntry ref   -> unTagged <$> treeRefOid ref
        CommitEntry oid -> return (unTagged oid)
    return (PinnedEntry oid co x)

commitEntryHistory :: Repository m => Commit m -> FilePath -> m [PinnedEntry m]
commitEntryHistory c path =
    map head . filter (not . null) . groupBy ((==) `on` pinnedOid) <$> go c
  where
    go co = do
        entry <- getEntry co
        rest  <- case commitParents co of
            []    -> return []
            (p:_) -> go =<< resolveCommitRef p
        return $ maybe rest (:rest) entry

    getEntry co = do
        ce <- commitTreeEntry co path
        case ce of
            Nothing  -> return Nothing
            Just ce' -> Just <$> identifyEntry co ce'

getCommitParents :: Repository m => Commit m -> m [Commit m]
getCommitParents = traverse resolveCommitRef . commitParents

resolveRefTree :: Repository m => Text -> m (Tree m)
resolveRefTree refName = do
    c <- resolveRef refName
    case c of
        Nothing -> newTree
        Just c' -> resolveCommitRef c' >>= resolveTreeRef . commitTree

withNewRepository :: (Repository (t m), MonadGit (t m),
                      MonadBaseControl IO m, MonadIO m, MonadTrans t)
                  => RepositoryFactory t m c
                  -> FilePath -> t m a -> m a
withNewRepository factory path action = do
    liftIO $ do
        exists <- isDirectory path
        when exists $ removeTree path

    -- we want exceptions to leave the repo behind
    a <- withRepository' factory (defaultOptions factory)
        { repoPath       = path
        , repoIsBare     = True
        , repoAutoCreate = True
        } action

    liftIO $ do
        exists <- isDirectory path
        when exists $ removeTree path

    return a

withNewRepository' :: (Repository (t m), MonadGit (t m),
                       MonadBaseControl IO m, MonadIO m, MonadTrans t)
                   => RepositoryFactory t m c -> FilePath -> t m a -> m a
withNewRepository' factory path action =
    Exc.bracket_ recover recover $
        withRepository' factory (defaultOptions factory)
            { repoPath       = path
            , repoIsBare     = True
            , repoAutoCreate = True
            } action
  where
    recover = liftIO $ do
        exists <- isDirectory path
        when exists $ removeTree path


-- Utils.hs ends here
