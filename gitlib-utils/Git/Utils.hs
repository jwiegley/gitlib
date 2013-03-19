{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Git.Utils where

import           Control.Applicative
import           Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable hiding (mapM, forM, sequence)
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS hiding (null)
import           Git
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

oid :: Repository m => Tree m -> m Text
oid t = renderObjOid <$> writeTree t

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Repository m => Text -> m ByteString
catBlob str = do
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
    mconcat <$> traverseEntries tree (\fp e -> case e of
                                           BlobEntry _ kind ->
                                               if kind == PlainBlob
                                               then return [(fp,e)]
                                               else return []
                                           _ -> return [])

commitTreeEntry :: Repository m
                => Commit m
                -> FilePath
                -> m (Maybe (TreeEntry m))
commitTreeEntry c path =
    flip lookupEntry path =<< resolveTreeRef (commitTree c)

copyOid :: (Repository m, Repository (t m), MonadTrans t)
        => Oid m -> t m (Oid (t m))
copyOid oid = parseOid (renderOid oid)

copyBlob :: (Repository m, Repository (t m), MonadTrans t)
         => BlobRef m -> t m (BlobOid (t m))
copyBlob blobr = do
    let oid = unTagged (blobRefOid blobr)
    oid2    <- parseOid (renderOid oid)
    exists2 <- existsObject oid2
    if exists2
        then return (Tagged oid2)
        else do
            bs <- lift $ blobToByteString
                  =<< resolveBlobRef (ByOid (Tagged oid))
            createBlob (BlobString bs)

copyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
              => TreeEntry m -> t m (TreeEntry (t m))
copyTreeEntry (BlobEntry oid kind) =
    BlobEntry <$> copyBlob (ByOid oid) <*> pure kind
copyTreeEntry (CommitEntry oid) =
    return . CommitEntry . Tagged =<< parseOid (renderObjOid oid)

copyTree :: (Repository m, Repository (t m), MonadTrans t)
         => TreeRef m -> t m (TreeRef (t m))
copyTree tr = do
    oid     <- unTagged <$> (lift $ treeRefOid tr)
    oid2    <- parseOid (renderOid oid)
    exists2 <- existsObject oid2
    if exists2
        then return (ByOid (Tagged oid2))
        else do
            tree    <- lift $ resolveTreeRef tr
            entries <- lift $ traverseEntries tree (curry return)
            tree2   <- newTree
            forM_ entries $ \(fp,ent) -> do
                case ent of
                    TreeEntry {} -> return ()
                    _ -> putTreeEntry tree2 fp =<< copyTreeEntry ent
            tree <- writeTree tree2
            return $ ByOid $! tree

copyCommitRec' :: (Repository m, Repository (t m), MonadTrans t)
              => CommitRef m
              -> Maybe Text
              -> HashMap Text (CommitRef (t m))
              -> t m (CommitRef (t m), HashMap Text (CommitRef (t m)))
copyCommitRec' cr mref seen = do
    let oid = unTagged (commitRefOid cr)
        sha = renderOid oid
    case HashMap.lookup sha seen of
        Just c -> return (c,seen)
        Nothing -> do
            commit   <- lift $ resolveCommitRef cr
            oid2     <- parseOid sha
            exists2  <- existsObject oid2
            if exists2
                then return (ByOid (Tagged oid2), seen)
                else do
                    let parents = commitParents commit
                    (parentRefs,seen') <-
                        foldM copyParent ([],seen) parents

                    tr <- copyTree (commitTree commit)
                    commit <- createCommit parentRefs tr
                        (commitAuthor commit)
                        (commitCommitter commit)
                        (commitLog commit)
                        mref
                    let cref = commitRef $! commit
                        x    = HashMap.insert sha cref seen
                    return $ x `seq` cref `seq` (cref, x)
  where
    copyParent (prefs,seen') cref = do
        (cref2,seen'') <- copyCommitRec' cref Nothing seen'
        let x = cref2 `seq` (cref2:prefs)
        return $ x `seq` seen'' `seq` (x,seen'')

copyCommitRec :: (Repository m, Repository (t m), MonadTrans t)
              => CommitRef m
              -> Maybe Text
              -> t m (CommitRef (t m), HashMap Text (CommitRef (t m)))
copyCommitRec cr mref = copyCommitRec' cr mref HashMap.empty

copyCommit :: (Repository m, Repository (t m), MonadTrans t)
           => CommitRef m
           -> Maybe Text
           -> t m (CommitRef (t m))
copyCommit cr mref =  do
    let oid = unTagged (commitRefOid cr)
        sha = renderOid oid
    commit   <- lift $ resolveCommitRef cr
    oid2     <- parseOid sha
    exists2  <- existsObject oid2
    if exists2
        then return $ ByOid (Tagged oid2)
        else do
            parentRefs <- mapM parentRef (commitParents commit)
            tr <- copyTree (commitTree commit)
            commit <- createCommit parentRefs tr
                (commitAuthor commit)
                (commitCommitter commit)
                (commitLog commit)
                mref
            return $ commitRef $! commit
  where
    parentRef cref = do
        let oid = unTagged (commitRefOid cref)
            sha = renderOid oid
        oid2 <- parseOid sha
        return $ ByOid (Tagged oid2)

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works.
genericPushRef :: (Repository m, Repository (t m), MonadTrans t)
               => Reference m (Commit m)
               -> Text
               -> t m Bool
genericPushRef ref remoteRefName = do
    mrref       <- lookupRef remoteRefName
    commits1    <- lift $ traverseCommits crefToSha ref
    fastForward <- case mrref of
        Just rref -> do
            mrsha <- referenceSha rref
            case mrsha of
                Nothing   -> return True
                Just rsha -> return $ rsha `elem` commits1
        Nothing -> return True
    if not fastForward
        then return False
        else do
        commits2 <- case mrref of
            Nothing   -> return HashMap.empty
            Just rref -> HashMap.fromList <$>
                         traverseCommits crefToPair rref
        case refTarget ref of
            RefObj cr -> do
                remoteHead  <- resolveRef remoteRefName
                (cref,seen) <- copyCommitRec' cr Nothing commits2
                case remoteHead of
                    Nothing -> do
                        createRef remoteRefName (RefObj cref)
                        return True
                    Just rh -> do
                        let sha = renderObjOid (commitRefOid rh)
                        if HashMap.member sha seen
                            then do
                                updateRef remoteRefName (RefObj cref)
                                return True
                            else return False
            _ -> return False
  where
    referenceSha ref = do
        r <- referenceToRef Nothing (Just ref)
        return $ renderObjOid . commitRefOid <$> r
    crefToSha cref  = return (renderObjOid (commitRefOid cref))
    crefToPair cref = (,) <$> crefToSha cref <*> pure cref

commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
commitHistoryFirstParent c =
    case commitParents c of
        []    -> return [c]
        (p:_) -> do ps <- commitHistoryFirstParent c
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

-- Utils.hs ends here