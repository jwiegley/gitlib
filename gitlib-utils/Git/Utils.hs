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
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hex
import           Data.IORef
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Traversable hiding (mapM, forM, sequence)
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS hiding (null, concat)
import           Git
import           Prelude hiding (FilePath)

data OidBytestring = OidBytestring { getOidBS :: ByteString }
                   deriving (Eq, Ord, Show)

instance IsOid OidBytestring where
    renderOid (OidBytestring x) = T.toLower (T.decodeUtf8 (hex x))

parseOidBytestring :: Monad m => Text -> m OidBytestring
parseOidBytestring x = OidBytestring `liftM` unhex (T.encodeUtf8 x)

data OidText = OidText { getOidT :: T.Text }
             deriving (Eq, Ord, Show)

instance IsOid OidText where
    renderOid (OidText x) = x

parseOidText :: Monad m => Text -> m OidText
parseOidText = return . OidText

data OidTextL = OidTextL { getOidTL :: TL.Text }
              deriving (Eq, Ord, Show)

instance IsOid OidTextL where
    renderOid (OidTextL x) = TL.toStrict x

parseOidTextL :: Monad m => Text -> m OidTextL
parseOidTextL = return . OidTextL . TL.fromStrict

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

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

data MutableTreeBuilder m = MutableTreeBuilder
    { mtbPendingUpdates :: IORef (HashMap Text (MutableTreeBuilder m))
    , mtbNewBuilder     :: Maybe (Tree m) -> m (MutableTreeBuilder m)
    , mtbWriteContents  :: m (TreeRef m)
    , mtbLookupEntry    :: Text -> m (Maybe (TreeEntry m))
    , mtbEntryCount     :: m Int
    , mtbPutEntry       :: Text -> TreeEntry m -> m ()
    , mtbDropEntry      :: Text -> m ()
    }

emptyTreeId :: Text
emptyTreeId = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

makeMutableTreeBuilder :: (Repository m, MonadIO m) => m (MutableTreeBuilder m)
makeMutableTreeBuilder = do
    ior <- liftIO (newIORef HashMap.empty)
    return MutableTreeBuilder
        { mtbPendingUpdates = ior
        , mtbNewBuilder     = error "MutableTreeBuilder.mtbNewBuilder"
        , mtbWriteContents  = error "MutableTreeBuilder.mtbWriteContents"
        , mtbLookupEntry    = \_ -> return Nothing
        , mtbEntryCount     = return 0
        , mtbPutEntry       = \_ _ -> return ()
        , mtbDropEntry      = \_ -> return ()
        }

updateMutableTreeBuilder
    :: (Repository m, MonadIO m)
    => MutableTreeBuilder m -> FilePath -> Bool
    -> (Maybe (TreeEntry m) -> ModifyTreeResult m)
    -> m (MutableTreeBuilder m, Maybe (TreeEntry m))
updateMutableTreeBuilder tb' path createIfNotExist f =
    fmap fromModifyTreeResult <$> doModifyTree tb' (splitPath path)
  where
    -- Lookup the current name in this tree.  If it doesn't exist, and there
    -- are more names in the path and 'createIfNotExist' is True, create a new
    -- Tree and descend into it.  Otherwise, if it exists we'll have @Just
    -- (TreeEntry {})@, and if not we'll have Nothing.
    doModifyTree tb [] = do
        tref <- writeMutatedTree tb
        return (tb, f . Just . TreeEntry . treeRefOid $ tref)

    doModifyTree tb (name:names) = do
        upds <- liftIO $ readIORef (mtbPendingUpdates tb)
        y <- case HashMap.lookup name upds of
            Just x  -> return $ Left x
            Nothing -> do
                mentry <- mtbLookupEntry tb name
                case mentry of
                    Nothing
                        | createIfNotExist && not (null names) ->
                            Left <$> mtbNewBuilder tb Nothing
                        | otherwise -> return $ Right Nothing
                    Just entry -> return $ Right (Just entry)
        go tb name names y

    -- If there are no further names in the path, call the transformer
    -- function, f.  It receives a @Maybe TreeEntry@ to indicate if there was
    -- a previous entry at this path.  It should return a 'Left' value to
    -- propagate out a user-defined error, or a @Maybe TreeEntry@ to indicate
    -- whether the entry at this path should be deleted or replaced with
    -- something new.
    --
    -- NOTE: There is no provision for leaving the entry unchanged!  It is
    -- assumed to always be changed, as we have no reliable method of testing
    -- object equality that is not O(n).
    go _  _    [] (Left stb)               = doModifyTree stb []
    go tb name [] (Right y)                = returnTree tb name (f y)
    go tb _    _  (Right Nothing)          = return (tb, TreeEntryNotFound)
    go _ _ _ (Right (Just BlobEntry {}))   = failure TreeCannotTraverseBlob
    go _ _ _ (Right (Just CommitEntry {})) = failure TreeCannotTraverseCommit

    -- If there are further names in the path, descend them now.  If
    -- 'createIfNotExist' was False and there is no 'Tree' under the current
    -- name, or if we encountered a 'Blob' when a 'Tree' was required, throw
    -- an exception to avoid colliding with user-defined 'Left' values.
    go tb name names arg = do
        stb <- case arg of
            Left stb' -> return stb'
            Right (Just (TreeEntry st')) -> do
                tree <- lookupTree st'
                mtbNewBuilder tb (Just tree)
            _ -> error "Impossible"
        (st'', ze) <- doModifyTree stb names
        case ze of
            TreeEntryNotFound     -> return ()
            TreeEntryPersistent _ -> return ()
            TreeEntryDeleted      -> postUpdate tb st'' name
            TreeEntryMutated _    -> postUpdate tb st'' name
        return (tb, ze)
      where
        postUpdate tb st name =
            liftIO $ modifyIORef (mtbPendingUpdates tb) $
                HashMap.insert name st

    returnTree tb n z = do
        case z of
            TreeEntryNotFound     -> return ()
            TreeEntryPersistent _ -> return ()
            TreeEntryDeleted      -> mtbDropEntry tb n
            TreeEntryMutated z'   -> mtbPutEntry tb n z'
        return (tb, z)

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeMutatedTree :: (Repository m, MonadIO m)
                 => MutableTreeBuilder m -> m (TreeRef m)
writeMutatedTree t = do
    -- This is the Oid of every empty tree
    emptyTreeOid <- parseObjOid emptyTreeId
    fromMaybe (ByOid emptyTreeOid) <$> doWriteTree t
  where
    doWriteTree tb = do
        upds <- liftIO $ readIORef (mtbPendingUpdates tb)
        forM_ (HashMap.toList upds) $ \(k,v) -> do
            mtref <- doWriteTree v
            case mtref of
                Nothing   -> mtbDropEntry tb k
                Just tref -> mtbPutEntry tb k (TreeEntry (treeRefOid tref))
        liftIO $ writeIORef (mtbPendingUpdates tb) HashMap.empty

        cnt <- mtbEntryCount tb
        if cnt == 0
            then return Nothing
            else Just <$> mtbWriteContents tb

treeBlobEntries :: Repository m => Tree m -> m [(FilePath,TreeEntry m)]
treeBlobEntries tree =
    mconcat <$> traverseEntries go tree
  where
    go fp e@(BlobEntry _ PlainBlob) = return [(fp,e)]
    go fp e@(BlobEntry _ ExecutableBlob) = return [(fp,e)]
    go _ _ = return []

commitTreeEntry :: Repository m
                => Commit m
                -> FilePath
                -> m (Maybe (TreeEntry m))
commitTreeEntry c path =
    flip getTreeEntry path =<< resolveTreeRef (commitTree c)

copyOid :: (Repository m, Repository (t m), MonadTrans t)
        => Oid m -> t m (Oid (t m))
copyOid = parseOid . renderOid

copyBlob :: (Repository m, Repository (t m), MonadTrans t)
         => BlobRef m
         -> HashSet Text
         -> t m (BlobOid (t m), HashSet Text)
copyBlob blobr needed = do
    let oid = untag (blobRefOid blobr)
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
    let oid = untag (treeRefOid tr)
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        tree            <- lift $ resolveTreeRef tr
        entries         <- lift $ traverseEntries (curry return) tree
        (needed', tref) <- withNewTree $ foldM doCopyTreeEntry needed entries

        let x = HashSet.delete sha needed'
        return $ tref `seq` x `seq` (tref, x)

        else return (ByOid (Tagged oid2), needed)
  where
    doCopyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
                    => HashSet Text -> (FilePath, TreeEntry m)
                    -> TreeT (t m) (HashSet Text)
    doCopyTreeEntry needed' (_,TreeEntry {}) = return needed'
    doCopyTreeEntry needed' (fp,ent) = do
        (ent2,needed'') <- lift $ copyTreeEntry ent needed'
        putEntry fp ent2
        return needed''

copyCommit :: (Repository m, Repository (t m), MonadTrans t)
           => CommitRef m
           -> Maybe Text
           -> HashSet Text
           -> t m (CommitRef (t m), HashSet Text)
copyCommit cr mref needed = do
    let oid = untag (commitRefOid cr)
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
            subobjss <- flip traverseEntries tr $ \_ ent ->
                return $ case ent of
                    BlobEntry oid _ -> [BlobObj (ByOid oid)]
                    TreeEntry oid   -> [TreeObj (ByOid oid)]
                    _ -> []
            return (obj:concat subobjss)
        _ -> return [obj]

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works.
genericPushCommit :: (Repository m, Repository (t m),
                      MonadTrans t, MonadIO (t m))
                  => CommitName m -> Text -> t m (CommitRef (t m))
genericPushCommit cname remoteRefName = do
    mrref    <- lookupReference remoteRefName
    commits1 <- lift $ traverseCommits crefToSha cname
    fastForward <- case mrref of
        Just rref -> do
            mrsha <- referenceSha rref
            case mrsha of
                Nothing -> failure (PushNotFastForward $
                                    "Could not find SHA for " <> remoteRefName)
                Just rsha
                    | rsha `elem` commits1 -> do
                        roid <- lift $ parseOid rsha
                        return $ Just (Just (CommitObjectId (Tagged roid)))
                    | otherwise -> do
                        mapM_ (liftIO . putStrLn . T.unpack) commits1
                        failure (PushNotFastForward $
                                 "SHA " <> rsha
                                        <> " not found in remote")
        Nothing -> return (Just Nothing)
    case fastForward of
        Nothing -> failure (PushNotFastForward "unexpected")
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
    let oid = case x of
            BlobEntry oid _ -> untag oid
            TreeEntry oid   -> untag oid
            CommitEntry oid -> untag oid
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

resolveRefTree :: Repository m => Text -> m (Maybe (Tree m))
resolveRefTree refName = do
    c <- resolveReference refName
    case c of
        Nothing -> return Nothing
        Just c' ->
            Just <$> (resolveCommitRef c' >>= resolveTreeRef . commitTree)

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
