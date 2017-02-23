{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing
                -fno-warn-unused-binds
                -fno-warn-orphans #-}

-- | Interface for opening and creating repositories.  Repository objects are
--   immutable, and serve only to refer to the given repository.  Any data
--   associated with the repository — such as the list of branches — is
--   queried as needed.
module Git.Libgit2
       ( MonadLg
       , LgRepo(..)
       , BlobOid()
       , Commit()
       , CommitOid()
       , Git.Oid
       , OidPtr(..)
       , mkOid
       , Tree()
       , TreeOid()
       , lgRepoPath
       , addTracingBackend
       , checkResult
       , lgBlobRawSize
       , lgBuildPackIndex
       , lgFactory
       , lgForEachObject
       , lgExcTrap
       , lgBuildPackFile
       , lgReadFromPack
       , lgOpenPackFile
       , lgClosePackFile
       , lgWithPackFile
       , lgCopyPackFile
       , lgDiffContentsWithTree
       , lgWrap
       , oidToSha
       , shaToCOid
       , shaToOid
       , openLgRepository
       , runLgRepository
       , lgDebug
       , lgWarn
       ) where

import           Bindings.Libgit2
import           Conduit
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async.Lifted
import           Control.Concurrent.STM
--import           Control.Exception.Lifted
import           Control.Monad hiding (forM, forM_, mapM, mapM_, sequence)
import           Control.Monad.Catch
import           Control.Monad.Loops
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Control.Monad.Trans.Resource
import           Data.Bits ((.|.), (.&.))
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import           Data.Conduit.Async
import           Data.Foldable
import           Data.IORef
import           Data.List as L
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as U
import           Data.Traversable
import           Foreign.C.String
import           Foreign.C.Types
import qualified Foreign.Concurrent as FC
import           Foreign.ForeignPtr
import qualified Foreign.ForeignPtr.Unsafe as FU
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
#ifdef missing_calloc
import           Foreign.Marshal.MissingAlloc
#endif
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Language.Haskell.TH.Syntax (Loc(..))

import           Prelude hiding (mapM, mapM_, sequence, catch)
import           System.Directory
import           System.FilePath.Posix
import           System.IO (openBinaryTempFile, hClose)
import qualified System.IO.Unsafe as SU
import           Unsafe.Coerce
import Data.Word

lgDebug :: MonadIO m => String -> m ()
lgDebug = liftIO . putStrLn . ("[DEBUG] " ++)

lgWarn :: MonadIO m => String -> m ()
lgWarn = liftIO . putStrLn . ("[WARN] " ++)

withFilePath :: Git.RawFilePath -> (CString -> IO a) -> IO a
withFilePath = B.useAsCString

peekFilePath :: CString -> IO Git.RawFilePath
peekFilePath = B.packCString

type Oid = OidPtr

data OidPtr = OidPtr
    { getOid    :: ForeignPtr C'git_oid
    , getOidLen :: Int           -- the number of digits, not bytes
    }

instance Git.IsOid OidPtr where
    renderOid = lgRenderOid

mkOid :: ForeignPtr C'git_oid -> OidPtr
mkOid fptr = OidPtr fptr 40

lgParseOidIO :: Text -> Int -> IO (Maybe Oid)
lgParseOidIO str len = do
    oid <- liftIO mallocForeignPtr
    r <- liftIO $ withCString (unpack str) $ \cstr ->
        withForeignPtr oid $ \ptr ->
            if len == 40
            then c'git_oid_fromstr ptr cstr
            else c'git_oid_fromstrn ptr cstr (fromIntegral len)
    return $ if r < 0
             then Nothing
             else Just (OidPtr oid len)

lgParseOid :: MonadLg m => Text -> m Oid
lgParseOid str
  | len > 40 = throwM (Git.OidParseFailed str)
  | otherwise = do
      moid <- liftIO $ lgParseOidIO str len
      case moid of
          Nothing  -> throwM (Git.OidParseFailed str)
          Just oid -> return oid
  where
    len = T.length str

lgRenderOid :: Oid -> Text
lgRenderOid = pack . show

instance Show OidPtr where
    show OidPtr {..} = SU.unsafePerformIO $
        withForeignPtr getOid (`oidToStr` getOidLen)

instance Ord OidPtr where
    (getOid -> coid1) `compare` (getOid -> coid2) =
        SU.unsafePerformIO $
        withForeignPtr coid1 $ \coid1Ptr ->
        withForeignPtr coid2 $ fmap (`compare` 0) . c'git_oid_cmp coid1Ptr

instance Eq OidPtr where
    oid1 == oid2 = oid1 `compare` oid2 == EQ

instance (Applicative m, MonadExcept m,
          MonadBaseControl IO m, MonadIO m)
         => Git.MonadGit LgRepo (ReaderT LgRepo m) where
    type Oid LgRepo = OidPtr
    data Tree LgRepo =
        LgTree { lgTreePtr :: Maybe (ForeignPtr C'git_tree) }
    data Options LgRepo = Options

    facts = return Git.RepositoryFacts { Git.hasSymbolicReferences = True }

    getRepository    = ask
    closeRepository  = return ()
    deleteRepository =
        Git.getRepository >>= liftIO . removeDirectoryRecursive . lgRepoPath

    parseOid = lgParseOid

    lookupReference   = lgLookupRef
    createReference   = lgUpdateRef
    updateReference   = lgUpdateRef
    deleteReference   = lgDeleteRef
    sourceReferences  = lgSourceRefs
    lookupCommit      = lgLookupCommit
    lookupTree        = lgLookupTree
    lookupBlob        = lgLookupBlob
    lookupTag         = error "Not implemented: LgRepository.lookupTag"
    readIndex         = lgReadIndex
    writeIndex        = error "Not implemented: LgRepository.writeIndex"
    lookupObject      = lgLookupObject
    existsObject      = lgExistsObject
    sourceObjects     = lgSourceObjects
    newTreeBuilder    = lgNewTreeBuilder
    treeEntry         = lgTreeEntry
    treeOid           = lgTreeOid
    sourceTreeEntries = lgSourceTreeEntries
    hashContents      = lgHashContents
    createBlob        = lgWrap . lgCreateBlob
    createTag         = error "Not implemented: LgRepository.createTag"

    createCommit p t a c l r = lgWrap $ lgCreateCommit p t a c l r

    diffContentsWithTree = error "Not implemented: lgDiffContentsWithTree"

    -- buildPackFile   = lgBuildPackFile
    -- buildPackIndex  = lgBuildPackIndexWrapper
    -- writePackFile   = lgWrap . lgWritePackFile

    -- remoteFetch     = lgRemoteFetch

lgExcTrap :: MonadLg m => ReaderT LgRepo m (IORef (Maybe Git.GitException))
lgExcTrap = repoExcTrap `liftM` Git.getRepository

lgWrap :: MonadLg m => ReaderT LgRepo m a -> ReaderT LgRepo m a
lgWrap f = f `catch` \e -> do
    etrap <- lgExcTrap
    mexc  <- liftIO $ readIORef etrap
    liftIO $ writeIORef etrap Nothing
    maybe (throwM (e :: SomeException)) throwM mexc

lgHashContents :: MonadLg m
               => Git.BlobContents (ReaderT LgRepo m)
               -> ReaderT LgRepo m BlobOid
lgHashContents b = do
    ptr <- liftIO mallocForeignPtr
    r   <- Git.blobContentsToByteString b >>= \bs ->
        liftIO $ withForeignPtr ptr $ \oidPtr ->
            BU.unsafeUseAsCStringLen bs $ uncurry $ \cstr len ->
                c'git_odb_hash oidPtr (castPtr cstr) (fromIntegral len)
                    c'GIT_OBJ_BLOB
    when (r < 0) $ lgThrow Git.BlobCreateFailed
    return $ Tagged (mkOid ptr)

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the given string is 'empty', it is an error.
lgCreateBlob :: MonadLg m
             => Git.BlobContents (ReaderT LgRepo m)
             -> ReaderT LgRepo m BlobOid
lgCreateBlob b = do
    repo <- Git.getRepository
    ptr  <- liftIO mallocForeignPtr -- freed automatically if GC'd
    r <- case b of
        Git.BlobString bs         -> createBlob repo ptr bs
        Git.BlobStringLazy bs     ->
            createBlob repo ptr (B.concat (BL.toChunks bs))
        Git.BlobStream src        -> readFromSource repo ptr src
        Git.BlobSizedStream src _ -> readFromSource repo ptr src
    when (r < 0) $ lgThrow Git.BlobCreateFailed
    return $ Tagged (mkOid ptr)

  where
    createBlob repo ptr bs = liftIO $ withForeignPtr ptr $ \coid' ->
        withForeignPtr (repoObj repo) $ \repoPtr ->
        BU.unsafeUseAsCStringLen bs $ uncurry $ \cstr len ->
            c'git_blob_create_frombuffer coid' repoPtr
                (castPtr cstr) (fromIntegral len)

    readFromSource repo ptr src =
        src $$ drainTo 2 $ \queue ->
            liftIO $ withForeignPtr ptr $ \coid' ->
            withForeignPtr (repoObj repo) $ \repoPtr ->
            bracket
                (mk'git_blob_chunk_cb (chunk_cb queue))
                freeHaskellFunPtr
                (\cb -> c'git_blob_create_fromchunks coid' repoPtr
                            nullPtr cb nullPtr)

    chunk_cb :: TBQueue (Maybe ByteString)
             -> CString          -- ^ content
             -> CSize            -- ^ max_length
             -> Ptr ()           -- ^ payload
             -> IO CInt
    chunk_cb queue content (fromIntegral -> maxLength) _payload = do
        (bs, len) <- atomically $ do
            mval <- readTBQueue queue
            let len = case mval of Nothing -> 0; Just val -> B.length val
            case mval of
                Nothing -> return (B.empty, 0)
                Just val
                    | len <= maxLength -> return (val, len)
                    | otherwise -> do
                        let (b, b') = B.splitAt maxLength val
                        unGetTBQueue queue (Just b')
                        return (b, maxLength)
        BU.unsafeUseAsCString bs $ flip (copyBytes content) len
        return $ fromIntegral len

lgObjToBlob :: MonadLg m
            => BlobOid
            -> ForeignPtr C'git_blob
            -> ReaderT LgRepo m (Git.Blob LgRepo (ReaderT LgRepo m))
lgObjToBlob oid fptr = do
    bs <- liftIO $ withForeignPtr fptr $ \ptr -> do
        size <- c'git_blob_rawsize ptr
        buf  <- c'git_blob_rawcontent ptr
        B.packCStringLen (castPtr buf, fromIntegral size)
    return $ Git.Blob oid $ Git.BlobString bs

lgLookupBlob :: MonadLg m
             => BlobOid
             -> ReaderT LgRepo m (Git.Blob LgRepo (ReaderT LgRepo m))
lgLookupBlob oid =
    lookupObject' (getOid (untag oid)) (getOidLen (untag oid))
        c'git_blob_lookup c'git_blob_lookup_prefix
        $ \boid obj _ -> lgObjToBlob (Tagged (mkOid boid)) obj

lgBlobRawSize :: MonadLg m => BlobOid -> ReaderT LgRepo m Int
lgBlobRawSize oid = do
  lookupObject' (getOid (untag oid)) (getOidLen (untag oid))
      c'git_blob_lookup c'git_blob_lookup_prefix
      $ \boid objectPtr _ ->
          liftIO . withForeignPtr objectPtr $ \ptr ->
            fromIntegral <$> c'git_blob_rawsize ptr

lgTreeEntry :: MonadLg m
            => Tree -> Git.TreeFilePath -> ReaderT LgRepo m (Maybe TreeEntry)
lgTreeEntry (LgTree Nothing) _ = return Nothing
lgTreeEntry (LgTree (Just tree)) fp = liftIO $ alloca $ \entryPtr ->
    withFilePath fp $ \pathStr ->
        withForeignPtr tree $ \treePtr -> do
            r <- c'git_tree_entry_bypath entryPtr treePtr pathStr
            if r < 0
                then return Nothing
                else Just <$> (entryToTreeEntry =<< peek entryPtr)

lgTreeOid :: MonadLg m => Tree -> ReaderT LgRepo m TreeOid
lgTreeOid (LgTree Nothing) =
    liftIO $ Tagged . fromJust <$> lgParseOidIO Git.emptyTreeId 40
lgTreeOid (LgTree (Just tree)) = liftIO $ do
    toid  <- withForeignPtr tree c'git_tree_id
    ftoid <- coidPtrToOid toid
    return $ Tagged (mkOid ftoid)

gatherFrom' :: (MonadIO m, MonadBaseControl IO m, MonadExcept m)
           => Int                -- ^ Size of the queue to create
           -> (TBQueue o -> m ()) -- ^ Action that generates output values
           -> Producer m o
gatherFrom' size scatter = do
    chan   <- liftIO $ newTBQueueIO size
    worker <- lift $ async (scatter chan)
    lift . restoreM =<< gather worker chan
  where
    gather worker chan = do
        (xs, mres) <- liftIO $ atomically $ do
            xs <- whileM (not <$> isEmptyTBQueue chan) (readTBQueue chan)
            (xs,) <$> pollSTM worker
        liftIO $ threadDelay 1
        mapM_ yield xs
        case mres of
            Just (Left e)  -> throwM (e :: SomeException)
            Just (Right r) -> return r
            Nothing        -> gather worker chan

lgSourceTreeEntries
    :: MonadLg m
    => Tree
    -> Producer (ReaderT LgRepo m) (Git.TreeFilePath, TreeEntry)
lgSourceTreeEntries (LgTree Nothing) = return ()
lgSourceTreeEntries (LgTree (Just tree)) = gatherFrom' 16 $ \queue -> do
    liftIO $ withForeignPtr tree $ \tr -> do
        r <- bracket
                (mk'git_treewalk_cb (callback queue))
                freeHaskellFunPtr
                (\callback ->
                  c'git_tree_walk tr c'GIT_TREEWALK_PRE callback nullPtr)
        when (r < 0) $ lgThrow Git.TreeWalkFailed
  where
    callback queue root te _payload = do
        fp    <- peekFilePath root
        cname <- c'git_tree_entry_name te
        name  <- (fp <>) <$> peekFilePath cname
        entry <- entryToTreeEntry te
        atomically $
            writeTBQueue queue $ name `seq` entry `seq` (name,entry)
        return 0

lgMakeBuilder :: MonadLg m
              => ForeignPtr C'git_treebuilder -> TreeBuilder (ReaderT LgRepo m)
lgMakeBuilder builder = Git.TreeBuilder
    { Git.mtbBaseTreeOid    = Nothing
    , Git.mtbPendingUpdates = mempty
    , Git.mtbNewBuilder     = lgNewTreeBuilder
    , Git.mtbWriteContents  = \tb -> (,) <$> pure (Git.BuilderUnchanged tb)
                                         <*> lgWriteBuilder builder
    , Git.mtbLookupEntry    = lgLookupBuilderEntry builder
    , Git.mtbEntryCount     = lgBuilderEntryCount builder
    , Git.mtbPutEntry       = \tb name ent ->
        Git.BuilderUnchanged tb <$ lgPutEntry builder name ent
    , Git.mtbDropEntry      = \tb name ->
        Git.BuilderUnchanged tb <$ lgDropEntry builder name
    }

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
lgNewTreeBuilder :: MonadLg m
                 => Maybe Tree
                 -> ReaderT LgRepo m (TreeBuilder (ReaderT LgRepo m))
lgNewTreeBuilder mtree = do
    mfptr <- liftIO $ alloca $ \pptr -> do
        r <- case mtree of
            Nothing -> c'git_treebuilder_create pptr nullPtr
            Just (LgTree Nothing) ->
                c'git_treebuilder_create pptr nullPtr
            Just (LgTree (Just tree)) ->
                withForeignPtr tree $ \treePtr ->
                    c'git_treebuilder_create pptr treePtr
        if r < 0
            then return Nothing
            else do
                builder <- peek pptr
                fptr <- FC.newForeignPtr builder
                            (c'git_treebuilder_free builder)
                return $ Just fptr
    case mfptr of
        Nothing ->
            throwM (Git.TreeCreateFailed "Failed to create new tree builder")
        Just fptr -> do
            toid <- mapM Git.treeOid mtree
            return (lgMakeBuilder fptr) { Git.mtbBaseTreeOid = toid }

lgPutEntry :: MonadLg m
           => ForeignPtr C'git_treebuilder -> Git.TreeFilePath -> TreeEntry
           -> ReaderT LgRepo m ()
lgPutEntry builder key (treeEntryToOid -> (oid, mode)) = do
    r2 <- liftIO $ withForeignPtr (getOid oid) $ \coid ->
        withForeignPtr builder $ \ptr ->
        withFilePath key $ \name ->
            c'git_treebuilder_insert nullPtr ptr name coid
                (fromIntegral mode)
    when (r2 < 0) $ throwM (Git.TreeBuilderInsertFailed key)

treeEntryToOid :: TreeEntry -> (Oid, Word32)
treeEntryToOid (Git.BlobEntry oid kind) =
    (untag oid, case kind of
          (Git.PlainBlob mode)      -> mode
          (Git.ExecutableBlob mode) -> mode
          (Git.SymlinkBlob mode)    -> mode)
treeEntryToOid (Git.CommitEntry coid) =
    (untag coid, 0o160000)
treeEntryToOid (Git.TreeEntry toid) =
    (untag toid, 0o040000)

lgDropEntry :: MonadLg m
            => ForeignPtr C'git_treebuilder -> Git.TreeFilePath
            -> ReaderT LgRepo m ()
lgDropEntry builder key =
    void $ liftIO $ withForeignPtr builder $ \ptr ->
        withFilePath key $ c'git_treebuilder_remove ptr

lgLookupBuilderEntry :: MonadLg m
                     => ForeignPtr C'git_treebuilder
                     -> Git.TreeFilePath
                     -> ReaderT LgRepo m (Maybe TreeEntry)
lgLookupBuilderEntry builderPtr name = do
    entry <- liftIO $ withForeignPtr builderPtr $ \builder ->
        withFilePath name $ c'git_treebuilder_get builder
    if entry == nullPtr
        then return Nothing
        else Just <$> liftIO (entryToTreeEntry entry)

lgBuilderEntryCount :: MonadLg m
                    => ForeignPtr C'git_treebuilder -> ReaderT LgRepo m Int
lgBuilderEntryCount tb =
    fromIntegral <$> liftIO (withForeignPtr tb c'git_treebuilder_entrycount)

lgTreeEntryCount :: MonadLg m => Tree -> ReaderT LgRepo m Int
lgTreeEntryCount (LgTree Nothing) = return 0
lgTreeEntryCount (LgTree (Just tree)) =
    fromIntegral <$> liftIO (withForeignPtr tree c'git_tree_entrycount)

lgWriteBuilder :: MonadLg m
               => ForeignPtr C'git_treebuilder -> ReaderT LgRepo m TreeOid
lgWriteBuilder tb = do
    repo <- Git.getRepository
    (r3,coid) <- liftIO $ do
        coid <- mallocForeignPtr
        withForeignPtr coid $ \coid' ->
            withForeignPtr tb $ \builder ->
            withForeignPtr (repoObj repo) $ \repoPtr -> do
                r3 <- c'git_treebuilder_write coid' repoPtr builder
                return (r3,coid)
    when (r3 < 0) $ lgThrow Git.TreeBuilderWriteFailed
    return $ Tagged (mkOid coid)

lgCloneBuilder :: MonadLg m
               => ForeignPtr C'git_treebuilder
               -> ReaderT LgRepo m (ForeignPtr C'git_treebuilder)
lgCloneBuilder fptr =
    liftIO $ withForeignPtr fptr $ \builder -> alloca $ \pptr -> do
        r <- c'git_treebuilder_create pptr nullPtr
        when (r < 0) $
            throwM (Git.BackendError "Could not create new treebuilder")
        builder' <- peek pptr
        bracket
            (mk'git_treebuilder_filter_cb (callback builder'))
            freeHaskellFunPtr
            (flip (c'git_treebuilder_filter builder) nullPtr)
        FC.newForeignPtr builder' (c'git_treebuilder_free builder')
  where
    callback builder te _ = do
        cname <- c'git_tree_entry_name te
        coid  <- c'git_tree_entry_id te
        fmode <- c'git_tree_entry_filemode te
        r <- c'git_treebuilder_insert
            nullPtr
            builder
            cname
            coid
            fmode
        when (r < 0) $
            throwM (Git.BackendError "Could not insert entry in treebuilder")
        return 0

lgLookupTree :: MonadLg m => TreeOid -> ReaderT LgRepo m Tree
lgLookupTree (untag -> oid)
    | show oid == unpack Git.emptyTreeId = return $ LgTree Nothing
    | otherwise = do
        fptr <- lookupObject' (getOid oid) (getOidLen oid)
            c'git_tree_lookup c'git_tree_lookup_prefix $
                \_ obj _ -> return obj
        return $ LgTree (Just fptr)

entryToTreeEntry :: Ptr C'git_tree_entry -> IO TreeEntry
entryToTreeEntry entry = do
    coid <- c'git_tree_entry_id entry
    oid  <- coidPtrToOid coid
    typ  <- c'git_tree_entry_type entry
    case () of
        () | typ == c'GIT_OBJ_BLOB ->
             do mode <- c'git_tree_entry_filemode entry
                Git.BlobEntry (Tagged (mkOid oid)) <$>
                    case mode of
                        (CUInt 0o100644) -> return $ Git.PlainBlob 0o100644
                        (CUInt 0o100755) -> return $ Git.ExecutableBlob 0o100755
                        (CUInt 0o120000) -> return $ Git.SymlinkBlob 0o120000
                        (CUInt mode') -> return $ Git.PlainBlob mode'
           | typ == c'GIT_OBJ_TREE ->
             return $ Git.TreeEntry (Tagged (mkOid oid))
           | typ == c'GIT_OBJ_COMMIT ->
             return $ Git.CommitEntry (Tagged (mkOid oid))
           | otherwise -> error "Unexpected"

lgObjToCommit :: CommitOid -> Ptr C'git_commit -> IO Commit
lgObjToCommit oid c = do
    enc    <- c'git_commit_message_encoding c
    encs   <- if enc == nullPtr
              then return "UTF-8"
              else peekCString enc
    conv   <- U.open encs (Just False)

    msg    <- c'git_commit_message c   >>= B.packCString
    auth   <- c'git_commit_author c    >>= packSignature conv
    comm   <- c'git_commit_committer c >>= packSignature conv
    toid   <- c'git_commit_tree_id c
    toid'  <- coidPtrToOid toid

    pn     <- c'git_commit_parentcount c
    poids  <- zipWithM ($)
                 (replicate (fromIntegral (toInteger pn))
                  (c'git_commit_parent_id c))
                 [0..pn]
    poids' <- mapM (\x -> Tagged . mkOid <$> coidPtrToOid x) poids

    return Git.Commit
        {
        -- Git.commitInfo      = Base (Just (Tagged (Oid coid))) (Just obj)
        -- ,
          Git.commitOid       = oid
        , Git.commitTree      = Tagged (mkOid toid')
        , Git.commitParents   = poids'
        , Git.commitAuthor    = auth
        , Git.commitCommitter = comm
        , Git.commitLog       = U.toUnicode conv msg
        , Git.commitEncoding  = "utf-8"
        }

lgLookupCommit :: MonadLg m => CommitOid -> ReaderT LgRepo m Commit
lgLookupCommit oid =
  lookupObject' (getOid (untag oid)) (getOidLen (untag oid))
      c'git_commit_lookup c'git_commit_lookup_prefix
      $ \coid obj _ -> liftIO $ withForeignPtr obj $
          lgObjToCommit (Tagged (mkOid coid))

lgReadIndex :: MonadLg m => Git.TreeT LgRepo (ReaderT LgRepo m) ()
lgReadIndex = do
  repo <- lift Git.getRepository
  xs <- liftIO $ withForeignPtr (repoObj repo) $ \repoPtr ->
    alloca $ \indexPp -> do
      r <- c'git_repository_index indexPp repoPtr
      idx <- if r < 0
             then lgThrow Git.BackendError
             else peek indexPp
      cnt <- c'git_index_entrycount idx
      fmap Prelude.concat $ forM [0..pred cnt] $ \i -> do
        entryPtr <- c'git_index_get_byindex idx i
        entry <- peek entryPtr
        let oid   = c'git_index_entry'oid entry
            mode  = c'git_index_entry'mode entry
            path  = c'git_index_entry'path entry
            flags = c'git_index_entry'flags entry
        if 0 /= flags .&. 2 -- c'GIT_IDXENTRY_REMOVE
            then return []
            else do
                oid'  <- new oid
                foid' <- newForeignPtr_ oid'
                path' <- peekFilePath path
                return
                    [(path',
                      if 0 /= mode .&. 16384 -- check if directory
                      then Git.TreeEntry (Tagged (mkOid foid'))
                      else Git.BlobEntry (Tagged (mkOid foid')) $
                           let (CUInt mode') = mode in
                            if (0 /= mode .&. 64 -- check if owner executable
                                )
                            then (Git.ExecutableBlob mode')
                            else (Git.PlainBlob mode')
                                  -- jww (2014-04-05): Handle CommitEntry
                     )]
  forM_ xs $ uncurry Git.putEntry

data ObjectPtr = BlobPtr (ForeignPtr C'git_blob)
               | TreePtr (ForeignPtr C'git_commit)
               | CommitPtr (ForeignPtr C'git_commit)
               | TagPtr (ForeignPtr C'git_tag)

lgLookupObject :: MonadLg m
               => Oid
               -> ReaderT LgRepo m (Git.Object LgRepo (ReaderT LgRepo m))
lgLookupObject oid = do
    repo <- Git.getRepository
    (oid', typ, fptr) <-
        lookupObject' (getOid oid) (getOidLen oid)
            (\x y z   -> c'git_object_lookup x y z c'GIT_OBJ_ANY)
            (\x y z l -> c'git_object_lookup_prefix x y z l c'GIT_OBJ_ANY)
            $ \coid fptr y -> do
                typ <- liftIO $ c'git_object_type y
                return (mkOid coid, typ, fptr)
    case () of
        () | typ == c'GIT_OBJ_BLOB   ->
                Git.BlobObj <$> lgObjToBlob (Tagged oid') (castForeignPtr fptr)
           | typ == c'GIT_OBJ_TREE   ->
                -- A ForeignPtr C'git_object is bit-wise equivalent to a
                -- ForeignPtr C'git_tree.
                return $ Git.TreeObj (LgTree (Just (unsafeCoerce fptr)))
           | typ == c'GIT_OBJ_COMMIT ->
                Git.CommitObj <$>
                liftIO (withForeignPtr fptr $ \y ->
                         lgObjToCommit (Tagged oid') (castPtr y))
           | typ == c'GIT_OBJ_TAG -> do
                commit <- liftIO (withForeignPtr (repoObj repo) $ \repoPtr ->
                     liftIO (withForeignPtr fptr $ \y ->
                           do oid'' <- c'git_tag_target_id (castPtr y)
                              alloca $ \pptr -> do
                                  _ <- c'git_commit_lookup pptr repoPtr oid''
                                  ptr <- peek pptr
                                  coid <- coidPtrToOid oid''
                                  lgObjToCommit (Tagged . mkOid $ coid) ptr
                        ))
                Git.CommitObj <$> (pure commit)
           | otherwise -> error $ "Unknown object type: " ++ show typ

lgExistsObject :: MonadLg m => Oid -> ReaderT LgRepo m Bool
lgExistsObject oid = do
    repo <- Git.getRepository
    result <- liftIO $ withForeignPtr (repoObj repo) $ \repoPtr ->
        alloca $ \pptr -> do
            r <- c'git_repository_odb pptr repoPtr
            if r < 0
                then return Nothing
                else
                -- jww (2013-02-28): Need to guard against exceptions so that
                -- ptr doesn't leak.
                withForeignPtr (getOid oid) $ \coid -> do
                    ptr <- peek pptr
                    r1 <- c'git_odb_exists ptr coid 0
                    c'git_odb_free ptr
                    return (Just (r1 == 0))
    maybe (throwM Git.RepositoryInvalid) return result

lgForEachObject :: Ptr C'git_odb
                -> (Ptr C'git_oid -> Ptr () -> IO CInt)
                -> Ptr ()
                -> IO CInt
lgForEachObject odbPtr f payload =
    bracket
        (mk'git_odb_foreach_cb f)
        freeHaskellFunPtr
        (flip (c'git_odb_foreach odbPtr) payload)

lgSourceObjects :: MonadLg m
                => Maybe CommitOid -> CommitOid -> Bool
                -> Producer (ReaderT LgRepo m) ObjectOid
lgSourceObjects mhave need alsoTrees = do
    repo   <- lift Git.getRepository
    walker <- liftIO $ alloca $ \pptr -> do
        r <- withForeignPtr (repoObj repo) $ \repoPtr ->
                c'git_revwalk_new pptr repoPtr
        when (r < 0) $
            throwM (Git.BackendError "Could not create revwalker")
        ptr <- peek pptr
        FC.newForeignPtr ptr (c'git_revwalk_free ptr)

    c <- lift $ lgLookupCommit need
    let oid = untag (Git.commitOid c)

    liftIO $ withForeignPtr (getOid oid) $ \coid -> do
        r2 <- withForeignPtr walker $ flip c'git_revwalk_push coid
        when (r2 < 0) $
            throwM (Git.BackendError $ "Could not push oid "
                         <> pack (show oid) <> " onto revwalker")

    case mhave of
        Nothing   -> return ()
        Just have -> liftIO $ withForeignPtr (getOid (untag have)) $ \coid -> do
            r2 <- withForeignPtr walker $ flip c'git_revwalk_hide coid
            when (r2 < 0) $
                throwM (Git.BackendError $ "Could not hide commit "
                             <> pack (show (untag have)) <> " from revwalker")

    liftIO $ withForeignPtr walker $ flip c'git_revwalk_sorting
        (fromIntegral ((1 :: Int) .|. (4 :: Int)))

    coidPtr <- liftIO mallocForeignPtr
    whileM_ ((==) <$> pure 0
                  <*> liftIO (withForeignPtr walker $ \walker' ->
                              withForeignPtr coidPtr $ \coidPtr' ->
                                  c'git_revwalk_next coidPtr' walker')) $ do
        oidPtr <- liftIO $ withForeignPtr coidPtr coidPtrToOid
        do
            let coid = Tagged (mkOid oidPtr)
            yield $ Git.CommitObjOid coid
            when alsoTrees $ do
                c <- lift $ lgLookupCommit coid
                yield $ Git.TreeObjOid (Git.commitTree c)

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
lgCreateCommit :: MonadLg m
               => [CommitOid]
               -> TreeOid
               -> Git.Signature
               -> Git.Signature
               -> Git.CommitMessage
               -> Maybe Git.RefName
               -> ReaderT LgRepo m Commit
lgCreateCommit pptrs tree author committer logText ref = do
    repo <- Git.getRepository
    let toid  = getOid . untag $ tree
    coid <- liftIO $ withForeignPtr (repoObj repo) $ \repoPtr -> do
        coid <- mallocForeignPtr
        conv <- U.open "utf-8" (Just True)
        withForeignPtr coid $ \coid' ->
            withForeignPtr toid $ \toid' ->
            withForeignPtrs (map (getOid . untag) pptrs) $ \pptrs' ->
            B.useAsCString (U.fromUnicode conv logText) $ \message ->
            withRef ref $ \update_ref ->
            withSignature conv author $ \author' ->
            withSignature conv committer $ \committer' ->
            withEncStr "utf-8" $ \_ {-message_encoding-} -> do
                parents' <- newArray pptrs'
                r <- c'git_commit_create_oid coid' repoPtr
                     update_ref author' committer'
                     nullPtr message toid'
                     (fromIntegral (L.length pptrs)) parents'
                when (r < 0) $ throwM Git.CommitCreateFailed
                return coid

    return Git.Commit
        {
        --   Git.commitInfo      = Base (Just (Tagged (Oid coid))) Nothing
        -- ,
          Git.commitOid       = Tagged (mkOid coid)
        , Git.commitTree      = tree
        , Git.commitParents   = pptrs
        , Git.commitAuthor    = author
        , Git.commitCommitter = committer
        , Git.commitLog       = logText
        , Git.commitEncoding  = "utf-8"
        }

  where
    withRef Nothing     = flip ($) nullPtr
    withRef (Just name) = B.useAsCString (T.encodeUtf8 name)

    withEncStr ""  = flip ($) nullPtr
    withEncStr enc = withCString enc

withForeignPtrs :: [ForeignPtr a] -> ([Ptr a] -> IO b) -> IO b
withForeignPtrs fos io = do
    r <- io (map FU.unsafeForeignPtrToPtr fos)
    mapM_ touchForeignPtr fos
    return r

lgLookupRef :: MonadLg m => Git.RefName -> ReaderT LgRepo m (Maybe RefTarget)
lgLookupRef name = do
    repo <- Git.getRepository
    liftIO $ alloca $ \ptr -> do
        r <- withForeignPtr (repoObj repo) $ \repoPtr ->
              withCString (unpack name) $ \namePtr ->
                c'git_reference_lookup ptr repoPtr namePtr
        if r < 0
            then return Nothing
            else do
            ref  <- peek ptr
            typ  <- c'git_reference_type ref
            targ <- if typ == c'GIT_REF_OID
                    then do oidPtr <- c'git_reference_target ref
                            Git.RefObj . mkOid
                                <$> coidPtrToOid oidPtr
                    else do targName <- c'git_reference_symbolic_target ref
                            Git.RefSymbolic . T.decodeUtf8
                                <$> B.packCString targName
            c'git_reference_free ref
            return (Just targ)

lgUpdateRef :: MonadLg m
            => Git.RefName -> Git.RefTarget LgRepo -> ReaderT LgRepo m ()
lgUpdateRef name refTarg = do
    repo <- Git.getRepository
    r <- liftIO $ alloca $ \ptr ->
        withForeignPtr (repoObj repo) $ \repoPtr ->
        withCString (unpack name) $ \namePtr ->
            case refTarg of
                Git.RefObj oid ->
                    withForeignPtr (getOid oid) $ \coidPtr ->
                        c'git_reference_create ptr repoPtr namePtr
                                               coidPtr (fromBool True)

                Git.RefSymbolic symName ->
                    withCString (unpack symName) $ \symPtr ->
                        c'git_reference_symbolic_create ptr repoPtr namePtr
                                                        symPtr (fromBool True)
    when (r < 0) $ lgThrow Git.ReferenceCreateFailed

-- int git_reference_name_to_oid(git_oid *out, git_repository *repo,
--   const char *name)

lgResolveRef :: MonadLg m => Git.RefName -> ReaderT LgRepo m (Maybe CommitOid)
lgResolveRef name = do
    repo <- Git.getRepository
    oid <- liftIO $ alloca $ \ptr ->
        withCString (unpack name) $ \namePtr ->
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            r <- c'git_reference_name_to_id ptr repoPtr namePtr
            if r < 0
                then return Nothing
                else Just <$> coidPtrToOid ptr
    return $ Tagged . mkOid <$> oid

-- int git_reference_rename(git_reference *ref, const char *new_name,
--   int force)

--renameRef = c'git_reference_rename

lgDeleteRef :: MonadLg m => Git.RefName -> ReaderT LgRepo m ()
lgDeleteRef name = do
    repo <- Git.getRepository
    r <- liftIO $ alloca $ \ptr ->
        withCString (unpack name) $ \namePtr ->
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            r <- c'git_reference_lookup ptr repoPtr namePtr
            if r < 0
                then return r
                else do
                ref <- peek ptr
                c'git_reference_delete ref
    when (r < 0) $ throwM (Git.ReferenceDeleteFailed name)

-- int git_reference_packall(git_repository *repo)

--packallRefs = c'git_reference_packall

data ListFlags = ListFlags { listFlagInvalid  :: Bool
                           , listFlagOid      :: Bool
                           , listFlagSymbolic :: Bool
                           , listFlagPacked   :: Bool
                           , listFlagHasPeel  :: Bool }
               deriving (Show, Eq)

allRefsFlag :: ListFlags
allRefsFlag = ListFlags { listFlagInvalid  = False
                        , listFlagOid      = True
                        , listFlagSymbolic = True
                        , listFlagPacked   = True
                        , listFlagHasPeel  = False }

-- symbolicRefsFlag :: ListFlags
-- symbolicRefsFlag = ListFlags { listFlagInvalid  = False
--                              , listFlagOid      = False
--                              , listFlagSymbolic = True
--                              , listFlagPacked   = False
--                              , listFlagHasPeel  = False }

-- oidRefsFlag :: ListFlags
-- oidRefsFlag = ListFlags { listFlagInvalid  = False
--                         , listFlagOid      = True
--                         , listFlagSymbolic = False
--                         , listFlagPacked   = True
--                         , listFlagHasPeel  = False }

-- looseOidRefsFlag :: ListFlags
-- looseOidRefsFlag = ListFlags { listFlagInvalid  = False
--                              , listFlagOid      = True
--                              , listFlagSymbolic = False
--                              , listFlagPacked   = False
--                              , listFlagHasPeel  = False }

gitStrArray2List :: Ptr C'git_strarray -> IO [Text]
gitStrArray2List gitStrs = do
  count <- fromIntegral <$> peek (p'git_strarray'count gitStrs)
  strings <- peek $ p'git_strarray'strings gitStrs

  r0 <- Foreign.Marshal.Array.peekArray count strings
  r1 <- sequence $ fmap peekCString r0
  return $ fmap pack r1

flagsToInt :: ListFlags -> CUInt
flagsToInt flags = (if listFlagOid flags      then 1 else 0)
                 + (if listFlagSymbolic flags then 2 else 0)
                 + (if listFlagPacked flags   then 4 else 0)
                 + (if listFlagHasPeel flags  then 8 else 0)

lgSourceRefs :: MonadLg m => Producer (ReaderT LgRepo m) Git.RefName
lgSourceRefs =
    gatherFrom' 16 $ \queue -> do
        repo <- Git.getRepository
        r <- liftIO $ bracket
            (mk'git_reference_foreach_cb (callback queue))
            freeHaskellFunPtr
            (\callback -> withForeignPtr (repoObj repo) $ \repoPtr ->
                  c'git_reference_foreach repoPtr
                      (flagsToInt allRefsFlag) callback nullPtr)
        when (r < 0) $ lgThrow Git.ReferenceListingFailed
  where
    callback :: TBQueue Text
             -> CString
             -> Ptr ()
             -> IO CInt
    callback queue cname _payload = do
        name <- peekCString cname
        atomically $ writeTBQueue queue (pack name)
        return 0

-- foreachRefCallback :: CString -> Ptr () -> IO CInt
-- foreachRefCallback name payload = do
--   (callback,results) <- deRefStablePtr =<< peek (castPtr payload)
--   nameStr <- peekCString name
--   result <- callback (pack nameStr)
--   modifyIORef results (\xs -> result:xs)
--   return 0

-- foreign export ccall "foreachRefCallback"
--   foreachRefCallback :: CString -> Ptr () -> IO CInt
-- foreign import ccall "&foreachRefCallback"
--   foreachRefCallbackPtr :: FunPtr (CString -> Ptr () -> IO CInt)

-- lgMapRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- lgMapRefs cb = do
--     repo <- Git.getRepository
--     liftIO $ do
--         withForeignPtr (repoObj repo) $ \repoPtr -> do
--             ioRef <- newIORef []
--             bracket
--                 (newStablePtr (cb,ioRef))
--                 freeStablePtr
--                 (\ptr -> with ptr $ \pptr -> do
--                       _ <- c'git_reference_foreach
--                            repoPtr (flagsToInt allRefsFlag)
--                            foreachRefCallbackPtr (castPtr pptr)
--                       readIORef ioRef)

-- mapAllRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapAllRefs repo = mapRefs repo allRefsFlag
-- mapOidRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapOidRefs repo = mapRefs repo oidRefsFlag
-- mapLooseOidRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapLooseOidRefs repo = mapRefs repo looseOidRefsFlag
-- mapSymbolicRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapSymbolicRefs repo = mapRefs repo symbolicRefsFlag

-- int git_reference_is_packed(git_reference *ref)

--refIsPacked = c'git_reference_is_packed

-- int git_reference_reload(git_reference *ref)

--reloadRef = c'git_reference_reload

-- int git_reference_cmp(git_reference *ref1, git_reference *ref2)

--compareRef = c'git_reference_cmp

lgThrow :: (MonadIO m, MonadExcept m, Exception e) => (Text -> e) -> m a
lgThrow f = do
    errStr <- liftIO $ do
        errPtr <- c'giterr_last
        if errPtr == nullPtr
            then return ""
            else do
                err <- peek errPtr
                peekCString (c'git_error'message err)
    throwM (f (pack errStr))

-- withLgTempRepo :: MonadLg m => ReaderT LgRepo m a -> m a
-- withLgTempRepo f = withTempDir $ \dir -> do
--     repo <- liftIO (removeTree dir) >> openLgRepository
--         defaultRepositoryOptions
--             { repoPath       = F.encodeString dir
--             , repoIsBare     = True
--             , repoAutoCreate = True
--             }
--     runProjectLgRepository $ f `finally` closeRepository

lgDiffContentsWithTree
    :: MonadLg m
    => Source (ReaderT LgRepo m)
        (Either Git.TreeFilePath (Either Git.SHA ByteString))
    -> Tree
    -> Producer (ReaderT LgRepo m) ByteString
lgDiffContentsWithTree _contents (LgTree Nothing) =
    liftIO $ throwM $
        Git.DiffTreeToIndexFailed "Cannot diff against an empty tree"

lgDiffContentsWithTree contents tree = do
    repo <- lift Git.getRepository
    gatherFrom' 16 $ generateDiff repo
  where
    -- generateDiff :: MonadLg m => LgRepo -> TBQueue ByteString -> m ()
    generateDiff repo chan = do
        entries   <- M.fromList <$> Git.listTreeEntries tree
        paths     <- liftIO $ newIORef []
        (src, ()) <- contents $$+ return ()

        handleEntries entries paths src
        contentsPaths <- liftIO $ readIORef paths

        forM_ (sort (M.keys entries) \\ sort contentsPaths) $ \path ->
            -- File was removed
            case entries M.! path of
                Git.BlobEntry oid _   -> do
                    let boid = getOid (untag oid)
                    diffBlob path Nothing (Just boid)

                -- jww (2013-11-24): NYI
                Git.CommitEntry _coid -> return ()
                Git.TreeEntry _toid   -> return ()
      where
        -- handleEntries :: M.Map Git.TreeFilePath TreeEntry
        --               -> IORef [Git.TreeFilePath]
        --               -> ResumableSource m (Either Git.TreeFilePath
        --                                     (Either Git.SHA ByteString))
        --               -> m ()
        handleEntries entries paths src = do
            (src', mres) <- src $$++ do
                mpath <- await
                case mpath of
                    Nothing   -> return Nothing
                    Just path -> Just <$> handlePath path
            case mres of
                Nothing -> return ()
                Just (path, content) -> do
                    liftIO $ modifyIORef paths (path:)
                    case M.lookup path entries of
                        Nothing ->
                            -- File is newly added
                            diffBlob path (Just content) Nothing

                        Just entry -> case entry of
                            -- File has been changed
                            Git.BlobEntry oid _   -> do
                                let boid = getOid (untag oid)
                                diffBlob path (Just content) (Just boid)

                            -- jww (2013-11-24): NYI
                            Git.CommitEntry _coid -> return ()
                            Git.TreeEntry _toid   -> return ()
                    handleEntries entries paths src'

        -- handlePath :: Either Git.TreeFilePath (Either Git.SHA ByteString)
        --            -> Consumer (Either Git.TreeFilePath
        --                         (Either Git.SHA ByteString)) m
        --                   (Git.TreeFilePath, Either Git.SHA ByteString)
        handlePath (Right _) =
            lift $ throwM $ Git.DiffTreeToIndexFailed
                "Received a Right value when a Left RawFilePath was expected"
        handlePath (Left path) = do
            mcontent <- await
            case mcontent of
                Nothing ->
                    lift $ throwM $ Git.DiffTreeToIndexFailed $
                        "Content not provided for " <> T.pack (show path)
                Just x -> handleContent path x

        -- handleContent :: Git.TreeFilePath
        --               -> Either Git.TreeFilePath (Either Git.SHA ByteString)
        --               -> Consumer (Either Git.TreeFilePath
        --                            (Either Git.SHA ByteString)) m
        --                   (Git.TreeFilePath, Either Git.SHA ByteString)
        handleContent _path (Left _) =
            lift $ throwM $ Git.DiffTreeToIndexFailed
                "Received a Left value when a Right ByteString was expected"
        handleContent path (Right content) = return (path, content)

        -- diffBlob :: MonadExcept m
        --          => Git.TreeFilePath
        --          -> Maybe (Either Git.SHA ByteString)
        --          -> Maybe (ForeignPtr C'git_oid)
        --          -> m ()
        diffBlob path mcontent mboid = do
            r <- liftIO $ runResourceT $ do
                fileHeader <- liftIO $ newIORef Nothing

                let f = flip allocate freeHaskellFunPtr
                (_, fcb) <- f $ mk'git_diff_file_cb (file_cb fileHeader)
                (_, hcb) <- f $ mk'git_diff_hunk_cb (hunk_cb fileHeader)
                (_, pcb) <- f $ mk'git_diff_data_cb print_cb

                let db b o = diffBlobs fcb hcb pcb b o
                    dbb b  = diffBlobToBuffer fcb hcb pcb b
                case mboid of
                    Nothing   -> liftIO $ dbb nullPtr
                    Just boid -> withBlob boid $ \blobp ->
                        case mcontent of
                            Just (Left sha) -> do
                                boid2 <- liftIO $ shaToCOid sha
                                if boid == boid2
                                    then withBlob boid2 $
                                        liftIO . db blobp
                                    else return 0
                            _ -> liftIO $ dbb blobp
            when (r < 0) $ lgThrow Git.DiffBlobFailed
          where
            withBlob :: ForeignPtr C'git_oid
                     -> (Ptr C'git_blob -> ResourceT IO CInt)
                     -> ResourceT IO CInt
            withBlob boid f = do
                (_, eblobp) <- flip allocate freeBlob $
                    alloca $ \blobpp ->
                    withForeignPtr boid $ \boidPtr ->
                    withForeignPtr (repoObj repo) $ \repoPtr -> do
                        r <- c'git_blob_lookup blobpp repoPtr boidPtr
                        if r < 0
                            then return $ Left r
                            else Right <$> peek blobpp
                case eblobp of
                    Left r      -> return r
                    Right blobp -> f blobp
              where
                freeBlob (Left _)      = return ()
                freeBlob (Right blobp) = c'git_blob_free blobp

            -- diffBlobToBuffer :: fcb -> hcb -> pcb -> Ptr C'git_blob -> IO CInt
            diffBlobToBuffer fcb hcb pcb blobp = do
                let diff s l =
                        c'git_diff_blob_to_buffer blobp s (fromIntegral l)
                            nullPtr fcb hcb pcb nullPtr
                case mcontent of
                    Just (Right c) -> BU.unsafeUseAsCStringLen c $ uncurry diff
                    _              -> diff nullPtr 0

            -- diffBlobs :: fcb -> hcb -> pcb -> Ptr C'git_blob -> Ptr C'git_blob
            --           -> IO CInt
            diffBlobs fcb hcb pcb blobp otherp =
                c'git_diff_blobs blobp otherp nullPtr fcb hcb pcb nullPtr

            isBinary :: C'git_diff_delta -> Bool
            isBinary delta =
                c'git_diff_delta'flags delta .&. c'GIT_DIFF_FLAG_BINARY /= 0

            file_cb :: IORef (Maybe ByteString)
                    -> Ptr C'git_diff_delta
                    -> CFloat
                    -> Ptr ()
                    -> IO CInt
            file_cb fh deltap _progress _payload = do
                delta <- peek deltap
                writeIORef fh $ Just $
                    if isBinary delta
                    then "Binary files a/" <> path <> " and b/" <> path
                        <> " differ\n"
                    else "--- a/" <> path <> "\n" <> "+++ b/" <> path <> "\n"
                return 0

            hunk_cb :: IORef (Maybe ByteString)
                    -> Ptr C'git_diff_delta
                    -> Ptr C'git_diff_range
                    -> CString
                    -> CSize
                    -> Ptr ()
                    -> IO CInt
            hunk_cb fh deltap _rangep header headerLen _payload = do
                delta <- peek deltap
                mfh <- readIORef fh
                forM_ mfh $ \h -> do
                    atomically $ writeTBQueue chan h
                    writeIORef fh Nothing
                unless (isBinary delta) $ do
                    bs <- curry B.packCStringLen header
                        (fromIntegral headerLen)
                    atomically $ writeTBQueue chan bs
                return 0

            print_cb :: Ptr C'git_diff_delta
                     -> Ptr C'git_diff_range
                     -> CChar
                     -> CString
                     -> CSize
                     -> Ptr ()
                     -> IO CInt
            print_cb deltap _range lineOrigin content contentLen _payload = do
                delta <- peek deltap
                unless (isBinary delta) $ do
                    bs <- curry B.packCStringLen content
                        (fromIntegral contentLen)
                    atomically $ writeTBQueue chan $
                        B.cons (fromIntegral lineOrigin) bs
                return 0

checkResult :: (Eq a, Num a, MonadExcept m) => a -> Text -> m ()
checkResult r why = when (r /= 0) $ throwM (Git.BackendError why)

lgBuildPackFile :: MonadLg m
                => FilePath -> [Either CommitOid TreeOid]
                -> ReaderT LgRepo m FilePath
lgBuildPackFile dir oids = do
    repo <- Git.getRepository
    liftIO $ do
        (filePath, fHandle) <- openBinaryTempFile dir "pack"
        hClose fHandle
        go repo filePath
        return filePath
  where
    go repo path = runResourceT $ do
        delKey <- register $ removeFile path

        (_,bPtrPtr) <- allocate malloc free
        (_,bPtr)    <- flip allocate c'git_packbuilder_free $
            liftIO $ withForeignPtr (repoObj repo) $ \repoPtr -> do
                r <- c'git_packbuilder_new bPtrPtr repoPtr
                checkResult r "c'git_packbuilder_new failed"
                peek bPtrPtr

        forM_ oids $ \oid -> case oid of
            -- jww (2013-04-24): When libgit2 0.19 comes out, we will only
            -- need to call c'git_packbuilder_insert_commit here, as it will
            -- insert both the commit and its tree.
            Left coid ->
                actOnOid
                    (flip (c'git_packbuilder_insert bPtr) nullPtr)
                    (untag coid)
                    "c'git_packbuilder_insert failed"
            Right toid ->
                actOnOid
                    (c'git_packbuilder_insert_tree bPtr)
                    (untag toid)
                    "c'git_packbuilder_insert_tree failed"

        liftIO $ do
            r1 <- c'git_packbuilder_set_threads bPtr 0
            checkResult r1 "c'git_packbuilder_set_threads failed"

            withCString path $ \cstr -> do
                r2 <- c'git_packbuilder_write bPtr cstr
                checkResult r2 "c'git_packbuilder_write failed"

        void $ unprotect delKey

    actOnOid f oid msg =
        liftIO $ withForeignPtr (getOid oid) $ \oidPtr -> do
            r <- f oidPtr
            checkResult r msg

lift_ :: (Monad m, Functor (t m), MonadTrans t) => m a -> t m ()
lift_ = void . lift

lgBuildPackIndex :: (MonadIO m, MonadBaseControl IO m)
                 => FilePath -> BL.ByteString -> m (Text, FilePath, FilePath)
lgBuildPackIndex dir bytes = do
    sha <- go dir bytes
    return (sha, dir </> ("pack-" <> unpack sha <> ".pack"),
                 dir </> ("pack-" <> unpack sha <> ".idx"))
  where
    go dir bytes = control $ \run -> alloca $ \idxPtrPtr -> runResourceT $ do
        lift_ . run $ lgDebug "Allocate a new indexer stream"
        (_,idxPtr) <- flip allocate c'git_indexer_stream_free $
            withCString dir $ \dirStr -> do
                r <- c'git_indexer_stream_new idxPtrPtr dirStr
                         nullFunPtr nullPtr
                checkResult r "c'git_indexer_stream_new failed"
                peek idxPtrPtr

        lift_ . run $
            lgDebug $ "Add the incoming packfile data to the stream ("
                 ++ show (BL.length bytes) ++ " bytes)"
        (_,statsPtr) <- allocate calloc free
        liftIO $ forM_ (BL.toChunks bytes) $ \chunk ->
            BU.unsafeUseAsCStringLen chunk $ uncurry $ \dataPtr dataLen -> do
                r <- c'git_indexer_stream_add idxPtr (castPtr dataPtr)
                         (fromIntegral dataLen) statsPtr
                checkResult r "c'git_indexer_stream_add failed"

        lift_ . run $ lgDebug "Finalizing the stream"
        r <- liftIO $ c'git_indexer_stream_finalize idxPtr statsPtr
        checkResult r "c'git_indexer_stream_finalize failed"

        lift_ . run $
            lgDebug "Discovering the hash used to identify the pack file"
        sha <- liftIO $ oidToSha =<< c'git_indexer_stream_hash idxPtr
        lift_ . run $ lgDebug $ "The hash used is: " ++ show (Git.shaToText sha)

        lift . run $ return (Git.shaToText sha)

oidToSha :: Ptr C'git_oid -> IO Git.SHA
oidToSha oidPtr =
    Git.SHA <$> B.packCStringLen
        (castPtr oidPtr, sizeOf (undefined :: C'git_oid))

shaToCOid :: Git.SHA -> IO (ForeignPtr C'git_oid)
shaToCOid (Git.SHA bs) = BU.unsafeUseAsCString bs $ \bytes -> do
    ptr <- mallocForeignPtr
    withForeignPtr ptr $ \ptr' -> do
        c'git_oid_fromraw ptr' (castPtr bytes)
        return ptr

shaToOid :: Git.SHA -> IO OidPtr
shaToOid = fmap mkOid . shaToCOid

lgCopyPackFile :: MonadLg m => FilePath -> ReaderT LgRepo m ()
lgCopyPackFile packFile = do
    -- jww (2013-04-23): This would be much more efficient (we already have
    -- the pack file on disk, why not just copy it?), but we have no way at
    -- present of communicating with the S3 backend directly.
    -- S3.uploadPackAndIndex undefined (F.directory packFile) packSha

    -- Use the ODB backend interface to transfer the pack file, which
    -- inefficiently transfers the pack file as a strict ByteString in memory,
    -- only to be written to disk again on the other side.  However, since
    -- this algorithm knows nothing about S3 or the S3 backend, this is our
    -- only way of talking to that backend.
    --
    -- The abstract API does have a writePackFile method, but we can't use it
    -- yet because it only calls into the Libgit2 backend, which doesn't know
    -- anything about the S3 backend.  As far as Libgit2 is concerned, the S3
    -- backend is just a black box with no special properties.
    repo <- Git.getRepository
    control $ \run -> withForeignPtr (repoObj repo) $ \repoPtr ->
        alloca $ \odbPtrPtr ->
        alloca $ \statsPtr ->
        alloca $ \writepackPtrPtr -> do
            runResourceT $ go run repoPtr odbPtrPtr writepackPtrPtr statsPtr
            run $ return ()
  where
    go run repoPtr odbPtrPtr writepackPtrPtr statsPtr = do
        lift_ . run $ lgDebug "Obtaining odb for repository"
        (_,odbPtr) <- flip allocate c'git_odb_free $ do
            r <- c'git_repository_odb odbPtrPtr repoPtr
            checkResult r "c'git_repository_odb failed"
            peek odbPtrPtr

        lift_ . run $ lgDebug "Opening pack writer into odb"
        writepackPtr <- liftIO $ do
            r <- c'git_odb_write_pack writepackPtrPtr odbPtr
                nullFunPtr nullPtr
            checkResult r "c'git_odb_write_pack failed"
            peek writepackPtrPtr
        writepack <- liftIO $ peek writepackPtr

        bs <- liftIO $ B.readFile packFile
        lift_ . run $
            lgDebug $ "Writing pack file " ++ show packFile ++ " into odb"
        lift_ . run $
            lgDebug $ "Writing " ++ show (B.length bs) ++ " pack bytes into odb"
        liftIO $ BU.unsafeUseAsCStringLen bs $
            uncurry $ \dataPtr dataLen -> do
                r <- mK'git_odb_writepack_add_callback
                         (c'git_odb_writepack'add writepack)
                         writepackPtr (castPtr dataPtr)
                         (fromIntegral dataLen) statsPtr
                checkResult r "c'git_odb_writepack'add failed"

        lift_ . run $ lgDebug "Committing pack into odb"
        r <- liftIO $ mK'git_odb_writepack_commit_callback
                 (c'git_odb_writepack'commit writepack) writepackPtr
                 statsPtr
        checkResult r "c'git_odb_writepack'commit failed"

lgLoadPackFileInMemory
    :: (MonadBaseControl IO m, MonadIO m, MonadExcept m)
    => FilePath
    -> Ptr (Ptr C'git_odb_backend)
    -> Ptr (Ptr C'git_odb)
    -> m (Ptr C'git_odb)
lgLoadPackFileInMemory idxPath backendPtrPtr odbPtrPtr = do
    lgDebug "Create temporary, in-memory object database"
    odbPtr <- liftIO $ do
        r <- c'git_odb_new odbPtrPtr
        checkResult r "c'git_odb_new failed"
        peek odbPtrPtr

    lgDebug $ "Load pack index " ++ show idxPath ++ " into temporary odb"
    bracketOnError
        (do r <- liftIO $ withCString idxPath $ \idxPathStr ->
                c'git_odb_backend_one_pack backendPtrPtr idxPathStr
            checkResult r "c'git_odb_backend_one_pack failed"
            liftIO $ peek backendPtrPtr)
        (\backendPtr -> liftIO $ do
            backend <- peek backendPtr
            mK'git_odb_backend_free_callback
                (c'git_odb_backend'free backend) backendPtr)
        (\backendPtr -> do
            -- Associate the new backend containing our single index file with
            -- the in-memory object database
            lgDebug "Associate odb with backend"
            r <- liftIO $ c'git_odb_add_backend odbPtr backendPtr 1
            checkResult r "c'git_odb_add_backend failed")

    return odbPtr

lgOpenPackFile :: (MonadBaseControl IO m, MonadIO m, MonadExcept m)
               => FilePath -> m (Ptr C'git_odb)
lgOpenPackFile idxPath = control $ \run ->
    alloca $ \odbPtrPtr ->
    alloca $ \backendPtrPtr -> run $
        lgLoadPackFileInMemory idxPath backendPtrPtr odbPtrPtr

lgClosePackFile :: (MonadBaseControl IO m, MonadIO m, MonadExcept m)
               => Ptr C'git_odb -> m ()
lgClosePackFile = liftIO . c'git_odb_free

lgWithPackFile :: (MonadBaseControl IO m, MonadIO m, MonadExcept m)
               => FilePath -> (Ptr C'git_odb -> m a) -> m a
lgWithPackFile idxPath = bracket (lgOpenPackFile idxPath) lgClosePackFile

lgReadFromPack :: (MonadBaseControl IO m, MonadIO m, MonadExcept m)
               => Ptr C'git_odb -> Git.SHA -> Bool
               -> m (Maybe (C'git_otype, CSize, ByteString))
lgReadFromPack odbPtr sha metadataOnly = liftIO $ do
    foid <- shaToCOid sha
    if metadataOnly
        then readMetadata odbPtr foid
        else readObject odbPtr foid
  where
    readMetadata odbPtr foid = alloca $ \sizePtr -> alloca $ \typPtr -> do
        r <- withForeignPtr foid $
             c'git_odb_read_header sizePtr typPtr odbPtr
        if r == 0
            then Just <$> ((,,) <$> peek typPtr
                                <*> peek sizePtr
                                <*> pure B.empty)
            else do
                unless (r == c'GIT_ENOTFOUND) $
                    checkResult r "c'git_odb_read_header failed"
                return Nothing

    readObject odbPtr foid = alloca $ \objectPtrPtr -> do
        r <- withForeignPtr foid $
             c'git_odb_read objectPtrPtr odbPtr
        mr <- if r == 0
              then Just <$> peek objectPtrPtr
              else do
                  unless (r == c'GIT_ENOTFOUND) $
                      checkResult r "c'git_odb_read failed"
                  return Nothing
        forM mr $ \objectPtr -> do
            typ <- c'git_odb_object_type objectPtr
            len <- c'git_odb_object_size objectPtr
            ptr <- c'git_odb_object_data objectPtr
            bytes <- curry B.packCStringLen (castPtr ptr)
                         (fromIntegral len)
            c'git_odb_object_free objectPtr
            return (typ,len,bytes)

lgRemoteFetch :: MonadLg m => Text -> Text -> ReaderT LgRepo m ()
lgRemoteFetch uri fetchSpec = do
    xferRepo <- Git.getRepository
    liftIO $ withForeignPtr (repoObj xferRepo) $ \repoPtr ->
        withCString (unpack uri) $ \uriStr ->
        withCString (unpack fetchSpec) $ \fetchStr ->
            alloca $ runResourceT . go repoPtr uriStr fetchStr
  where
    go repoPtr uriStr fetchStr remotePtrPtr = do
        (_,remotePtr) <- flip allocate c'git_remote_free $ do
            r <- c'git_remote_create_inmemory remotePtrPtr repoPtr
                     fetchStr uriStr
            checkResult r "c'git_remote_create_inmemory failed"
            peek remotePtrPtr

        r1 <- liftIO $ c'git_remote_connect remotePtr c'GIT_DIRECTION_FETCH
        checkResult r1 "c'git_remote_connect failed"
        void $ register $ c'git_remote_disconnect remotePtr

        r2 <- liftIO $ c'git_remote_download remotePtr nullFunPtr nullPtr
        checkResult r2 "c'git_remote_download failed"

lgFactory :: MonadIO m
          => Git.RepositoryFactory (ReaderT LgRepo m) m LgRepo
lgFactory = Git.RepositoryFactory
    { Git.openRepository = openLgRepository
    , Git.runRepository  = runLgRepository
    }

runLgRepository :: LgRepo -> ReaderT LgRepo m a -> m a
runLgRepository = flip runReaderT

openLgRepository :: MonadIO m => Git.RepositoryOptions -> m LgRepo
openLgRepository opts = do
    startupLgBackend
    let path = Git.repoPath opts
    p <- liftIO $ doesDirectoryExist path
    liftIO $ openRepositoryWith path $
        if not (Git.repoAutoCreate opts) || p
        then c'git_repository_open
        else \x y -> c'git_repository_init x y
                         (fromBool (Git.repoIsBare opts))
  where
    openRepositoryWith path fn = do
        fptr <- alloca $ \ptr ->
            withCString path $ \str -> do
                r <- fn ptr str
                when (r < 0) $
                    error $ "Could not open repository " ++ show path
                ptr' <- peek ptr
                FC.newForeignPtr ptr' (c'git_repository_free ptr')
        excTrap <- newIORef Nothing
        return LgRepo
            { repoOptions = opts
            , repoObj     = fptr
            , repoExcTrap = excTrap
            }

startupLgBackend :: MonadIO m => m ()
startupLgBackend = liftIO (void c'git_threads_init)

shutdownLgBackend :: MonadIO m => m ()
shutdownLgBackend = liftIO c'git_threads_shutdown

-- Libgit2.hs
