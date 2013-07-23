{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
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
       ( LgRepository(..)
       , BlobOid()
       , Commit()
       , CommitOid()
       , Git.Oid
       , OidPtr(..)
       , mkOid
       , Repository(..)
       , Tree()
       , TreeOid()
       , repoPath
       , addTracingBackend
       , checkResult
       , closeLgRepository
       , defaultLgOptions
       , lgBuildPackIndex
       , lgFactory
       , lgForEachObject
       , lgGet
       , lgExcTrap
       , lgLoadPackFileInMemory
       , lgBuildPackFile
       , lgReadFromPack
       , lgWithPackFile
       , lgCopyPackFile
       , lgWrap
       , oidToSha
       , shaToOid
       , openLgRepository
       , runLgRepository
       , strToOid
       , withLibGitDo
       ) where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Exception
import qualified Control.Exception.Lifted as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Loops
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Bits ((.|.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import           Data.Conduit
import           Data.IORef
import           Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as U
import           Filesystem hiding (removeFile)
import           Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.C.String
import           Foreign.C.Types
import qualified Foreign.Concurrent as FC
import           Foreign.ForeignPtr
import qualified Foreign.ForeignPtr.Unsafe as FU
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.MissingAlloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Prelude hiding (FilePath)
import           System.Directory (removeFile)
import           System.IO (openBinaryTempFile, hClose)
import qualified System.IO.Unsafe as SU
import           Unsafe.Coerce

debug :: MonadIO m => String -> m ()
--debug = liftIO . putStrLn
debug = const (return ())

type Oid = OidPtr

data OidPtr = OidPtr { getOid :: ForeignPtr C'git_oid }

instance Git.IsOid OidPtr where
    renderOid = lgRenderOid

mkOid :: ForeignPtr C'git_oid -> OidPtr
mkOid = OidPtr

lgParseOidIO :: Text -> Int -> IO (Maybe Oid)
lgParseOidIO str len = do
    oid <- liftIO $ mallocForeignPtr
    r <- liftIO $ withCString (T.unpack str) $ \cstr ->
        withForeignPtr oid $ \ptr ->
            if len == 40
                then c'git_oid_fromstr ptr cstr
                else c'git_oid_fromstrn ptr cstr (fromIntegral len)
    return $ if r < 0
             then Nothing
             else Just (mkOid oid)

lgParseOid :: Git.MonadGit m => Text -> LgRepository m Oid
lgParseOid str
  | len > 40 = failure (Git.OidParseFailed str)
  | otherwise = do
      moid <- liftIO $ lgParseOidIO str len
      case moid of
          Nothing  -> failure (Git.OidParseFailed str)
          Just oid -> return oid
  where
    len = T.length str

lgRenderOid :: Git.Oid (LgRepository m) -> Text
lgRenderOid = T.pack . show

instance Show OidPtr where
    show (getOid -> coid) = SU.unsafePerformIO $ withForeignPtr coid oidToStr

instance Ord OidPtr where
    (getOid -> coid1) `compare` (getOid -> coid2) =
        SU.unsafePerformIO $
        withForeignPtr coid1 $ \coid1Ptr ->
        withForeignPtr coid2 $ \coid2Ptr -> do
            r <- c'git_oid_cmp coid1Ptr coid2Ptr
            return $ if r < 0
                     then LT
                     else if r > 0
                          then GT
                          else EQ

instance Eq OidPtr where
    oid1 == oid2 = oid1 `compare` oid2 == EQ

instance Git.MonadGit m => Git.Repository (LgRepository m) where
    type Oid (LgRepository m)  = OidPtr
    data Tree (LgRepository m) = LgTree
        { lgTreePtr :: Maybe (ForeignPtr C'git_tree) }
    data Options (LgRepository m) = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    parseOid = lgParseOid

    lookupReference   = lgLookupRef
    createReference   = lgUpdateRef
    updateReference   = lgUpdateRef
    deleteReference   = lgDeleteRef
    listReferences    = lgListRefs
    lookupCommit      = lgLookupCommit 40
    lookupTree        = lgLookupTree 40
    lookupBlob        = lgLookupBlob
    lookupTag         = error "Not implemented: LgRepository.lookupTag"
    lookupObject      = lgLookupObject
    existsObject      = lgExistsObject
    sourceObjects     = lgSourceObjects
    newTreeBuilder    = lgNewTreeBuilder
    treeEntry         = lgTreeEntry
    treeOid           = lgTreeOid
    listTreeEntries   = lgListTreeEntries
    hashContents      = lgHashContents
    createBlob        = lgWrap . lgCreateBlob
    createTag         = error "Not implemented: LgRepository.createTag"

    createCommit p t a c l r = lgWrap $ lgCreateCommit p t a c l r

    deleteRepository = lgGet >>= liftIO . removeTree . repoPath

    -- buildPackFile   = lgBuildPackFile
    -- buildPackIndex  = lgBuildPackIndexWrapper
    -- writePackFile   = lgWrap . lgWritePackFile

    -- remoteFetch     = lgRemoteFetch

lgWrap :: (MonadIO m, MonadBaseControl IO m)
       => LgRepository m a -> LgRepository m a
lgWrap f = f `Exc.catch` \e -> do
    etrap <- lgExcTrap
    mexc  <- liftIO $ readIORef etrap
    liftIO $ writeIORef etrap Nothing
    maybe (throw (e :: SomeException)) throw mexc

lgHashContents :: Git.MonadGit m => Git.BlobContents (LgRepository m)
               -> LgRepository m (BlobOid m)
lgHashContents b = do
    ptr <- liftIO mallocForeignPtr
    r   <- Git.blobContentsToByteString b >>= \bs ->
        liftIO $ withForeignPtr ptr $ \oidPtr ->
            BU.unsafeUseAsCStringLen bs $
                uncurry (\cstr len ->
                          c'git_odb_hash oidPtr (castPtr cstr)
                                         (fromIntegral len) c'GIT_OBJ_BLOB)
    when (r < 0) $ failure Git.BlobCreateFailed
    return (Tagged (mkOid ptr))

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the given string is 'empty', it is an error.
lgCreateBlob :: Git.MonadGit m
             => Git.BlobContents (LgRepository m)
             -> LgRepository m (BlobOid m)
lgCreateBlob b = do
    repo <- lgGet
    ptr  <- liftIO mallocForeignPtr
    r    <- Git.blobContentsToByteString b
            >>= \bs -> liftIO $ evaluate
                       =<< createBlobFromByteString repo ptr bs
    when (r < 0) $ failure Git.BlobCreateFailed
    return (Tagged (mkOid ptr))

  where
    createBlobFromByteString repo coid bs =
        BU.unsafeUseAsCStringLen bs $
            uncurry (\cstr len ->
                      withForeignPtr coid $ \coid' ->
                      withForeignPtr (repoObj repo) $ \repoPtr ->
                        c'git_blob_create_frombuffer
                          coid' repoPtr (castPtr cstr) (fromIntegral len))

lgObjToBlob :: Git.MonadGit m
            => BlobOid m -> Ptr C'git_blob -> IO (Git.Blob (LgRepository m))
lgObjToBlob oid ptr = do
    size <- c'git_blob_rawsize ptr
    buf  <- c'git_blob_rawcontent ptr
    -- The lifetime of buf is tied to the lifetime of the blob object in
    -- libgit2, which this Blob object controls, so we can use
    -- unsafePackCStringLen to refer to its bytes.
    bstr <- curry B.packCStringLen (castPtr buf) (fromIntegral size)
    return $ Git.Blob oid (Git.BlobString bstr)

lgLookupBlob :: Git.MonadGit m => BlobOid m
             -> LgRepository m (Git.Blob (LgRepository m))
lgLookupBlob oid =
    lookupObject'
        (getOid (untag oid))
        40
        c'git_blob_lookup
        c'git_blob_lookup_prefix
        $ \_ obj _ -> withForeignPtr obj $ lgObjToBlob oid

type TreeEntry m = Git.TreeEntry (LgRepository m)

lgTreeEntry :: Git.MonadGit m => Tree m -> Text
            -> LgRepository m (Maybe (TreeEntry m))
lgTreeEntry (LgTree Nothing) _ = return Nothing
lgTreeEntry (LgTree (Just tree)) fp = liftIO $ alloca $ \entryPtr ->
    withCString (T.unpack fp) $ \pathStr ->
        withForeignPtr tree $ \treePtr -> do
            r <- c'git_tree_entry_bypath entryPtr treePtr pathStr
            if r < 0
                then return Nothing
                else Just <$> (entryToTreeEntry =<< peek entryPtr)

lgTreeOid :: Git.MonadGit m => Tree m -> TreeOid m
lgTreeOid (LgTree Nothing) =
    SU.unsafePerformIO . liftIO $
        Tagged . fromJust <$> lgParseOidIO Git.emptyTreeId 40
lgTreeOid (LgTree (Just tree)) = SU.unsafePerformIO $ liftIO $ do
    toid  <- withForeignPtr tree $ c'git_tree_id
    ftoid <- coidPtrToOid toid
    return $ Tagged (mkOid ftoid)

lgListTreeEntries :: Git.MonadGit m
                  => Tree m
                  -> LgRepository m [(Text, TreeEntry m)]
lgListTreeEntries (LgTree Nothing) = return []
lgListTreeEntries (LgTree (Just tree)) = do
    liftIO $ withForeignPtr tree $ \tr -> do
        ior <- newIORef []
        r <- bracket
                (mk'git_treewalk_cb (callback ior))
                freeHaskellFunPtr
                (flip (c'git_tree_walk tr c'GIT_TREEWALK_PRE) nullPtr)
        when (r < 0) $ failure Git.TreeWalkFailed
        readIORef ior

  where
    callback ior root te _ = do
        fp    <- peekCString root
        cname <- c'git_tree_entry_name te
        name  <- T.pack . (fp <>) <$> peekCString cname
        entry <- entryToTreeEntry te
        seq name $ seq entry $ modifyIORef ior $ \xs -> (name,entry):xs
        return 0

lgMakeBuilder :: Git.MonadGit m
              => ForeignPtr C'git_treebuilder -> TreeBuilder m
lgMakeBuilder builder = Git.TreeBuilder
    { Git.mtbBaseTreeOid    = Nothing
    , Git.mtbPendingUpdates = mempty
    , Git.mtbNewBuilder     = lgNewTreeBuilder
    , Git.mtbWriteContents  = \tb -> (,) <$> pure (Git.BuilderUnchanged tb)
                                         <*> lgWriteBuilder builder
    , Git.mtbLookupEntry    = lgLookupBuilderEntry builder
    , Git.mtbEntryCount     = lgBuilderEntryCount builder
    , Git.mtbPutEntry       = \tb name ent ->
        lgPutEntry builder name ent >> return (Git.BuilderUnchanged tb)
    , Git.mtbDropEntry      = \tb name ->
        lgDropEntry builder name >> return (Git.BuilderUnchanged tb)
    }

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
lgNewTreeBuilder :: Git.MonadGit m
                 => Maybe (Tree m) -> LgRepository m (TreeBuilder m)
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
            failure (Git.TreeCreateFailed "Failed to create new tree builder")
        Just fptr ->
            return (lgMakeBuilder fptr)
                { Git.mtbBaseTreeOid = Git.treeOid <$> mtree }

lgPutEntry :: Git.MonadGit m
           => ForeignPtr C'git_treebuilder -> Text -> TreeEntry m
           -> LgRepository m ()
lgPutEntry builder key (treeEntryToOid -> (oid,mode)) = do
    r2 <- liftIO $ withForeignPtr (getOid oid) $ \coid ->
        withForeignPtr builder $ \ptr ->
        withCString (T.unpack key) $ \name ->
            c'git_treebuilder_insert nullPtr ptr name coid
                (fromIntegral mode)
    when (r2 < 0) $ failure (Git.TreeBuilderInsertFailed key)

treeEntryToOid :: TreeEntry m -> (Oid, CUInt)
treeEntryToOid (Git.BlobEntry oid kind) =
    (untag oid, case kind of
          Git.PlainBlob      -> 0o100644
          Git.ExecutableBlob -> 0o100755
          Git.SymlinkBlob    -> 0o120000
          Git.UnknownBlob    -> 0o100000)
treeEntryToOid (Git.CommitEntry coid) =
    (untag coid, 0o160000)
treeEntryToOid (Git.TreeEntry toid) =
    (untag toid, 0o040000)

lgDropEntry :: Git.MonadGit m
            => ForeignPtr C'git_treebuilder -> Text -> LgRepository m ()
lgDropEntry builder key = do
    void $ liftIO $ withForeignPtr builder $ \ptr ->
        withCString (T.unpack key) $ c'git_treebuilder_remove ptr

lgLookupBuilderEntry :: Git.MonadGit m
                     => ForeignPtr C'git_treebuilder
                     -> Text
                     -> LgRepository m (Maybe (TreeEntry m))
lgLookupBuilderEntry builderPtr name = do
    entry <- liftIO $ withForeignPtr builderPtr $ \builder -> do
        withCString (T.unpack name) (c'git_treebuilder_get builder)
    if entry == nullPtr
        then return Nothing
        else Just <$> liftIO (entryToTreeEntry entry)

lgBuilderEntryCount :: Git.MonadGit m
                    => ForeignPtr C'git_treebuilder -> LgRepository m Int
lgBuilderEntryCount tb = do
    fromIntegral <$> liftIO (withForeignPtr tb c'git_treebuilder_entrycount)

lgTreeEntryCount :: Git.MonadGit m => Tree m -> LgRepository m Int
lgTreeEntryCount (LgTree Nothing) = return 0
lgTreeEntryCount (LgTree (Just tree)) = do
    fromIntegral <$> liftIO (withForeignPtr tree c'git_tree_entrycount)

lgWriteBuilder :: Git.MonadGit m
               => ForeignPtr C'git_treebuilder
               -> LgRepository m (TreeOid m)
lgWriteBuilder tb = do
    repo <- lgGet
    (r3,coid) <- liftIO $ do
        coid <- mallocForeignPtr
        withForeignPtr coid $ \coid' ->
            withForeignPtr tb $ \builder ->
            withForeignPtr (repoObj repo) $ \repoPtr -> do
                r3 <- c'git_treebuilder_write coid' repoPtr builder
                return (r3,coid)
    when (r3 < 0) $ do
        errStr <- liftIO $ do
            errPtr <- c'giterr_last
            err    <- peek errPtr
            peekCString (c'git_error'message err)
        failure (Git.TreeBuilderWriteFailed $ T.pack $
                 "c'git_treebuilder_write failed with " ++ show r3
                 ++ ": " ++ errStr)
    return $ Tagged (mkOid coid)

lgCloneBuilder :: Git.MonadGit m
               => ForeignPtr C'git_treebuilder
               -> LgRepository m (ForeignPtr C'git_treebuilder)
lgCloneBuilder fptr =
    liftIO $ withForeignPtr fptr $ \builder -> alloca $ \pptr -> do
        r <- c'git_treebuilder_create pptr nullPtr
        when (r < 0) $
            failure (Git.BackendError "Could not create new treebuilder")
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
            failure (Git.BackendError "Could not insert entry in treebuilder")
        return 0

lgLookupTree :: Git.MonadGit m => Int -> Tagged (Tree m) Oid
             -> LgRepository m (Tree m)
lgLookupTree len (getOid . untag -> oid) = do
    str <- liftIO $ withForeignPtr oid $ \oidPtr -> oidToStr oidPtr
    if str == T.unpack Git.emptyTreeId
        then return (LgTree Nothing)
        else do
            -- jww (2013-01-28): Verify the oid here
            fptr <- lookupObject' oid len
                  c'git_tree_lookup c'git_tree_lookup_prefix $
                  \_coid obj _ -> return obj
            return $ LgTree (Just fptr)

entryToTreeEntry :: Ptr C'git_tree_entry -> IO (TreeEntry m)
entryToTreeEntry entry = do
    coid <- c'git_tree_entry_id entry
    oid  <- coidPtrToOid coid
    typ  <- c'git_tree_entry_type entry
    case () of
        () | typ == c'GIT_OBJ_BLOB ->
             do mode <- c'git_tree_entry_filemode entry
                return $ Git.BlobEntry (Tagged (mkOid oid)) $
                    case mode of
                        0o100644 -> Git.PlainBlob
                        0o100755 -> Git.ExecutableBlob
                        0o120000 -> Git.SymlinkBlob
                        _        -> Git.UnknownBlob
           | typ == c'GIT_OBJ_TREE ->
             return $ Git.TreeEntry (Tagged (mkOid oid))
           | typ == c'GIT_OBJ_COMMIT ->
             return $ Git.CommitEntry (Tagged (mkOid oid))
           | otherwise -> error "Unexpected"

lgObjToCommit :: Git.MonadGit m
              => CommitOid m -> Ptr C'git_commit -> IO (Commit m)
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

lgLookupCommit :: Git.MonadGit m
               => Int -> CommitOid m -> LgRepository m (Commit m)
lgLookupCommit len oid =
  lookupObject'
      (getOid (untag oid))
      len
      c'git_commit_lookup
      c'git_commit_lookup_prefix
      $ \_ obj _ -> withForeignPtr obj $ lgObjToCommit oid

data ObjectPtr = BlobPtr (ForeignPtr C'git_blob)
               | TreePtr (ForeignPtr C'git_commit)
               | CommitPtr (ForeignPtr C'git_commit)
               | TagPtr (ForeignPtr C'git_tag)

lgLookupObject :: Git.MonadGit m
               => Oid -> LgRepository m (Git.Object (LgRepository m))
lgLookupObject (Git.renderOid -> str)
    | len > 40 = failure (Git.ObjectLookupFailed str len)
    | otherwise = do
        mfptr <- liftIO $ do
            fptr <- mallocForeignPtr
            withForeignPtr fptr $ \ptr ->
                withCString (T.unpack str) $ \cstr -> do
                    r <- if len == 40
                         then c'git_oid_fromstr ptr cstr
                         else c'git_oid_fromstrn ptr cstr (fromIntegral len)
                    return $ if r < 0 then Nothing else Just fptr

        case mfptr of
            Nothing   -> failure (Git.ObjectLookupFailed str len)
            Just fptr -> go fptr
  where
    len = T.length str
    go fptr = do
        (oid,typ,fptr) <-
            lookupObject' fptr len
                (\x y z -> c'git_object_lookup x y z c'GIT_OBJ_ANY)
                (\x y z l ->
                  c'git_object_lookup_prefix x y z l c'GIT_OBJ_ANY)
                $ \_ fptr y -> do
                    coid <- c'git_object_id y
                    oid  <- mkOid <$> coidPtrToOid coid
                    typ  <- c'git_object_type y
                    return (oid,typ,fptr)
        case () of
            () | typ == c'GIT_OBJ_BLOB   ->
                    Git.BlobObj <$>
                    liftIO (withForeignPtr fptr $ \y ->
                             lgObjToBlob (Tagged oid) (castPtr y))
               | typ == c'GIT_OBJ_TREE   ->
                    -- A ForeignPtr C'git_object is bit-wise equivalent to a
                    -- ForeignPtr C'git_tree.
                    return $ Git.TreeObj (LgTree (Just (unsafeCoerce fptr)))
               | typ == c'GIT_OBJ_COMMIT ->
                    Git.CommitObj <$>
                    liftIO (withForeignPtr fptr $ \y ->
                             lgObjToCommit (Tagged oid) (castPtr y))
               | typ == c'GIT_OBJ_TAG -> error "jww (2013-07-08): NYI"
               | otherwise -> error $ "Unknown object type: " ++ show typ

lgExistsObject :: Git.MonadGit m => Oid -> LgRepository m Bool
lgExistsObject oid = do
    repo <- lgGet
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
    maybe (failure Git.RepositoryInvalid) return result

lgForEachObject :: Ptr C'git_odb
                -> (Ptr C'git_oid -> Ptr () -> IO CInt)
                -> Ptr ()
                -> IO CInt
lgForEachObject odbPtr f payload =
    bracket
        (mk'git_odb_foreach_cb f)
        freeHaskellFunPtr
        (flip (c'git_odb_foreach odbPtr) payload)

lgSourceObjects
    :: Git.MonadGit m
    => Maybe (CommitOid m) -> CommitOid m -> Bool
    -> Source (LgRepository m) (ObjectOid m)
lgSourceObjects mhave need alsoTrees = do
    repo   <- lift $ lgGet
    walker <- liftIO $ alloca $ \pptr -> do
        r <- withForeignPtr (repoObj repo) $ \repoPtr ->
                c'git_revwalk_new pptr repoPtr
        when (r < 0) $
            failure (Git.BackendError "Could not create revwalker")
        ptr <- peek pptr
        newForeignPtr p'git_revwalk_free ptr

    liftIO $ withForeignPtr (getOid (untag need)) $ \coid -> do
        r2 <- withForeignPtr walker $ flip c'git_revwalk_push coid
        when (r2 < 0) $
            failure (Git.BackendError "Could not push oid on revwalker")

    case mhave of
        Nothing   -> return ()
        Just have -> liftIO $ withForeignPtr (getOid (untag have)) $ \coid -> do
            r2 <- withForeignPtr walker $ flip c'git_revwalk_hide coid
            when (r2 < 0) $
                failure (Git.BackendError "Could not hide commit on revwalker")

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
                c <- lift $ lgLookupCommit 40 coid
                yield $ Git.TreeObjOid (Git.commitTree c)

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
lgCreateCommit :: Git.MonadGit m
               => [CommitOid m]
               -> TreeOid m
               -> Git.Signature
               -> Git.Signature
               -> Text
               -> Maybe Text
               -> LgRepository m (Commit m)
lgCreateCommit pptrs tree author committer logText ref = do
    repo <- lgGet
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
                when (r < 0) $ throwIO Git.CommitCreateFailed
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

lgLookupRef :: Git.MonadGit m => Text -> LgRepository m (Maybe (RefTarget m))
lgLookupRef name = do
    repo <- lgGet
    liftIO $ alloca $ \ptr -> do
        r <- withForeignPtr (repoObj repo) $ \repoPtr ->
              withCString (T.unpack name) $ \namePtr ->
                c'git_reference_lookup ptr repoPtr namePtr
        if r < 0
            then return Nothing
            else do
            ref  <- peek ptr
            typ  <- c'git_reference_type ref
            targ <- if typ == c'GIT_REF_OID
                    then do oidPtr <- c'git_reference_target ref
                            Git.RefObj . Tagged . mkOid
                                <$> coidPtrToOid oidPtr
                    else do targName <- c'git_reference_symbolic_target ref
                            Git.RefSymbolic . T.decodeUtf8
                                <$> B.packCString targName
            c'git_reference_free ref
            return (Just targ)

lgUpdateRef :: Git.MonadGit m
            => Text -> Git.RefTarget (LgRepository m)
            -> LgRepository m ()
lgUpdateRef name refTarg = do
    repo <- lgGet
    r <- liftIO $ alloca $ \ptr ->
        withForeignPtr (repoObj repo) $ \repoPtr ->
        withCString (T.unpack name) $ \namePtr -> do
            case refTarg of
                Git.RefObj oid ->
                    withForeignPtr (getOid (untag oid)) $ \coidPtr ->
                        c'git_reference_create ptr repoPtr namePtr
                                               coidPtr (fromBool True)

                Git.RefSymbolic symName ->
                    withCString (T.unpack symName) $ \symPtr ->
                        c'git_reference_symbolic_create ptr repoPtr namePtr
                                                        symPtr (fromBool True)
    when (r < 0) $ do
        errStr <- liftIO $ do
            errPtr <- c'giterr_last
            err    <- peek errPtr
            peekCString (c'git_error'message err)
        failure (Git.ReferenceCreateFailed $ name <> " => "
                 <> T.pack (show refTarg) <> ": " <> T.pack errStr)

-- int git_reference_name_to_oid(git_oid *out, git_repository *repo,
--   const char *name)

lgResolveRef :: Git.MonadGit m => Text -> LgRepository m (Maybe (CommitOid m))
lgResolveRef name = do
    repo <- lgGet
    oid <- liftIO $ alloca $ \ptr ->
        withCString (T.unpack name) $ \namePtr ->
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            r <- c'git_reference_name_to_id ptr repoPtr namePtr
            if r < 0
                then return Nothing
                else Just <$> coidPtrToOid ptr
    return $ Tagged . mkOid <$> oid

-- int git_reference_rename(git_reference *ref, const char *new_name,
--   int force)

--renameRef = c'git_reference_rename

lgDeleteRef :: Git.MonadGit m => Text -> LgRepository m ()
lgDeleteRef name = do
    repo <- lgGet
    r <- liftIO $ alloca $ \ptr ->
        withCString (T.unpack name) $ \namePtr ->
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            r <- c'git_reference_lookup ptr repoPtr namePtr
            if r < 0
                then return r
                else do
                ref <- peek ptr
                c'git_reference_delete ref
    when (r < 0) $ failure (Git.ReferenceDeleteFailed name)

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
  return $ fmap T.pack r1

flagsToInt :: ListFlags -> CUInt
flagsToInt flags = (if listFlagOid flags      then 1 else 0)
                 + (if listFlagSymbolic flags then 2 else 0)
                 + (if listFlagPacked flags   then 4 else 0)
                 + (if listFlagHasPeel flags  then 8 else 0)

listRefNames :: Git.MonadGit m
             => ListFlags -> LgRepository m [Text]
listRefNames flags = do
    repo <- lgGet
    refs <- liftIO $ alloca $ \c'refs ->
      withForeignPtr (repoObj repo) $ \repoPtr -> do
        r <- c'git_reference_list c'refs repoPtr (flagsToInt flags)
        if r < 0
            then return Nothing
            else do refs <- gitStrArray2List c'refs
                    c'git_strarray_free c'refs
                    return (Just refs)
    maybe (failure Git.ReferenceListingFailed) return refs

lgListRefs :: Git.MonadGit m
              => LgRepository m [Text]
lgListRefs = listRefNames allRefsFlag

-- foreachRefCallback :: CString -> Ptr () -> IO CInt
-- foreachRefCallback name payload = do
--   (callback,results) <- deRefStablePtr =<< peek (castPtr payload)
--   nameStr <- peekCString name
--   result <- callback (T.pack nameStr)
--   modifyIORef results (\xs -> result:xs)
--   return 0

-- foreign export ccall "foreachRefCallback"
--   foreachRefCallback :: CString -> Ptr () -> IO CInt
-- foreign import ccall "&foreachRefCallback"
--   foreachRefCallbackPtr :: FunPtr (CString -> Ptr () -> IO CInt)

-- lgMapRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- lgMapRefs cb = do
--     repo <- lgGet
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

checkResult :: (Eq a, Num a, Failure Git.GitException m) => a -> Text -> m ()
checkResult r why = when (r /= 0) $ failure (Git.BackendError why)

lgBuildPackFile :: Git.MonadGit m
                => FilePath -> [Either (CommitOid m) (TreeOid m)]
                -> LgRepository m FilePath
lgBuildPackFile dir oids = do
    repo <- lgGet
    liftIO $ do
        (filePath, fHandle) <- openBinaryTempFile (pathStr dir) "pack"
        hClose fHandle
        go repo filePath
        return . F.fromText . T.pack $ filePath
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

lgBuildPackIndexWrapper :: Git.MonadGit m
                        => FilePath
                        -> B.ByteString
                        -> LgRepository m (Text, FilePath, FilePath)
lgBuildPackIndexWrapper = (liftIO .) . lgBuildPackIndex

lgBuildPackIndex :: FilePath -> B.ByteString
                 -> IO (Text, FilePath, FilePath)
lgBuildPackIndex dir bytes = do
    sha <- go dir bytes
    (,,) <$> pure sha
         <*> pure (dir </> F.fromText ("pack-" <> sha <> ".pack"))
         <*> pure (dir </> F.fromText ("pack-" <> sha <> ".idx"))
  where
    go dir bytes = alloca $ \idxPtrPtr -> runResourceT $ do
        debug "Allocate a new indexer stream"
        (_,idxPtr) <- flip allocate c'git_indexer_stream_free $
            withCString (pathStr dir) $ \dirStr -> do
                r <- c'git_indexer_stream_new idxPtrPtr dirStr
                         nullFunPtr nullPtr
                checkResult r "c'git_indexer_stream_new failed"
                peek idxPtrPtr

        debug $ "Add the incoming packfile data to the stream ("
            ++ show (B.length bytes) ++ " bytes)"
        (_,statsPtr) <- allocate calloc free
        liftIO $ BU.unsafeUseAsCStringLen bytes $
            uncurry $ \dataPtr dataLen -> do
                r <- c'git_indexer_stream_add idxPtr (castPtr dataPtr)
                         (fromIntegral dataLen) statsPtr
                checkResult r "c'git_indexer_stream_add failed"

        debug "Finalizing the stream"
        r <- liftIO $ c'git_indexer_stream_finalize idxPtr statsPtr
        checkResult r "c'git_indexer_stream_finalize failed"

        debug "Discovering the hash used to identify the pack file"
        sha <- liftIO $ oidToSha =<< c'git_indexer_stream_hash idxPtr
        debug $ "The hash used is: " ++ show (Git.shaToText sha)
        return (Git.shaToText sha)

strToOid :: String -> IO (ForeignPtr C'git_oid)
strToOid oidStr = do
    ptr <- mallocForeignPtr
    withCString oidStr $ \cstr ->
      withForeignPtr ptr $ \ptr' -> do
        r <- c'git_oid_fromstr ptr' cstr
        when (r < 0) $ throwIO Git.OidCopyFailed
        return ptr

oidToSha :: Ptr C'git_oid -> IO Git.SHA
oidToSha oidPtr =
    Git.SHA <$> B.packCStringLen
        (castPtr oidPtr, sizeOf (undefined :: C'git_oid))

shaToOid :: Git.SHA -> IO (ForeignPtr C'git_oid)
shaToOid (Git.SHA bs) = BU.unsafeUseAsCString bs $ \bytes -> do
    ptr <- mallocForeignPtr
    withForeignPtr ptr $ \ptr' -> do
        c'git_oid_fromraw ptr' (castPtr bytes)
        return ptr

lgCopyPackFile :: Git.MonadGit m => FilePath -> LgRepository m ()
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
    repo <- lgGet
    liftIO $ withForeignPtr (repoObj repo) $ \repoPtr ->
        alloca $ \odbPtrPtr ->
        alloca $ \statsPtr ->
        alloca $ \writepackPtrPtr ->
            runResourceT $ go repoPtr odbPtrPtr writepackPtrPtr statsPtr
  where
    go repoPtr odbPtrPtr writepackPtrPtr statsPtr = do
        debug "Obtaining odb for repository"
        (_,odbPtr) <- flip allocate c'git_odb_free $ do
            r <- c'git_repository_odb odbPtrPtr repoPtr
            checkResult r "c'git_repository_odb failed"
            peek odbPtrPtr

        debug "Opening pack writer into odb"
        (_,writepackPtr) <- allocate
            (do r <- c'git_odb_write_pack writepackPtrPtr odbPtr
                         nullFunPtr nullPtr
                checkResult r "c'git_odb_write_pack failed"
                peek writepackPtrPtr)
            (\writepackPtr -> do
                  writepack <- peek writepackPtr
                  mK'git_odb_writepack_free_callback
                      (c'git_odb_writepack'free writepack) writepackPtr)
        writepack <- liftIO $ peek writepackPtr

        bs <- liftIO $ B.readFile (pathStr packFile)
        debug $ "Writing pack file " ++ show packFile ++ " into odb"
        debug $ "Writing " ++ show (B.length bs) ++ " pack bytes into odb"
        liftIO $ BU.unsafeUseAsCStringLen bs $
            uncurry $ \dataPtr dataLen -> do
                r <- mK'git_odb_writepack_add_callback
                         (c'git_odb_writepack'add writepack)
                         writepackPtr (castPtr dataPtr)
                         (fromIntegral dataLen) statsPtr
                checkResult r "c'git_odb_writepack'add failed"

        debug "Committing pack into odb"
        r <- liftIO $ mK'git_odb_writepack_commit_callback
                 (c'git_odb_writepack'commit writepack) writepackPtr
                 statsPtr
        checkResult r "c'git_odb_writepack'commit failed"

lgLoadPackFileInMemory :: FilePath
                       -> Ptr (Ptr C'git_odb_backend)
                       -> Ptr (Ptr C'git_odb)
                       -> ResourceT IO (Ptr C'git_odb)
lgLoadPackFileInMemory idxPath backendPtrPtr odbPtrPtr = do
    debug "Creating temporary, in-memory object database"
    (freeKey,odbPtr) <- flip allocate c'git_odb_free $ do
        r <- c'git_odb_new odbPtrPtr
        checkResult r "c'git_odb_new failed"
        peek odbPtrPtr

    debug $ "Load pack index " ++ show idxPath ++ " into temporary odb"
    (_,backendPtr) <- allocate
        (do r <- withCString (pathStr idxPath) $ \idxPathStr ->
                c'git_odb_backend_one_pack backendPtrPtr idxPathStr
            checkResult r "c'git_odb_backend_one_pack failed"
            peek backendPtrPtr)
        (\backendPtr -> do
              backend <- peek backendPtr
              mK'git_odb_backend_free_callback
                  (c'git_odb_backend'free backend) backendPtr)

    -- Since freeing the backend will now free the object database, unregister
    -- the finalizer we had setup for the odbPtr
    void $ unprotect freeKey

    -- Associate the new backend containing our single index file with the
    -- in-memory object database
    debug "Associate odb with backend"
    r <- liftIO $ c'git_odb_add_backend odbPtr backendPtr 1
    checkResult r "c'git_odb_add_backend failed"

    return odbPtr

lgWithPackFile :: FilePath -> (Ptr C'git_odb -> ResourceT IO a) -> IO a
lgWithPackFile idxPath f = alloca $ \odbPtrPtr ->
    alloca $ \backendPtrPtr -> runResourceT $ do
        debug "Load pack file into an in-memory object database"
        odbPtr <- lgLoadPackFileInMemory idxPath backendPtrPtr odbPtrPtr
        debug "Calling function using in-memory odb"
        f odbPtr

lgReadFromPack :: FilePath -> Git.SHA -> Bool
               -> IO (Maybe (C'git_otype, CSize, B.ByteString))
lgReadFromPack idxPath sha metadataOnly =
    alloca $ \objectPtrPtr ->
    lgWithPackFile idxPath $ \odbPtr -> do
        foid <- liftIO $ shaToOid sha
        if metadataOnly
            then liftIO $ alloca $ \sizePtr -> alloca $ \typPtr -> do
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
            else do
                r <- liftIO $ withForeignPtr foid $
                     c'git_odb_read objectPtrPtr odbPtr
                mr <- if r == 0
                      then do
                          objectPtr <- liftIO $ peek objectPtrPtr
                          void $ register $ c'git_odb_object_free objectPtr
                          return $ Just objectPtr
                      else do
                          unless (r == c'GIT_ENOTFOUND) $
                              checkResult r "c'git_odb_read failed"
                          return Nothing
                case mr of
                    Just objectPtr -> liftIO $ do
                        typ <- c'git_odb_object_type objectPtr
                        len <- c'git_odb_object_size objectPtr
                        ptr <- c'git_odb_object_data objectPtr
                        bytes <- curry B.packCStringLen (castPtr ptr)
                                     (fromIntegral len)
                        return $ Just (typ,len,bytes)
                    Nothing -> return Nothing

lgRemoteFetch :: Git.MonadGit m => Text -> Text -> LgRepository m ()
lgRemoteFetch uri fetchSpec = do
    xferRepo <- lgGet
    liftIO $ withForeignPtr (repoObj xferRepo) $ \repoPtr ->
        withCString (T.unpack uri) $ \uriStr ->
        withCString (T.unpack fetchSpec) $ \fetchStr ->
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

lgFactory :: Git.MonadGit m
          => Git.RepositoryFactory LgRepository m Repository
lgFactory = Git.RepositoryFactory
    { Git.openRepository  = openLgRepository
    , Git.runRepository   = runLgRepository
    , Git.closeRepository = closeLgRepository
    , Git.getRepository   = lgGet
    , Git.defaultOptions  = defaultLgOptions
    , Git.startupBackend  = startupLgBackend
    , Git.shutdownBackend = shutdownLgBackend
    }

withFilePath :: FilePath -> (CString -> IO a) -> IO a
withFilePath fp f = case F.toText fp of
    Left _  -> error $ "Could not translate path: " ++ show fp
    Right p -> withCString (T.unpack p) f

openLgRepository :: Git.MonadGit m => Git.RepositoryOptions -> m Repository
openLgRepository opts = do
    let path = Git.repoPath opts
    p <- liftIO $ isDirectory path
    liftIO $ openRepositoryWith path $
        if not (Git.repoAutoCreate opts) || p
        then c'git_repository_open
        else \x y -> c'git_repository_init x y
                         (fromBool (Git.repoIsBare opts))
  where
    openRepositoryWith path fn = do
        fptr <- alloca $ \ptr ->
            withFilePath path $ \str -> do
                r <- fn ptr str
                when (r < 0) $
                    error $ "Could not open repository " ++ show path
                ptr' <- peek ptr
                newForeignPtr p'git_repository_free ptr'
        excTrap <- newIORef Nothing
        return Repository { repoOptions = opts
                          , repoObj     = fptr
                          , repoExcTrap = excTrap
                          }

runLgRepository :: Repository -> LgRepository m a -> m a
runLgRepository repo action =
    runReaderT (lgRepositoryReaderT action) repo

closeLgRepository :: Git.MonadGit m => Repository -> m ()
closeLgRepository = const (return ())

defaultLgOptions :: Git.RepositoryOptions
defaultLgOptions = Git.RepositoryOptions "" False False

startupLgBackend :: MonadIO m => m ()
startupLgBackend = liftIO (void c'git_threads_init)

shutdownLgBackend :: MonadIO m => m ()
shutdownLgBackend = liftIO c'git_threads_shutdown

-- Libgit2.hs
