{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Interface for opening and creating repositories.  Repository objects are
--   immutable, and serve only to refer to the given repository.  Any data
--   associated with the repository — such as the list of branches — is
--   queried as needed.
module Git.Libgit2
       ( LgRepository(..), Repository(..)
       , Git.Oid(..), BlobOid(..), TreeOid(..), CommitOid(..)
       , Tree(..), Commit(..)
       , TreeRef(..), CommitRef(..), Reference(..)
       , lgFactory, openLgRepository, runLgRepository
       , closeLgRepository, defaultLgOptions
       , lgGet, addTracingBackend
       ) where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as BU
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List as L
import           Data.Maybe
import           Data.Stringable as S
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as U
import           Data.Traversable (for)
import           Filesystem
import           Filesystem.Path.CurrentOS (FilePath, (</>))
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.C.String
import           Foreign.C.Types
import qualified Foreign.Concurrent as FC
import           Foreign.ForeignPtr
import qualified Foreign.ForeignPtr.Unsafe as FU
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git as Git
import           Git.Libgit2.Internal
import           Git.Libgit2.Trace
import           Git.Libgit2.Types
import qualified Git.Utils as Git
import           Prelude hiding (FilePath)
import qualified System.IO.Unsafe as SU

instance M m => Git.RepositoryBase (LgRepository m) where
    data Oid (LgRepository m) = Oid
        { getOid :: ForeignPtr C'git_oid }

    data Tree (LgRepository m) = Tree
        { lgTreeInfo       :: IORef (Base m (Tree m) C'git_tree)
        , lgPendingUpdates :: IORef (HashMap Text (Tree m))
        , lgTreeContents   :: ForeignPtr C'git_treebuilder }

    data Commit (LgRepository m) = Commit
        { lgCommitInfo      :: Base m (Commit m) C'git_commit
        , lgCommitAuthor    :: Git.Signature
        , lgCommitCommitter :: Git.Signature
        , lgCommitLog       :: Text
        , lgCommitEncoding  :: String
        , lgCommitTree      :: TreeRef m
        , lgCommitParents   :: [CommitRef m] }

    data Tag (LgRepository m) = Tag
        { tagCommit :: CommitRef m }

    data Options (LgRepository m) = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    parseOid     = lgParseOid
    renderOid    = lgRenderOid
    lookupRef    = lgLookupRef
    createRef    = lgUpdateRef
    updateRef    = lgUpdateRef
    deleteRef    = lgDeleteRef
    resolveRef   = lgResolveRef
    allRefNames  = lgAllRefNames
    lookupCommit = lgLookupCommit 40
    lookupTree   = lgLookupTree 40
    lookupBlob   = lgLookupBlob
    lookupTag    = undefined
    lookupObject = lgLookupObject
    existsObject = lgExistsObject
    newTree      = lgNewTree
    hashContents = lgHashContents
    createBlob   = lgCreateBlob
    createCommit = lgCreateCommit
    createTag    = undefined

    deleteRepository = lgGet >>= liftIO . removeTree . repoPath

lgParseOid :: M m => Text -> LgRepository m (Oid m)
lgParseOid str
  | len > 40 = failure (Git.OidParseFailed str)
  | otherwise = do
      oid <- liftIO $ mallocForeignPtr
      r <- liftIO $ withCStringable str $ \cstr ->
          withForeignPtr oid $ \ptr ->
              if len == 40
                  then c'git_oid_fromstr ptr cstr
                  else c'git_oid_fromstrn ptr cstr (fromIntegral len)
      if r < 0
          then failure (Git.OidParseFailed str)
          else return (Oid oid)
  where
    len = S.length str

lgRenderOid :: Git.Oid (LgRepository m) -> Text
lgRenderOid = T.pack . show

instance Show (Git.Oid (LgRepository m)) where
    show (Oid coid) = SU.unsafePerformIO $ withForeignPtr coid oidToStr

instance Ord (Git.Oid (LgRepository m)) where
    Oid coid1 `compare` Oid coid2 =
        SU.unsafePerformIO $
        withForeignPtr coid1 $ \coid1Ptr ->
        withForeignPtr coid2 $ \coid2Ptr -> do
            r <- c'git_oid_cmp coid1Ptr coid2Ptr
            if r < 0
                then return LT
                else if r > 0
                     then return GT
                     else return EQ

instance Eq (Git.Oid (LgRepository m)) where
    oid1 == oid2 = oid1 `compare` oid2 == EQ

lgHashContents :: M m => Git.BlobContents (LgRepository m)
               -> LgRepository m (BlobOid m)
lgHashContents b = do
    repo <- lgGet
    ptr  <- liftIO $ mallocForeignPtr
    r    <- Git.blobContentsToByteString b
            >>= \bs -> liftIO $ withForeignPtr ptr $ \oidPtr ->
            BU.unsafeUseAsCStringLen bs $
            uncurry (\cstr len ->
                      c'git_odb_hash oidPtr (castPtr cstr)
                                     (fromIntegral len) c'GIT_OBJ_BLOB)
    when (r < 0) $ failure Git.BlobCreateFailed
    return (Tagged (Oid ptr))

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the given string is 'empty', it is an error.
lgCreateBlob :: M m
             => Git.BlobContents (LgRepository m)
             -> LgRepository m (BlobOid m)
lgCreateBlob b = do
    repo <- lgGet
    ptr  <- liftIO $ mallocForeignPtr
    r    <- Git.blobContentsToByteString b
            >>= \bs -> liftIO $ createBlobFromByteString repo ptr bs
    when (r < 0) $ failure Git.BlobCreateFailed
    return (Tagged (Oid ptr))

  where
    createBlobFromByteString repo coid bs =
        BU.unsafeUseAsCStringLen bs $
            uncurry (\cstr len ->
                      withForeignPtr coid $ \coid' ->
                      withForeignPtr (repoObj repo) $ \repoPtr ->
                        c'git_blob_create_frombuffer
                          coid' repoPtr (castPtr cstr) (fromIntegral len))

lgLookupBlob :: M m => BlobOid m -> LgRepository m (Git.Blob (LgRepository m))
lgLookupBlob oid =
    lookupObject' (getOid (unTagged oid)) 40
        c'git_blob_lookup c'git_blob_lookup_prefix
        $ \_ obj _ ->
        withForeignPtr obj $ \ptr -> do
            size <- c'git_blob_rawsize (castPtr ptr)
            buf  <- c'git_blob_rawcontent (castPtr ptr)
            -- The lifetime of buf is tied to the lifetime of the blob object
            -- in libgit2, which this Blob object controls, so we can use
            -- unsafePackCStringLen to refer to its bytes.
            bstr <- curry BU.unsafePackCStringLen (castPtr buf)
                          (fromIntegral size)
            return (Git.Blob oid (Git.BlobString bstr))

type TreeEntry m = Git.TreeEntry (LgRepository m)

instance M m => Git.Treeish (Tree m) where
    type TreeRepository (Tree m) = LgRepository m

    modifyTree = lgModifyTree
    writeTree  = lgWriteTree

    traverseEntries t f = go "" t f
      where
        go fp t f = do
            entries <- liftIO $ withForeignPtr (lgTreeContents t) $ \tb -> do
                ior <- newIORef []
                bracket
                    (mk'git_treebuilder_filter_callback (callback fp ior))
                    freeHaskellFunPtr
                    (flip (c'git_treebuilder_filter tb) nullPtr)
                readIORef ior
            concat <$> mapM (uncurry handle) entries

        handle path entry@(Git.TreeEntry tref) = do
            x  <- f path entry
            xs <- Git.resolveTreeRef tref >>= flip (go path) f
            return (x:xs)
        handle path entry = liftM2 (:) (f path entry) (return [])

        callback fp ior te _ = do
            cname <- c'git_tree_entry_name te
            name  <- (fp </>) . F.decodeString <$> peekCString cname
            entry <- entryToTreeEntry te
            modifyIORef ior $
                seq name $ seq entry $ (\xs -> (name,entry):xs)
            return 0

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
lgNewTree :: M m => LgRepository m (Tree m)
lgNewTree = do
    -- size <- liftIO $ newIORef 0

    (r,fptr) <- liftIO $ alloca $ \pptr -> do
        r <- c'git_treebuilder_create pptr nullPtr
        builder <- peek pptr
        fptr <- FC.newForeignPtr builder (c'git_treebuilder_free builder)
        return (r,fptr)

    if r < 0
        then failure (Git.TreeCreateFailed "Failed to create new tree builder")
        else do
        upds <- liftIO $ newIORef HashMap.empty
        info <- liftIO $ newIORef (Base Nothing Nothing)
        return Tree { lgTreeInfo       = info
                    , lgPendingUpdates = upds
                    , lgTreeContents   = fptr }

lgLookupTree :: M m => Int -> Tagged (Tree m) (Oid m) -> LgRepository m (Tree m)
lgLookupTree len oid =
    -- jww (2013-01-28): Verify the oid here
  lookupObject' (getOid (unTagged oid)) len
      c'git_tree_lookup c'git_tree_lookup_prefix $
      \coid obj _ -> do
          withForeignPtr obj $ \objPtr -> do
              -- count <- c'git_tree_entrycount (castPtr objPtr)
              -- size <- newIORef 0
              (r,fptr) <- alloca $ \pptr -> do
                  r <- c'git_treebuilder_create pptr objPtr
                  builder <- peek pptr
                  fptr <- FC.newForeignPtr builder
                              (c'git_treebuilder_free builder)
                  return (r,fptr)
              if r < 0
                  then failure
                       (Git.TreeCreateFailed "Failed to create tree builder")
                  else do
                  upds <- liftIO $ newIORef HashMap.empty
                  info <- liftIO $ newIORef (Base (Just (Tagged (Oid coid)))
                                                 (Just obj))
                  return Tree { lgTreeInfo       = info
                              , lgPendingUpdates = upds
                              , lgTreeContents   = fptr }

entryToTreeEntry :: M m => Ptr C'git_tree_entry -> IO (TreeEntry m)
entryToTreeEntry entry = do
    coid <- c'git_tree_entry_id entry
    oid  <- coidPtrToOid coid
    typ  <- c'git_tree_entry_type entry
    case () of
        () | typ == c'GIT_OBJ_BLOB ->
             do attrs <- c'git_tree_entry_attributes entry
                return $ Git.BlobEntry (Tagged (Oid oid)) $
                    case attrs of
                        0o100644 -> Git.PlainBlob
                        0o100755 -> Git.ExecutableBlob
                        0o120000 -> Git.SymlinkBlob
                        _        -> Git.UnknownBlob
           | typ == c'GIT_OBJ_TREE ->
             return $ Git.TreeEntry (Git.ByOid (Tagged (Oid oid)))
           | typ == c'GIT_OBJ_COMMIT ->
             return $ Git.CommitEntry (Tagged (Oid oid))
           | otherwise -> error "Unexpected"

doLookupTreeEntry :: M m
                  => Tree m
                  -> [Text]
                  -> LgRepository m (Maybe (TreeEntry m))
doLookupTreeEntry t [] = return (Just (Git.treeEntry t))
doLookupTreeEntry t (name:names) = do
  -- Lookup the current name in this tree.  If it doesn't exist, and there are
  -- more names in the path and 'createIfNotExist' is True, create a new Tree
  -- and descend into it.  Otherwise, if it exists we'll have @Just (TreeEntry
  -- {})@, and if not we'll have Nothing.

  upds <- liftIO $ readIORef (lgPendingUpdates t)
  y <- case HashMap.lookup name upds of
      Just m -> return . Just . Git.TreeEntry . Git.Known $ m
      Nothing ->
          liftIO $ withForeignPtr (lgTreeContents t) $ \builder -> do
              entry <- withCStringable name (c'git_treebuilder_get builder)
              if entry == nullPtr
                  then return Nothing
                  else Just <$> entryToTreeEntry entry

  if null names
      then return y
      else case y of
      Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
      Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
      Just (Git.TreeEntry t')   -> do t'' <- Git.resolveTreeRef t'
                                      doLookupTreeEntry t'' names
      _ -> return Nothing

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
lgWriteTree :: M m => Tree m -> LgRepository m (TreeOid m)
lgWriteTree t = doWriteTree t
                >>= maybe (failure Git.TreeBuilderWriteFailed)
                          (return . Tagged)

insertEntry :: (CStringable a, M m)
            => ForeignPtr C'git_treebuilder -> a -> Oid m -> CUInt
            -> LgRepository m ()
insertEntry builder key oid attrs = do
  r2 <- liftIO $ withForeignPtr (getOid oid) $ \coid ->
      withForeignPtr builder $ \ptr ->
      withCStringable key $ \name ->
          c'git_treebuilder_insert nullPtr ptr name coid attrs
  when (r2 < 0) $ failure Git.TreeBuilderInsertFailed

dropEntry :: (CStringable a, Show a, M m)
            => ForeignPtr C'git_treebuilder -> a -> LgRepository m ()
dropEntry builder key = do
  r2 <- liftIO $ withForeignPtr builder $ \ptr ->
      withCStringable key $ \name ->
          c'git_treebuilder_remove ptr name
  when (r2 < 0) $ failure Git.TreeBuilderRemoveFailed

doWriteTree :: M m => Tree m -> LgRepository m (Maybe (Oid m))
doWriteTree t = do
    repo <- lgGet

    let contents = lgTreeContents t
    upds <- liftIO $ readIORef (lgPendingUpdates t)
    mapM_ (\(k,v) -> do
                oid <- doWriteTree v
                case oid of
                    Nothing   -> dropEntry contents k
                    Just oid' -> insertEntry contents k oid' 0o040000)
          (HashMap.toList upds)
    liftIO $ writeIORef (lgPendingUpdates t) HashMap.empty

    cnt <- liftIO $ withForeignPtr contents $ c'git_treebuilder_entrycount
    if cnt == 0
        then return Nothing
        else go contents (repoObj repo)
  where
    go :: M m
       => ForeignPtr C'git_treebuilder
       -> ForeignPtr C'git_repository
       -> LgRepository m (Maybe (Oid m))
    go fptr repo = do
        (r3,coid) <- liftIO $ do
            coid <- mallocForeignPtr
            withForeignPtr coid $ \coid' ->
                withForeignPtr fptr $ \builder ->
                withForeignPtr repo $ \repoPtr -> do
                    r3 <- c'git_treebuilder_write coid' repoPtr builder
                    return (r3,coid)
        when (r3 < 0) $ failure Git.TreeBuilderWriteFailed
        return (Just (Oid coid))

doModifyTree :: M m
             => Tree m
             -> [Text]
             -> Bool
             -> (Maybe (TreeEntry m) -> LgRepository m (Maybe (TreeEntry m)))
             -> LgRepository m (Maybe (TreeEntry m))
doModifyTree t [] _ _ = return . Just . Git.TreeEntry . Git.Known $ t
doModifyTree t (name:names) createIfNotExist f = do
    -- Lookup the current name in this tree.  If it doesn't exist, and there
    -- are more names in the path and 'createIfNotExist' is True, create a new
    -- Tree and descend into it.  Otherwise, if it exists we'll have @Just
    -- (TreeEntry {})@, and if not we'll have Nothing.
    y' <- doLookupTreeEntry t [name]
    y  <- if isNothing y' && createIfNotExist && not (null names)
          then Just . Git.TreeEntry . Git.Known <$> Git.newTree
          else return y'

    if null names
        then do
        -- If there are no further names in the path, call the transformer
        -- function, f.  It receives a @Maybe TreeEntry@ to indicate if there
        -- was a previous entry at this path.  It should return a 'Left' value
        -- to propagate out a user-defined error, or a @Maybe TreeEntry@ to
        -- indicate whether the entry at this path should be deleted or
        -- replaced with something new.
        --
        -- NOTE: There is no provision for leaving the entry unchanged!  It is
        -- assumed to always be changed, as we have no reliable method of
        -- testing object equality that is not O(n).
        ze <- f y
        returnTree t name ze

        else
          -- If there are further names in the path, descend them now.  If
          -- 'createIfNotExist' was False and there is no 'Tree' under the
          -- current name, or if we encountered a 'Blob' when a 'Tree' was
          -- required, throw an exception to avoid colliding with user-defined
          -- 'Left' values.
          case y of
              Nothing -> return Nothing
              Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
              Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
              Just (Git.TreeEntry st')  -> do
                  st <- Git.resolveTreeRef st'
                  ze <- doModifyTree st names createIfNotExist f
                  liftIO $ modifyIORef
                      (lgPendingUpdates t) (HashMap.insert name st)
                  return ze
  where
    returnTree tr n z = do
        let contents = lgTreeContents tr
        case z of
            Nothing -> dropEntry contents n
            Just z' -> do
                (oid,mode) <- treeEntryToOid z'
                case oid of
                    Nothing   -> dropEntry contents n
                    Just oid' -> insertEntry contents n oid' mode
        return z

    treeEntryToOid (Git.BlobEntry oid kind) =
        return (Just (unTagged oid),
                case kind of
                    Git.PlainBlob      -> 0o100644
                    Git.ExecutableBlob -> 0o100755
                    Git.SymlinkBlob    -> 0o120000
                    Git.UnknownBlob    -> 0o100000)
    treeEntryToOid (Git.CommitEntry coid) = do
        return (Just (unTagged coid), 0o160000)
    treeEntryToOid (Git.TreeEntry tr) = do
        oid <- Git.treeRefOid tr
        return (Just (unTagged oid), 0o040000)

lgModifyTree
  :: M m
  => Tree m -> FilePath -> Bool
     -> (Maybe (TreeEntry m) -> LgRepository m (Maybe (TreeEntry m)))
     -> LgRepository m (Maybe (TreeEntry m))
lgModifyTree t path createIfNotExist f =
    doModifyTree t (splitPath path) createIfNotExist f

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

instance M m => Git.Commitish (Commit m) where
    type CommitRepository (Commit m) = LgRepository m
    commitOid       = fromJust . gitId . lgCommitInfo
    commitParents   = lgCommitParents
    commitTree      = lgCommitTree
    commitAuthor    = lgCommitAuthor
    commitCommitter = lgCommitCommitter
    commitLog       = lgCommitLog
    commitEncoding  = T.pack . lgCommitEncoding

instance M m => Git.Treeish (Commit m) where
    type TreeRepository (Commit m) = LgRepository m

    modifyTree      = Git.defaultCommitModifyTree
    writeTree       = Git.defaultCommitWriteTree
    traverseEntries = Git.defaultCommitTraverseEntries

lgLookupCommit :: M m => Int -> CommitOid m -> LgRepository m (Commit m)
lgLookupCommit len oid =
  lookupObject' (getOid (unTagged oid)) len c'git_commit_lookup
                c'git_commit_lookup_prefix $ \coid obj _ ->
      withForeignPtr obj $ \cobj -> do
        let c = castPtr cobj

        enc   <- c'git_commit_message_encoding c
        encs  <- if enc == nullPtr
                then return "UTF-8"
                else peekCString enc
        conv  <- U.open encs (Just False)

        msg   <- c'git_commit_message c   >>= B.packCString
        auth  <- c'git_commit_author c    >>= packSignature conv
        comm  <- c'git_commit_committer c >>= packSignature conv
        toid  <- c'git_commit_tree_oid c
        toid' <- coidPtrToOid toid

        pn    <- c'git_commit_parentcount c
        poids <- sequence
                 (zipWith ($) (replicate (fromIntegral (toInteger pn))
                               (c'git_commit_parent_oid c)) [0..pn])
        poids' <- mapM (\x -> Git.ByOid . Tagged . Oid <$> coidPtrToOid x) poids

        return Commit
            { lgCommitInfo      = Base (Just (Tagged (Oid coid))) (Just obj)
            , lgCommitAuthor    = auth
            , lgCommitCommitter = comm
            , lgCommitTree      = Git.ByOid (Tagged (Oid toid'))
            , lgCommitParents   = poids'
            , lgCommitLog       = U.toUnicode conv msg
            , lgCommitEncoding  = encs
            }

lgLookupObject :: M m => Text -> LgRepository m (Git.Object (LgRepository m))
lgLookupObject str
    | len > 40 = failure (Git.ObjectLookupFailed str len)
    | otherwise = do
        fptr <- liftIO $ do
            fptr <- mallocForeignPtr
            withForeignPtr fptr $ \ptr ->
                withCStringable str $ \cstr -> do
                    r <- if len == 40
                         then c'git_oid_fromstr ptr cstr
                         else c'git_oid_fromstrn ptr cstr (fromIntegral len)
                    return $ if r < 0 then Nothing else Just fptr

        case fptr of
            Nothing -> failure (Git.ObjectLookupFailed str len)
            Just x' ->
                lookupObject' x' len
                  (\x y z -> c'git_object_lookup x y z c'GIT_OBJ_ANY)
                  (\x y z l ->
                    c'git_object_lookup_prefix x y z l c'GIT_OBJ_ANY) go
  where
    len = T.length str

    go coid _ y = do
        typ <- liftIO $ c'git_object_type y
        if typ == c'GIT_OBJ_BLOB
            then ret Git.BlobObj (Oid coid)
            else if typ == c'GIT_OBJ_TREE
                 then ret Git.TreeObj (Oid coid)
                 else if typ == c'GIT_OBJ_COMMIT
                      then ret Git.CommitObj (Oid coid)
                      else if typ == c'GIT_OBJ_TAG
                           then ret Git.TagObj (Oid coid)
                           else failure (Git.ObjectLookupFailed str len)

    ret f = return . f . Git.ByOid . Tagged

lgExistsObject :: M m => Oid m -> LgRepository m Bool
lgExistsObject oid = do
    repo <- lgGet
    result <- liftIO $ withForeignPtr (repoObj repo) $ \repoPtr ->
        alloca $ \pptr -> do
            r0 <- c'git_repository_odb pptr repoPtr
            if r0 < 0
                then return Nothing
                else
                -- jww (2013-02-28): Need to guard against exceptions so that
                -- ptr doesn't leak.
                withForeignPtr (getOid oid) $ \coid -> do
                    ptr <- peek pptr
                    r <- c'git_odb_exists ptr coid
                    c'git_odb_free ptr
                    return (Just (r == 0))
    maybe (failure Git.RepositoryInvalid) return result

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
lgCreateCommit :: M m
               => [CommitRef m]
               -> TreeRef m
               -> Git.Signature
               -> Git.Signature
               -> Text
               -> Maybe Text
               -> LgRepository m (Commit m)
lgCreateCommit parents tree author committer logText ref = do
    repo <- lgGet
    toid <- getOid . unTagged <$> Git.treeRefOid tree
    let pptrs = map Git.commitRefOid parents
    coid  <- liftIO $ withForeignPtr (repoObj repo) $ \repoPtr -> do
        coid <- mallocForeignPtr
        conv <- U.open "utf-8" (Just True)
        withForeignPtr coid $ \coid' ->
            withForeignPtr toid $ \toid' ->
            withForeignPtrs (map (getOid . unTagged) pptrs) $ \pptrs' ->
            B.useAsCString (U.fromUnicode conv logText) $ \message ->
            withRef ref $ \update_ref ->
            withSignature conv author $ \author' ->
            withSignature conv committer $ \committer' ->
            withEncStr "utf-8" $ \message_encoding -> do
                parents' <- newArray pptrs'
                r <- c'git_commit_create_oid coid' repoPtr
                     update_ref author' committer'
                     nullPtr message toid'
                     (fromIntegral (L.length parents)) parents'
                when (r < 0) $ throwIO Git.CommitCreateFailed
                return coid

    return Commit
        { lgCommitInfo      = Base (Just (Tagged (Oid coid))) Nothing
        , lgCommitAuthor    = author
        , lgCommitCommitter = committer
        , lgCommitTree      = tree
        , lgCommitParents   = parents
        , lgCommitLog       = logText
        , lgCommitEncoding  = "utf-8"
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

lgLookupRef :: M m => Text -> LgRepository m (Maybe (Reference m))
lgLookupRef name = do
    repo <- lgGet
    targ <- liftIO $ alloca $ \ptr -> do
        r <- withForeignPtr (repoObj repo) $ \repoPtr ->
              withCStringable name $ \namePtr ->
                c'git_reference_lookup ptr repoPtr namePtr
        if r < 0
            then return Nothing
            else do
            ref  <- peek ptr
            typ  <- c'git_reference_type ref
            targ <- if typ == c'GIT_REF_OID
                    then do oidPtr <- c'git_reference_oid ref
                            Git.RefObj . Git.ByOid . Tagged . Oid
                                <$> coidPtrToOid oidPtr
                    else do targName <- c'git_reference_target ref
                            B.packCString targName
                                >>= return . Git.RefSymbolic . T.decodeUtf8
            c'git_reference_free ref
            return (Just targ)
    for targ $ \targ' ->
        return $ Git.Reference
            { Git.refName   = name
            , Git.refTarget = targ' }

lgUpdateRef :: M m
            => Text -> Git.RefTarget (LgRepository m) (Commit m)
            -> LgRepository m (Reference m)
lgUpdateRef name refTarg = do
    repo <- lgGet
    liftIO $ alloca $ \ptr ->
        withForeignPtr (repoObj repo) $ \repoPtr ->
        withCStringable name $ \namePtr -> do
            r <- case refTarg of
                Git.RefObj (Git.ByOid oid) ->
                    withForeignPtr (getOid (unTagged oid)) $ \coidPtr ->
                        c'git_reference_create_oid ptr repoPtr namePtr
                                                   coidPtr (fromBool True)

                Git.RefObj (Git.Known c) ->
                    withForeignPtr
                        (getOid (unTagged (Git.commitOid c))) $ \coidPtr ->
                        c'git_reference_create_oid ptr repoPtr namePtr
                                                   coidPtr (fromBool True)

                Git.RefSymbolic symName ->
                  withCStringable symName $ \symPtr ->
                    c'git_reference_create_symbolic ptr repoPtr namePtr
                                                    symPtr (fromBool True)
            when (r < 0) $ failure Git.ReferenceCreateFailed

    return Git.Reference { Git.refName   = name
                         , Git.refTarget = refTarg }

-- int git_reference_name_to_oid(git_oid *out, git_repository *repo,
--   const char *name)

lgResolveRef :: M m => Text -> LgRepository m (Maybe (CommitRef m))
lgResolveRef name = do
    repo <- lgGet
    oid <- liftIO $ alloca $ \ptr ->
        withCStringable name $ \namePtr ->
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            r <- c'git_reference_name_to_oid ptr repoPtr namePtr
            if r < 0
                then return Nothing
                else Just <$> coidPtrToOid ptr
    return (Git.ByOid . Tagged . Oid <$> oid)

-- int git_reference_rename(git_reference *ref, const char *new_name,
--   int force)

--renameRef = c'git_reference_rename

lgDeleteRef :: M m
            => Text -> LgRepository m ()
lgDeleteRef name = do
    repo <- lgGet
    r <- liftIO $ alloca $ \ptr ->
        withCStringable name $ \namePtr ->
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
  count <- fromIntegral <$> ( peek $ p'git_strarray'count gitStrs )
  strings <- peek $ p'git_strarray'strings gitStrs

  r0 <- Foreign.Marshal.Array.peekArray count strings
  r1 <- sequence $ fmap peekCString r0
  return $ fmap T.pack r1

flagsToInt :: ListFlags -> CUInt
flagsToInt flags = (if listFlagOid flags      then 1 else 0)
                 + (if listFlagSymbolic flags then 2 else 0)
                 + (if listFlagPacked flags   then 4 else 0)
                 + (if listFlagHasPeel flags  then 8 else 0)

listRefNames :: M m
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

lgAllRefNames :: M m
              => LgRepository m [Text]
lgAllRefNames = listRefNames allRefsFlag

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

lgFactory :: M m => Git.RepositoryFactory (LgRepository m) m Repository
lgFactory = Git.RepositoryFactory
    { Git.openRepository  = openLgRepository
    , Git.runRepository   = runLgRepository
    , Git.closeRepository = closeLgRepository
    , Git.defaultOptions  = defaultLgOptions
    }

openLgRepository :: M m => Git.RepositoryOptions -> m Repository
openLgRepository opts = do
    let path = Git.repoPath opts
    p <- liftIO $ isDirectory path
    if not (Git.repoAutoCreate opts) || p
        then liftIO $ openRepositoryWith path c'git_repository_open
        else liftIO $ openRepositoryWith path
                 (\x y -> c'git_repository_init x y
                              (fromBool (Git.repoIsBare opts)))
  where
    openRepositoryWith path fn = do
        fptr <- alloca $ \ptr ->
            case F.toText path of
                Left p  -> error $ "Repository does not exist: " ++ T.unpack p
                Right p -> withCStringable p $ \str -> do
                    r <- fn ptr str
                    when (r < 0) $
                        error $ "Repository does not exist: " ++ T.unpack p
                    ptr' <- peek ptr
                    newForeignPtr p'git_repository_free ptr'
        return Repository { repoOptions = opts
                          , repoObj     = fptr }

runLgRepository :: Repository -> LgRepository m a -> m a
runLgRepository repo action =
    runReaderT (lgRepositoryReaderT action) repo

closeLgRepository :: M m => Repository -> m ()
closeLgRepository = const (return ())

defaultLgOptions :: Git.RepositoryOptions
defaultLgOptions = Git.RepositoryOptions "" False False

-- Libgit2.hs
