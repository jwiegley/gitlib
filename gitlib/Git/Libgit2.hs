{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Interface for opening and creating repositories.  Repository objects are
--   immutable, and serve only to refer to the given repository.  Any data
--   associated with the repository — such as the list of branches — is
--   queried as needed.
module Git.Libgit2
       ( LgRepository(..)
       , withLgRepository
       , withOpenLgRepository
       , openLgRepository
       , createLgRepository
       , openOrCreateLgRepository
       ) where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.ByteString
import           Data.ByteString.Unsafe
import           Data.Monoid
import           Data.Stringable
import           Data.Tagged
import           Data.Text as T
import           Filesystem
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git as Git
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

data Repository = Repository
    { repoPath :: FilePath
    , repoObj  :: ForeignPtr C'git_repository
    }

instance Eq Repository where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

instance Show Repository where
  show x = "Repository " <> toString (repoPath x)

newtype LgRepository a = LgRepository
    { runLgRepository :: StateT Repository IO a }

instance Functor LgRepository where
    fmap f (LgRepository x) = LgRepository (fmap f x)

instance Applicative LgRepository where
    pure = LgRepository . pure
    LgRepository f <*> LgRepository x = LgRepository (f <*> x)

instance Monad LgRepository where
    return = LgRepository . return
    LgRepository m >>= f = LgRepository (m >>= runLgRepository . f)

instance MonadIO LgRepository where
    liftIO m = LgRepository (liftIO m)

instance Failure Git.Exception LgRepository where
    failure e = liftIO $ throwIO e

lgPut = LgRepository . put
lgGet = LgRepository get

instance Git.Repository LgRepository where
    -- lookupRef :: Text -> m Reference
    lookupRef = undefined
    -- updateRef :: Text -> Reference -> m Reference
    updateRef = undefined
    -- traverseRefs :: Traversable t => (Reference -> m b) -> m (t b)
    traverseRefs = undefined
    -- lookupCommit :: Oid -> m Commit
    lookupCommit = undefined
    -- lookupTree :: Oid -> m Tree
    lookupTree = undefined
    -- lookupBlob :: Oid -> m (Blob m)
    lookupBlob = lgLookupBlob
    -- lookupTag :: Oid -> m Tag
    lookupTag = undefined
    -- newTree :: m Tree
    newTree = undefined
    -- createBlob :: BlobContents m -> m (BlobOid m)
    createBlob = lgCreateBlob
    -- createCommit :: [ObjRef Commit] -> ObjRef Tree -> Signature -> Signature
    --                 -> Text -> m c
    createCommit = undefined

withOpenLgRepository :: Repository -> LgRepository a -> IO a
withOpenLgRepository repo action =
    evalStateT (runLgRepository action) repo

withLgRepository :: FilePath -> Bool -> LgRepository a -> IO a
withLgRepository path bare action = do
    repo <- openOrCreateLgRepository path bare
    withOpenLgRepository repo action

openLgRepository :: FilePath -> IO Repository
openLgRepository path =
  openRepositoryWith path c'git_repository_open

createLgRepository :: FilePath -> Bool -> IO Repository
createLgRepository path bare =
  openRepositoryWith path (\x y -> c'git_repository_init x y (fromBool bare))

openOrCreateLgRepository :: FilePath -> Bool -> IO Repository
openOrCreateLgRepository path bare = do
  p <- liftIO $ isDirectory path
  if p
    then openLgRepository path
    else createLgRepository path bare

openRepositoryWith :: FilePath
                   -> (Ptr (Ptr C'git_repository) -> CString -> IO CInt)
                   -> IO Repository
openRepositoryWith path fn = do
    fptr <- liftIO $ alloca $ \ptr ->
        case F.toText path of
            Left p  -> error $ "Repository does not exist: " ++ T.unpack p
            Right p -> withCStringable p $ \str -> do
                r <- fn ptr str
                when (r < 0) $
                    error $ "Repository does not exist: " ++ T.unpack p
                ptr' <- peek ptr
                newForeignPtr p'git_repository_free ptr'
    return Repository { repoPath = path
                      , repoObj  = fptr }

type ObjPtr a = Maybe (ForeignPtr a)

data Base = Base { gitId  :: Maybe COid
                 , gitObj :: ObjPtr C'git_object }
          deriving Eq

instance Show Base where
  show x = case gitId x of
             Nothing -> "Base..."
             Just y  -> "Base#" ++ show y

-- | 'COid' is a type wrapper for a foreign pointer to libgit2's 'git_oid'
--   structure.  Users should not have to deal with this type.
type COid = ForeignPtr C'git_oid

coidToOid :: COid -> Git.Oid
coidToOid coid =
    Git.Oid (unsafePerformIO $ withForeignPtr coid $ packCString . castPtr)

coidPtrToOid :: Ptr C'git_oid -> Git.Oid
coidPtrToOid coidptr =
    Git.Oid (unsafePerformIO (packCString (castPtr coidptr)))

oidToCoid :: Git.Oid -> COid
oidToCoid (Git.Oid oid) = unsafePerformIO $ do
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr ->
        useAsCString oid (c'git_oid_fromraw ptr . castPtr)
    return fptr

lookupObject'
  :: Git.Oid
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> CUInt -> IO CInt)
  -> (COid -> ForeignPtr C'git_object -> Ptr C'git_object -> IO b)
  -> LgRepository b
lookupObject' oid lookupFn lookupPrefixFn createFn = do
    repo <- lgGet
    liftIO $ alloca $ \ptr -> do
      r <- withForeignPtr (repoObj repo) $ \repoPtr ->
          withForeignPtr (oidToCoid oid) $ \oidPtr ->
              lookupFn ptr repoPtr oidPtr
               -- PartialOid (COid oid') len ->
               --   withForeignPtr oid' $ \oidPtr ->
               --     lookupPrefixFn ptr repoPtr oidPtr (fromIntegral len)
      if r < 0
        then error "lookupObject' failed"
        else do
        ptr'     <- castPtr <$> peek ptr
        coid     <- c'git_object_id ptr'
        coidCopy <- mallocForeignPtr
        withForeignPtr coidCopy $ flip c'git_oid_cpy coid

        fptr <- newForeignPtr p'git_object_free ptr'
        createFn coidCopy fptr ptr'

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the give string is 'empty', it is an error.
lgCreateBlob ::
    Git.BlobContents LgRepository -> LgRepository (Git.BlobOid LgRepository)
lgCreateBlob b = do
    repo <- lgGet
    ptr  <- liftIO $ mallocForeignPtr
    r    <- Git.blobContentsToByteString b
            >>= \bs -> liftIO $ createBlobFromByteString repo ptr bs
    when (r < 0) $ failure Git.BlobCreateFailed
    return (Tagged (coidToOid ptr))

  where
    createBlobFromByteString repo coid bs =
        unsafeUseAsCStringLen bs $
            uncurry (\cstr len ->
                      withForeignPtr coid $ \coid' ->
                      withForeignPtr (repoObj repo) $ \repoPtr ->
                        c'git_blob_create_frombuffer
                          coid' repoPtr (castPtr cstr) (fromIntegral len))

lgLookupBlob :: Git.Oid -> LgRepository (Git.Blob LgRepository)
lgLookupBlob oid =
    lookupObject' oid c'git_blob_lookup c'git_blob_lookup_prefix
    $ \coid obj _ ->
    withForeignPtr obj $ \ptr -> do
        size <- c'git_blob_rawsize (castPtr ptr)
        buf  <- c'git_blob_rawcontent (castPtr ptr)
        -- The lifetime of buf is tied to the lifetime of the blob object in
        -- libgit2, which this Blob object controls, so we can use
        -- unsafePackCStringLen to refer to its bytes.
        bstr <- curry unsafePackCStringLen (castPtr buf)
                      (fromIntegral size)
        return (Git.Blob (Git.BlobString bstr))

-- Types.hs
