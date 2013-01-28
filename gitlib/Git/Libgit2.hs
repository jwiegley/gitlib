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
import           Git.Libgit2.Blob
import           Git.Libgit2.Types
import           Git.Libgit2.Internal
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

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

lgLookupBlob :: Git.Oid -> LgRepository (Git.Blob m)
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

-- Libgit2.hs
