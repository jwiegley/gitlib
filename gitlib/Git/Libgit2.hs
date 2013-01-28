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

-- Libgit2.hs
