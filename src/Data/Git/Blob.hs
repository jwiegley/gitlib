{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Blob where

import Bindings.Libgit2
import Control.Exception
import Control.Lens
import Control.Monad
import Data.ByteString as B hiding (map)
import Data.ByteString.Unsafe
import Data.Either
import Data.Git.Common
import Data.Git.Errors
import Data.Git.Foreign
import Data.Git.Repository
import Data.Maybe
import Data.Text as T hiding (map)
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (FilePath)
import Unsafe.Coerce

default (Text)

data Blob = Blob { _blobInfo     :: Base Blob
                 , _blobContents :: B.ByteString }

makeClassy ''Blob

instance Show Blob where
  show x = case x^.blobInfo^.gitId of
    Left _  -> "Blob"
    Right y -> "Blob#" ++ show y

newBlobBase :: Blob -> Base Blob
newBlobBase b = newBase (b^.blobInfo^.gitRepo) doWriteBlob

-- | Create a new blob, starting it with the contents at the given path.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one.
createBlob :: Repository -> B.ByteString -> Blob
createBlob repo text = Blob { _blobInfo     = newBase repo doWriteBlob
                            , _blobContents = text }

writeBlob :: Blob -> IO Blob
writeBlob b = do
  hash <- doWriteBlob b
  return Blob { _blobInfo =
                   Base { _gitId   = Right hash
                        , _gitRepo = b^.blobInfo^.gitRepo }
              , _blobContents = B.empty }

doWriteBlob :: Blob -> IO Hash
doWriteBlob b = alloca $ \ptr -> do
  r <- withForeignPtr
         (fromMaybe (error "Repository invalid")
                    (b^.blobInfo^.gitRepo^.repoObj))
         (\repo ->
             unsafeUseAsCStringLen (b^.blobContents) $
               uncurry (\cstr len ->
                         c'git_blob_create_frombuffer
                           ptr repo (unsafeCoerce cstr :: Ptr ())
                           (fromIntegral len)))
  when (r < 0) $ throwIO BlobCreateFailed
  peek ptr

-- Blob.hs
