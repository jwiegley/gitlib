{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Blob
       ( Blob(..), HasBlob(..)
       , createBlob
       , getBlobContents
       , writeBlob
       , writeBlob_ )
       where

import Bindings.Libgit2
import Data.ByteString as B hiding (map)
import Data.ByteString.Unsafe
import Data.Git.Common
import Data.Git.Errors
import Data.Git.Internal
import Data.Text as T hiding (map)
import Prelude hiding (FilePath)

default (Text)

data Blob = Blob { _blobInfo     :: Base Blob
                 , _blobContents :: B.ByteString
                 , _blobObj      :: ObjPtr C'git_object }

makeClassy ''Blob

instance Show Blob where
  show x = case x^.blobInfo.gitId of
    Left _  -> "Blob"
    Right y -> "Blob#" ++ show y

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the give string is 'empty', it is an error.
createBlob :: Repository -> B.ByteString -> Blob
createBlob repo text
  | text == B.empty = error "Cannot create an empty blob"
  | otherwise = Blob { _blobInfo     = newBase repo doWriteBlob
                     , _blobContents = text
                     , _blobObj      = Nothing }

lookupBlob :: Repository -> Oid -> IO (Maybe Blob)
lookupBlob repo oid =
  lookupObject' repo oid
                (\x y z -> c'git_blob_lookup x y z)
                (\x _ ->
                  return Blob { _blobInfo     = newBase'
                              , _blobContents = B.empty
                              , _blobObj      = Just x })
  where
    newBase' = Base { _gitId   = Right oid
                    , _gitRepo = repo }

getBlobContents :: Blob -> IO (Blob, B.ByteString)
getBlobContents b =
  case b^.blobInfo.gitId of
    Left _     -> return $ (b, contents)
    Right hash ->
      if contents /= B.empty
        then return (b, contents)
        else
        case b^.blobObj of
          Just blobPtr ->
            withForeignPtr blobPtr $ \ptr -> do
              size <- c'git_blob_rawsize (castPtr ptr)
              buf  <- c'git_blob_rawcontent (castPtr ptr)
              bstr <- curry unsafePackCStringLen (castPtr buf)
                            (fromIntegral size)
              return (blobContents .~ bstr $ b, bstr)

          Nothing -> do
            b' <- lookupBlob repo hash
            case b' of
              -- Should this be returned back to the user?
              Just blobPtr' -> getBlobContents blobPtr'
              Nothing       -> return (b, B.empty)

  where repo     = b^.blobInfo.gitRepo
        contents = b^.blobContents

-- | Write out a blob to its repository.  If it has already been written,
--   nothing will happen.
writeBlob :: Blob -> IO Blob
writeBlob b@(Blob { _blobInfo = Base { _gitId = Right _ } }) = return b
writeBlob b = do hash <- doWriteBlob b
                 return $ blobInfo.gitId .~ Right hash $
                          blobContents   .~ B.empty    $ b

writeBlob_ :: Blob -> IO ()
writeBlob_ b = void (writeBlob b)

doWriteBlob :: Blob -> IO Oid
doWriteBlob b = do
  ptr <- mallocForeignPtr
  r   <- withForeignPtr repo (createFromBuffer ptr)
  when (r < 0) $ throwIO BlobCreateFailed
  return ptr

  where
    repo = fromMaybe (error "Repository invalid") $
           b^.blobInfo.gitRepo.repoObj

    createFromBuffer ptr repoPtr =
      unsafeUseAsCStringLen (b^.blobContents) $
        uncurry (\cstr len ->
                  withForeignPtr ptr $ \ptr' ->
                    c'git_blob_create_frombuffer
                      ptr' repoPtr (castPtr cstr) (fromIntegral len))

-- Blob.hs
