{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Blob
       ( Blob(..)
       , newBlobBase
       , createBlob
       , getBlobContents
       , writeBlob )
       where

import           Data.ByteString as B hiding (map)
import           Data.ByteString.Unsafe
import           Data.Git.Common
import           Data.Git.Errors
import           Data.Git.Internal
import qualified Prelude

default (Text)

data Blob = Blob { blobInfo     :: Base Blob
                 , blobContents :: B.ByteString }

instance Show Blob where
  show x = case gitId (blobInfo x) of
    Pending _ -> "Blob"
    Stored y  -> "Blob#" ++ show y

instance Updatable Blob where
  getId x        = gitId (blobInfo x)
  objectRepo x   = gitRepo (blobInfo x)
  objectPtr x    = gitObj (blobInfo x)
  update         = writeBlob
  lookupFunction = lookupBlob

newBlobBase :: Blob -> Base Blob
newBlobBase b =
  newBase (gitRepo (blobInfo b)) (Pending doWriteBlob) Nothing

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the give string is 'empty', it is an error.
createBlob :: B.ByteString -> Repository -> Blob
createBlob text repo
  | text == B.empty = error "Cannot create an empty blob"
  | otherwise =
    Blob { blobInfo     = newBase repo (Pending doWriteBlob) Nothing
         , blobContents = text }

lookupBlob :: Oid -> Repository -> IO (Maybe Blob)
lookupBlob oid repo =
  lookupObject' oid repo c'git_blob_lookup c'git_blob_lookup_prefix $
    \coid obj _ ->
      return Blob { blobInfo     = newBase repo (Stored coid) (Just obj)
                  , blobContents = B.empty }

getBlobContents :: Blob -> IO (Blob, B.ByteString)
getBlobContents b =
  case gitId (blobInfo b) of
    Pending _   -> return (b, contents)
    Stored hash ->
      if contents /= B.empty
        then return (b, contents)
        else
        case gitObj (blobInfo b) of
          Just blobPtr ->
            withForeignPtr blobPtr $ \ptr -> do
              size <- c'git_blob_rawsize (castPtr ptr)
              buf  <- c'git_blob_rawcontent (castPtr ptr)
              bstr <- curry unsafePackCStringLen (castPtr buf)
                            (fromIntegral size)
              return (b { blobContents = bstr }, bstr)

          Nothing -> do
            b' <- lookupBlob (Oid hash) repo
            case b' of
              Just blobPtr' -> getBlobContents blobPtr'
              Nothing       -> return (b, B.empty)

  where repo     = gitRepo (blobInfo b)
        contents = blobContents b

-- | Write out a blob to its repository.  If it has already been written,
--   nothing will happen.
writeBlob :: Blob -> IO Blob
writeBlob b@(Blob { blobInfo = Base { gitId = Stored _ } }) = return b
writeBlob b = do hash <- doWriteBlob b
                 return b { blobInfo     = (blobInfo b) { gitId = Stored hash }
                          , blobContents = B.empty }

doWriteBlob :: Blob -> IO COid
doWriteBlob b = do
  ptr <- mallocForeignPtr
  r   <- withForeignPtr repo (createFromBuffer ptr)
  when (r < 0) $ throwIO BlobCreateFailed
  return (COid ptr)

  where
    repo = fromMaybe (error "Repository invalid")
                     (repoObj (gitRepo (blobInfo b)))

    createFromBuffer ptr repoPtr =
      unsafeUseAsCStringLen (blobContents b) $
        uncurry (\cstr len ->
                  withForeignPtr ptr $ \ptr' ->
                    c'git_blob_create_frombuffer
                      ptr' repoPtr (castPtr cstr) (fromIntegral len))

-- Blob.hs
