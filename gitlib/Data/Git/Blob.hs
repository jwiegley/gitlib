{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module Data.Git.Blob
       ( Blob(..)
       , BlobContents(..)
       , ByteSource
       , newBlobBase
       , createBlob
       , getBlobContents
       , blobSourceToString
       , lookupBlob
       , writeBlob )
       where

import           Data.ByteString as B hiding (map)
import           Data.ByteString.Unsafe
import           Data.Conduit
import           Data.Git.Common
import           Data.Git.Error
import           Data.Git.Internal
import qualified Prelude

data BlobContents = BlobEmpty
                  | BlobString B.ByteString
                  | BlobStream ByteSource

instance Eq BlobContents where
  BlobEmpty == BlobEmpty = True
  BlobString str1 == BlobString str2 = str1 == str2
  BlobStream src1 == BlobStream src2 = False
  _ == _ = False

blobSourceToString :: BlobContents -> IO (Maybe B.ByteString)
blobSourceToString BlobEmpty = return Nothing
blobSourceToString (BlobString bs) = return (Just bs)
blobSourceToString (BlobStream bs) = do str <- bs $$ await
                                        case str of
                                          Nothing   -> return Nothing
                                          Just str' -> return (Just str')

data Blob = Blob { blobInfo     :: Base Blob
                 , blobContents :: BlobContents }
          deriving Eq

instance Show Blob where
  show x = case gitId (blobInfo x) of
    Pending _ -> "Blob..."
    Stored y  -> "Blob#" ++ show y

instance Updatable Blob where
  getId          = gitId . blobInfo
  objectRepo     = gitRepo . blobInfo
  objectPtr      = gitObj . blobInfo
  update         = writeBlob
  lookupFunction = lookupBlob
#if defined(PROFILING)
  loadObject' x y =
    maybe (throwIO ObjectLookupFailed) return =<< loadObject x y
#endif

newBlobBase :: Blob -> Base Blob
newBlobBase b = newBase (gitRepo (blobInfo b)) (Pending doWriteBlob) Nothing

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the give string is 'empty', it is an error.
createBlob :: Repository -> B.ByteString -> Blob
createBlob repo text
  | text == B.empty = error "Cannot create an empty blob"
  | otherwise =
    Blob { blobInfo     = newBase repo (Pending doWriteBlob) Nothing
         , blobContents = BlobString text }

lookupBlob :: Repository -> Oid -> IO (Maybe Blob)
lookupBlob repo oid =
  lookupObject' repo oid c'git_blob_lookup c'git_blob_lookup_prefix $
    \coid obj _ ->
      return Blob { blobInfo     = newBase repo (Stored coid) (Just obj)
                  , blobContents = BlobEmpty }

getBlobContents :: Blob -> IO (Blob, BlobContents)
getBlobContents b@(gitId . blobInfo -> Pending _) = return (b, blobContents b)
getBlobContents b@(gitId . blobInfo -> Stored hash)
  | BlobEmpty <- contents =
    case gitObj (blobInfo b) of
      Just blobPtr ->
        withForeignPtr blobPtr $ \ptr -> do
          size <- c'git_blob_rawsize (castPtr ptr)
          buf  <- c'git_blob_rawcontent (castPtr ptr)
          -- The lifetime of buf is tied to the lifetime of the blob object in
          -- libgit2, which this Blob object controls, so we can use
          -- unsafePackCStringLen to refer to its bytes.
          bstr <- curry unsafePackCStringLen (castPtr buf)
                        (fromIntegral size)
          let contents' = BlobString bstr
          return (b { blobContents = contents' }, contents' )

      Nothing -> do
        b' <- lookupBlob repo (Oid hash)
        case b' of
          Just blobPtr' -> getBlobContents blobPtr'
          Nothing       -> return (b, BlobEmpty)

  | otherwise = return (b, contents)

  where repo     = gitRepo (blobInfo b)
        contents = blobContents b

-- | Write out a blob to its repository.  If it has already been written,
--   nothing will happen.
writeBlob :: Blob -> IO Blob
writeBlob b@(Blob { blobInfo = Base { gitId = Stored _ } }) = return b
writeBlob b = do hash <- doWriteBlob b
                 return b { blobInfo     = (blobInfo b) { gitId = Stored hash }
                          , blobContents = BlobEmpty }

-- jww (2012-12-14): Have the write functions return Either instead
doWriteBlob :: Blob -> IO COid
doWriteBlob b = do
  ptr <- mallocForeignPtr
  r   <- withForeignPtr repo (createFromBuffer ptr)
  when (r < 0) $ throwIO BlobCreateFailed
  return (COid ptr)

  where
    repo = fromMaybe (error "Repository invalid")
                     (repoObj (gitRepo (blobInfo b)))

    createFromBuffer ptr repoPtr = do
      str <- blobSourceToString (blobContents b)
      case str of
        Nothing   -> throw BlobCreateFailed
        Just str' -> createBlobFromByteString ptr repoPtr str'

    createBlobFromByteString ptr repoPtr bs =
          unsafeUseAsCStringLen bs $
            uncurry (\cstr len ->
                      withForeignPtr ptr $ \ptr' ->
                        c'git_blob_create_frombuffer
                          ptr' repoPtr (castPtr cstr) (fromIntegral len))

-- Blob.hs
