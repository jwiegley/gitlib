module Git.Blob where

import           Control.Applicative
import           Control.Monad
import           Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Text as T
import           Data.Text.Encoding as T
import           Git.DSL
import           Git.Types
import           Pipes
import           Pipes.Safe
import qualified Pipes.ByteString as PB
import qualified Pipes.Prelude as P
import           System.IO

createBlobUtf8 :: Text -> GitT r m (Oid r)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Monad m => Oid r -> GitT r m ByteString
catBlob = lookupBlob >=> blobToByteString

catBlobLazy :: Monad m => Oid r -> GitT r m BL.ByteString
catBlobLazy = lookupBlob >=> blobToLazyByteString

catBlobUtf8 :: Monad m => Oid r -> GitT r m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobContentsToByteString :: Monad m => BlobContents m -> GitT r m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStringLazy bs) =
    return $ B.concat $ BL.toChunks bs
blobContentsToByteString (BlobStream bs) =
    B.concat <$> P.toListM (hoist lift bs)
blobContentsToByteString (BlobSizedStream bs _) =
    B.concat <$> P.toListM (hoist lift bs)

blobToByteString :: Monad m => Blob r m -> GitT r m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

blobContentsToLazyByteString :: Monad m
                             => BlobContents m -> GitT r m BL.ByteString
blobContentsToLazyByteString (BlobString bs) = return $ BL.fromChunks [bs]
blobContentsToLazyByteString (BlobStringLazy bs) = return bs
blobContentsToLazyByteString (BlobStream bs) =
    BL.fromChunks <$> P.toListM (hoist lift bs)
blobContentsToLazyByteString (BlobSizedStream bs _) =
    BL.fromChunks <$> P.toListM (hoist lift bs)

blobToLazyByteString :: Monad m => Blob r m -> GitT r m BL.ByteString
blobToLazyByteString (Blob _ contents) = blobContentsToLazyByteString contents

getBlobContents :: Monad m
                => BlobContents m -> Producer ByteString (GitT r m) ()
getBlobContents (BlobString bs)        = yield bs
getBlobContents (BlobStringLazy bs)    = each (BL.toChunks bs)
getBlobContents (BlobStream bs)        = hoist lift bs
getBlobContents (BlobSizedStream bs _) = hoist lift bs

writeBlob :: (MonadSafe m, MonadIO m)
          => FilePath -> BlobContents m -> GitT r m ()
writeBlob path (BlobString bs)         = liftIO $ B.writeFile path bs
writeBlob path (BlobStringLazy bs)     = liftIO $ BL.writeFile path bs
writeBlob path (BlobStream str)        =
    lift $ bracket (liftIO $ openFile path WriteMode) (liftIO . hClose) $ \h ->
        runEffect $ str >-> PB.toHandle h
writeBlob path (BlobSizedStream str _) = writeBlob path (BlobStream str)

treeBlobEntries :: Monad m
                => Tree r -> GitT r m [(TreeFilePath, Oid r, BlobKind)]
treeBlobEntries = P.toListM . allTreeBlobEntries

allTreeBlobEntries :: Monad m
                   => Tree r
                   -> Producer (TreeFilePath, Oid r, BlobKind) (GitT r m) ()
allTreeBlobEntries tree = for (allTreeEntries tree) go
  where
    go (fp ,BlobEntry oid k) = yield (fp, oid, k)
    go _ = return ()

copyBlob :: (Monad m, Repository r, Repository s)
         => Oid r -> HashSet String -> GitT s (GitT r m) (Oid s, HashSet String)
copyBlob oid needed = do
    let sha = show oid
    oid2 <- parseOid (show oid)
    if HashSet.member sha needed
      then do
        bs   <- lift $ blobToByteString =<< lookupBlob oid
        boid <- createBlob (BlobString bs)

        let x = HashSet.delete sha needed
        return $ boid `seq` x `seq` (boid, x)

      else return (oid2, needed)
