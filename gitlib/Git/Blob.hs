module Git.Blob where

import           Conduit
import           Control.Applicative
import           Control.Monad
import           Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Tagged
import           Data.Text as T
import           Data.Text.Encoding as T
import           Git.Types

createBlobUtf8 :: MonadGit r m => Text -> m (BlobOid r)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: MonadGit r m => BlobOid r -> m ByteString
catBlob = lookupBlob >=> blobToByteString

catBlobLazy :: MonadGit r m => BlobOid r -> m BL.ByteString
catBlobLazy = lookupBlob >=> blobToLazyByteString

catBlobUtf8 :: MonadGit r m => BlobOid r -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobContentsToByteString :: MonadGit r m => BlobContents m -> m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStringLazy bs) =
    return $ B.concat (BL.toChunks bs)
blobContentsToByteString (BlobStream bs) =
    B.concat . BL.toChunks <$> (runConduit $ bs .| sinkLazy)
blobContentsToByteString (BlobSizedStream bs _) =
    runConduit $ B.concat . BL.toChunks <$> (bs .| sinkLazy)

blobToByteString :: MonadGit r m => Blob r m -> m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

blobContentsToLazyByteString :: MonadGit r m
                             => BlobContents m -> m BL.ByteString
blobContentsToLazyByteString (BlobString bs) = return $ BL.fromChunks [bs]
blobContentsToLazyByteString (BlobStringLazy bs) = return bs
blobContentsToLazyByteString (BlobStream bs) = runConduit $ bs .| sinkLazy
blobContentsToLazyByteString (BlobSizedStream bs _) = runConduit $ bs .| sinkLazy

blobToLazyByteString :: MonadGit r m => Blob r m -> m BL.ByteString
blobToLazyByteString (Blob _ contents) = blobContentsToLazyByteString contents

writeBlob :: (MonadGit r m, MonadIO m, MonadResource m)
          => FilePath -> BlobContents m -> m ()
writeBlob path (BlobString bs)         = liftIO $ B.writeFile path bs
writeBlob path (BlobStringLazy bs)     = runConduit $ sourceLazy bs .| sinkFile path
writeBlob path (BlobStream str)        = runConduit $ str .| sinkFile path
writeBlob path (BlobSizedStream str _) = runConduit $ str .| sinkFile path

treeBlobEntries :: MonadGit r m
                => Tree r -> m [(TreeFilePath, BlobOid r, BlobKind)]
treeBlobEntries tree = runConduit $ sourceTreeBlobEntries tree .| sinkList

sourceTreeBlobEntries
    :: MonadGit r m
    => Tree r -> ConduitT i (TreeFilePath, BlobOid r, BlobKind) m ()
sourceTreeBlobEntries tree =
    sourceTreeEntries tree .| awaitForever go
  where
    go (fp ,BlobEntry oid k) = yield (fp, oid, k)
    go _ = return ()

copyBlob :: (MonadGit r m, MonadGit s (t m), MonadTrans t)
         => BlobOid r -> HashSet Text -> t m (BlobOid s, HashSet Text)
copyBlob blobr needed = do
    let oid = untag blobr
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        bs <- lift $ blobToByteString =<< lookupBlob (Tagged oid)
        boid <- createBlob (BlobString bs)

        let x = HashSet.delete sha needed
        return $ boid `seq` x `seq` (boid, x)

        else return (Tagged oid2, needed)
