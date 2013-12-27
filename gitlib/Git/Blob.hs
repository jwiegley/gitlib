module Git.Blob where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Conduit.Binary as CB
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
    B.concat <$> (bs $$ CL.consume)
blobContentsToByteString (BlobSizedStream bs _) =
    B.concat <$> (bs $$ CL.consume)

blobToByteString :: MonadGit r m => Blob r m -> m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

blobContentsToLazyByteString :: MonadGit r m
                             => BlobContents m -> m BL.ByteString
blobContentsToLazyByteString (BlobString bs) = return $ BL.fromChunks [bs]
blobContentsToLazyByteString (BlobStringLazy bs) = return bs
blobContentsToLazyByteString (BlobStream bs) = bs $$ CB.sinkLbs
blobContentsToLazyByteString (BlobSizedStream bs _) = bs $$ CB.sinkLbs

blobToLazyByteString :: MonadGit r m => Blob r m -> m BL.ByteString
blobToLazyByteString (Blob _ contents) = blobContentsToLazyByteString contents

writeBlob :: (MonadGit r m, MonadIO m, MonadResource m)
          => FilePath -> BlobContents m -> m ()
writeBlob path (BlobString bs)         = liftIO $ B.writeFile path bs
writeBlob path (BlobStringLazy bs)     = CB.sourceLbs bs $$ CB.sinkFile path
writeBlob path (BlobStream str)        = str $$ CB.sinkFile path
writeBlob path (BlobSizedStream str _) = str $$ CB.sinkFile path

treeBlobEntries :: MonadGit r m
                => Tree r -> m [(TreeFilePath, BlobOid r, BlobKind)]
treeBlobEntries tree = sourceTreeBlobEntries tree $$ CL.consume

sourceTreeBlobEntries
    :: MonadGit r m
    => Tree r -> Producer m (TreeFilePath, BlobOid r, BlobKind)
sourceTreeBlobEntries tree =
    sourceTreeEntries tree =$= awaitForever go
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
