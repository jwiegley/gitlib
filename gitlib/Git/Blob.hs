module Git.Blob where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Tagged
import           Data.Text as T
import           Data.Text.Encoding as T
import           Git.Types

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Repository m => BlobOid m -> m ByteString
catBlob = lookupBlob >=> blobToByteString

catBlobUtf8 :: Repository m => BlobOid m -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobContentsToByteString :: Repository m => BlobContents m -> m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStream bs) =
    B.concat <$> (bs $$ CL.consume)
blobContentsToByteString (BlobSizedStream bs _) =
    B.concat <$> (bs $$ CL.consume)

blobToByteString :: Repository m => Blob m -> m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

treeBlobEntries :: Repository m
                => Tree m -> m [(TreeFilePath, BlobOid m, BlobKind)]
treeBlobEntries tree = sourceTreeBlobEntries tree $$ CL.consume

sourceTreeBlobEntries
    :: Repository m
    => Tree m -> Producer m (TreeFilePath, BlobOid m, BlobKind)
sourceTreeBlobEntries tree =
    sourceTreeEntries tree =$= awaitForever go
  where
    go (fp ,BlobEntry oid k) = yield (fp, oid, k)
    go _ = return ()

copyBlob :: (Repository m, Repository (t m), MonadTrans t)
         => BlobOid m
         -> HashSet Text
         -> t m (BlobOid (t m), HashSet Text)
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
