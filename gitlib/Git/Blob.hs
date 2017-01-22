module Git.Blob where

import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import qualified Data.ByteString.Lazy as BL
import           Git.DSL
import           Git.Types
import           Streaming
import qualified Streaming.Prelude as S

treeBlobEntries :: Monad m
                => Tree r -> GitT r m [(TreeFilePath, BlobOid r, BlobKind)]
treeBlobEntries = S.toList_ . allTreeBlobEntries

allTreeBlobEntries :: Monad m
                   => Tree r
                   -> Stream (Of (TreeFilePath, BlobOid r, BlobKind)) (GitT r m) ()
allTreeBlobEntries tree = S.for (allTreeEntries tree) go
  where
    go (fp ,BlobEntry oid k) = S.yield (fp, oid, k)
    go _ = return ()

copyBlob :: (Monad m, Repository r, Repository s)
         => BlobOid r -> HashSet String -> GitT s (GitT r m) (BlobOid s, HashSet String)
copyBlob oid needed = do
    let sha = show oid
    oid2 <- parseOid (show oid)
    if HashSet.member sha needed
      then do
        Just bss <- lift $ lookupBlob oid
        bs <- lift $ lift $ S.toList_ bss
        boid <- createBlob (BL.fromChunks bs)

        let x = HashSet.delete sha needed
        return $ boid `seq` x `seq` (boid, x)

      else return (oid2, needed)
