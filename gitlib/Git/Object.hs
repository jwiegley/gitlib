module Git.Object where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Function
import           Data.Maybe
import           Git.Types
import           Prelude hiding (FilePath)

listObjects :: Repository m
            => Maybe (CommitOid m) -- ^ A commit we may already have
            -> CommitOid m         -- ^ The commit we need
            -> Bool                -- ^ Include commit trees also?
            -> m [ObjectOid m]     -- ^ All the objects in between
listObjects mhave need alsoTrees =
    sourceObjects mhave need alsoTrees $$ CL.consume

traverseObjects :: Repository m => (ObjectOid m -> m a) -> CommitOid m -> m [a]
traverseObjects f need = mapM f =<< listObjects Nothing need False

traverseObjects_ :: Repository m => (ObjectOid m -> m ()) -> CommitOid m -> m ()
traverseObjects_ = (void .) . traverseObjects

-- | Given a list of objects (commit and top-level trees) return by
--   'listObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
expandTreeObjects :: Repository m => Conduit (ObjectOid m) m (ObjectOid m)
expandTreeObjects = awaitForever $ \obj -> case obj of
    TreeObjOid toid -> do
        yield $ TreeObjOid toid
        tr <- lift $ lookupTree toid
        sourceTreeEntries tr
            =$= awaitForever (\ent -> case ent of
                (_, BlobEntry oid _) -> yield $ BlobObjOid oid
                (_, TreeEntry oid)   -> yield $ TreeObjOid oid
                _ -> return ())
    _ -> yield obj

listAllObjects :: Repository m
               => Maybe (CommitOid m) -> CommitOid m -> m [ObjectOid m]
listAllObjects mhave need =
    sourceObjects mhave need True $= expandTreeObjects $$ CL.consume
