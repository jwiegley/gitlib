module Git.Object where

import Conduit
import Control.Monad
import Data.Function
import Data.Maybe
import Git.Types
import Prelude hiding (FilePath)

listObjects :: MonadGit r m
            => Maybe (CommitOid r) -- ^ A commit we may already have
            -> CommitOid r         -- ^ The commit we need
            -> Bool                -- ^ Include commit trees also?
            -> m [ObjectOid r]     -- ^ All the objects in between
listObjects mhave need alsoTrees =
    sourceObjects mhave need alsoTrees $$ sinkList

traverseObjects :: MonadGit r m => (ObjectOid r -> m a) -> CommitOid r -> m [a]
traverseObjects f need = mapM f =<< listObjects Nothing need False

traverseObjects_ :: MonadGit r m => (ObjectOid r -> m ()) -> CommitOid r -> m ()
traverseObjects_ = (void .) . traverseObjects

-- | Given a list of objects (commit and top-level trees) return by
--   'listObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
expandTreeObjects :: MonadGit r m => Conduit (ObjectOid r) m (ObjectOid r)
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

listAllObjects :: MonadGit r m
               => Maybe (CommitOid r) -> CommitOid r -> m [ObjectOid r]
listAllObjects mhave need =
    sourceObjects mhave need True $= expandTreeObjects $$ sinkList
