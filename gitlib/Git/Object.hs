module Git.Object where

import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Git.DSL
import           Git.Types
import           Pipes
import qualified Pipes.Prelude as P
import           Prelude hiding (FilePath)

listObjects :: Monad m
            => Maybe (CommitOid r)    -- ^ A commit we may already have
            -> CommitOid r            -- ^ The commit we need
            -> Bool                   -- ^ Include commit trees also?
            -> GitT r m [ObjectOid r] -- ^ All the objects in between
listObjects mhave need alsoTrees =
    P.toListM $ allObjects mhave need alsoTrees

traverseObjects :: Monad m
                => (ObjectOid r -> GitT r m a) -> CommitOid r -> GitT r m [a]
traverseObjects f need = mapM f =<< listObjects Nothing need False

traverseObjects_ :: Monad m
                 => (ObjectOid r -> GitT r m ()) -> CommitOid r -> GitT r m ()
traverseObjects_ = (void .) . traverseObjects

produce :: Monad m => Producer a m r -> Pipe x a m r
produce = loop
  where
    loop p = do
        eres <- lift $ next p
        case eres of
            Left r -> return r
            Right (a, p') -> yield a >> loop p'

-- | Given a list of objects (commit and top-level trees) return by
--   'listObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
expandTreeObjects :: Monad m => Pipe (ObjectOid r) (ObjectOid r) (GitT r m) ()
expandTreeObjects = for cat $ \obj -> case obj of
    TreeObjOid toid -> do
        yield $ TreeObjOid toid
        tr <- lift $ lookupTree toid
        produce $ for (allTreeEntries tr) $ \ent -> case ent of
            (_, BlobEntry oid _) -> yield $ BlobObjOid oid
            (_, TreeEntry oid)   -> yield $ TreeObjOid oid
            _ -> return ()
    _ -> yield obj

listAllObjects :: Monad m
               => Maybe (CommitOid r) -> CommitOid r -> GitT r m [ObjectOid r]
listAllObjects mhave need =
    P.toListM $ allObjects mhave need True >-> expandTreeObjects
