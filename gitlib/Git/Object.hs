{-# LANGUAGE LambdaCase #-}

module Git.Object where

import           Control.Monad
import           Data.Function
import           Data.Maybe
import           Git.DSL
import           Git.Types
import           Prelude hiding (FilePath)
import           Streaming
import qualified Streaming.Prelude as S

listObjects :: Monad m
            => Maybe (CommitOid r)    -- ^ A commit we may already have
            -> CommitOid r            -- ^ The commit we need
            -> Bool                   -- ^ Include commit trees also?
            -> GitT r m [ObjectOid r] -- ^ All the objects in between
listObjects mhave need alsoTrees = S.toList_ =<< allObjects mhave need alsoTrees

traverseObjects :: Monad m
                => (ObjectOid r -> GitT r m a) -> CommitOid r -> GitT r m [a]
traverseObjects f need = mapM f =<< listObjects Nothing need False

traverseObjects_ :: Monad m
                 => (ObjectOid r -> GitT r m ()) -> CommitOid r -> GitT r m ()
traverseObjects_ = (void .) . traverseObjects

-- produce :: Monad m => Producer a m r -> Pipe x a m r
-- produce = loop
--   where
--     loop p = do
--         eres <- lift $ next p
--         case eres of
--             Left r -> return r
--             Right (a, p') -> yield a >> loop p'

-- | Given a list of objects (commit and top-level trees) return by
--   'listObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
expandTreeObjects :: Monad m
                  => Stream (Of (ObjectOid r)) (GitT r m) ()
                  -> Stream (Of (ObjectOid r)) (GitT r m) ()
expandTreeObjects str = S.for str $ \case
    TreeObjOid toid -> do
        S.yield $ TreeObjOid toid
        lift $ runTreeBuilderFromT toid $ do
            ents <- allEntries
            return $ S.for ents $ \case
                (_, BlobEntry oid _) -> S.yield $ BlobObjOid oid
                (_, TreeEntry oid)   -> S.yield $ TreeObjOid oid
                _ -> return ()
    obj -> S.yield obj

listAllObjects :: Monad m
               => Maybe (CommitOid r) -> CommitOid r -> GitT r m [ObjectOid r]
listAllObjects mhave need =
    S.toList_ . expandTreeObjects =<< allObjects mhave need True
