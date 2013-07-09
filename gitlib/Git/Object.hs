module Git.Object where

import Control.Monad
import Data.Function
import Data.List
import Data.Maybe
import Git.Types
import Prelude hiding (FilePath)

traverseObjects :: Repository m => (ObjectOid m -> m a) -> CommitOid m -> m [a]
traverseObjects f need = mapM f =<< listObjects Nothing need False

traverseObjects_ :: Repository m => (ObjectOid m -> m ()) -> CommitOid m -> m ()
traverseObjects_ = (void .) . traverseObjects

-- | Given a list of objects (commit and top-level trees) return by
--   'listObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
expandTreeObjects :: Repository m => [ObjectOid m] -> m [ObjectOid m]
expandTreeObjects objs =
    fmap concat . forM objs $ \obj -> case obj of
        TreeObjOid toid -> do
            tr <- lookupTree toid
            entries <- listTreeEntries tr
            let subobjss = foldr f [] entries
            return (obj:subobjss)
        _ -> return [obj]
  where
    f (_, ent) rest = case ent of
        BlobEntry oid _ -> BlobObjOid oid : rest
        TreeEntry oid   -> TreeObjOid oid : rest
        _ -> rest

listAllObjects :: Repository m
               => Maybe (CommitOid m) -> CommitOid m -> m [ObjectOid m]
listAllObjects have need = expandTreeObjects =<< listObjects have need True
