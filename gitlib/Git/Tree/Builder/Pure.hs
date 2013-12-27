module Git.Tree.Builder.Pure
       ( EntryHashMap
       , newPureTreeBuilder
       ) where

import           Control.Applicative
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Monoid
import           Data.Traversable
import           Git
import           Prelude hiding (mapM)

type EntryHashMap r = HashMap TreeFilePath (TreeEntry r)

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
newPureTreeBuilder :: MonadGit r m
                   => (Tree r -> m (EntryHashMap r))
                   -> (EntryHashMap r -> m (TreeOid r))
                   -> Maybe (Tree r)
                   -> m (TreeBuilder r m)
newPureTreeBuilder reader writer mtree = do
    entMap <- case mtree of
        Nothing   -> return HashMap.empty
        Just tree -> reader tree
    toid <- mapM treeOid mtree
    return $ makePureBuilder
        toid
        mempty
        (newPureTreeBuilder reader writer)
        entMap
        writer

makePureBuilder :: MonadGit r m
                => Maybe (TreeOid r)
                -> HashMap TreeFilePath (TreeBuilder r m)
                -> (Maybe (Tree r) -> m (TreeBuilder r m))
                -> EntryHashMap r
                -> (EntryHashMap r -> m (TreeOid r))
                -> TreeBuilder r m
makePureBuilder baseTree upds newBuilder entMap writer = TreeBuilder
    { mtbBaseTreeOid    = baseTree
    , mtbPendingUpdates = upds
    , mtbNewBuilder     = newBuilder

    , mtbWriteContents  = \tb -> (,) <$> pure (BuilderUnchanged tb)
                                     <*> writer entMap

    , mtbLookupEntry = \key -> return $ HashMap.lookup key entMap
    , mtbEntryCount = return $ HashMap.size entMap

    , mtbPutEntry = \tb key ent ->
        return . ModifiedBuilder $
            makePureBuilder
                baseTree
                (mtbPendingUpdates tb)
                newBuilder
                (HashMap.insert key ent entMap)
                writer

    , mtbDropEntry = \tb key ->
        return . ModifiedBuilder $
            makePureBuilder
                baseTree
                (mtbPendingUpdates tb)
                newBuilder
                (HashMap.delete key entMap)
                writer
    }
