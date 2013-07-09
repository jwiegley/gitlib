module Git.Tree.Builder.Pure
       ( EntryHashMap
       , newPureTreeBuilder
       ) where

import           Control.Applicative
import           Data.Monoid
import           Data.Text
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Git

type EntryHashMap m = HashMap Text (TreeEntry m)

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
newPureTreeBuilder :: Repository m
                   => (Tree m -> m (EntryHashMap m))
                   -> (EntryHashMap m -> m (TreeOid m))
                   -> Maybe (Tree m)
                   -> m (TreeBuilder m)
newPureTreeBuilder reader writer mtree = do
    entMap <- case mtree of
        Nothing   -> return HashMap.empty
        Just tree -> reader tree
    return $ makePureBuilder
        (treeOid <$> mtree)
        mempty
        (newPureTreeBuilder reader writer)
        entMap
        writer

makePureBuilder :: Repository m
                => Maybe (TreeOid m)
                -> HashMap Text (TreeBuilder m)
                -> (Maybe (Tree m) -> m (TreeBuilder m))
                -> EntryHashMap m
                -> (EntryHashMap m -> m (TreeOid m))
                -> TreeBuilder m
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
