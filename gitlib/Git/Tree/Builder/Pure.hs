{-# LANGUAGE TupleSections #-}

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
newPureTreeBuilder :: Monad m
                   => (Tree r -> GitT r m (EntryHashMap r))
                   -> (EntryHashMap r -> GitT r m (TreeOid r))
                   -> Maybe (Tree r)
                   -> GitT r m (TreeBuilder r m)
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

makePureBuilder :: Monad m
                => Maybe (TreeOid r)
                -> HashMap TreeFilePath (TreeBuilder r m)
                -> (Maybe (Tree r) -> GitT r m (TreeBuilder r m))
                -> EntryHashMap r
                -> (EntryHashMap r -> GitT r m (TreeOid r))
                -> TreeBuilder r m
makePureBuilder baseTree upds newBuilder entMap writer = TreeBuilder
    { mtbTreeOid    = baseTree
    , mtbUpdates    = upds
    , mtbNewBuilder = newBuilder

    , mtbWriteContents  = \tb -> (BuilderUnchanged tb,) <$> writer entMap

    , mtbLookupEntry = \key -> return $ HashMap.lookup key entMap
    , mtbEntryCount = return $ HashMap.size entMap

    , mtbPutEntry = \tb key ent ->
        return . ModifiedBuilder $
            makePureBuilder
                baseTree
                (mtbUpdates tb)
                newBuilder
                (HashMap.insert key ent entMap)
                writer

    , mtbDropEntry = \tb key ->
        return . ModifiedBuilder $
            makePureBuilder
                baseTree
                (mtbUpdates tb)
                newBuilder
                (HashMap.delete key entMap)
                writer
    }
