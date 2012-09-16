{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Tree where

import           Bindings.Libgit2
import           Control.Lens
import           Data.Either
import           Data.Git.Blob
import           Data.Git.Common
import           Data.Git.Errors
import           Data.Git.Internal
import qualified Data.Map as M hiding (map)
import           Data.Text as T hiding (map)
import           Filesystem.Path.CurrentOS as F
import           Prelude hiding (FilePath)

default (Text)

type TreeOrBlob = Either Blob Tree
type TreeMap    = M.Map Text TreeOrBlob

data Tree = Tree { _treeInfo     :: Base Tree
                 , _treeContents :: TreeMap }

makeClassy ''Tree

instance Show Tree where
  show x = case x^.treeInfo.gitId of
    Left _  -> "Tree"
    Right y -> "Tree#" ++ show y

instance Updatable Tree where
  update     = writeTree
  --objectId t = return $ t^.treeInfo.gitId
  objectId t = undefined

newTreeBase :: Tree -> Base Tree
newTreeBase t = newBase (t^.treeInfo.gitRepo) (Left doWriteTree) Nothing

-- | Create a new tree, starting it with the contents at the given path.
--
--   Note that since empty trees cannot exist in Git, no means is provided for
--   creating one.
createTree :: Repository -> FilePath -> TreeOrBlob -> Tree
createTree repo path item = updateTree path item (emptyTree repo)

lookupTree :: Repository -> Oid -> IO (Maybe Tree)
lookupTree repo oid =
  lookupObject' repo oid c'git_tree_lookup c'git_tree_lookup_prefix
    (\coid obj _ ->
      return Tree { _treeInfo =
                       newBase repo (Right coid) (Just obj)
                  , _treeContents = M.empty })

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTree :: Tree -> IO Tree
writeTree t@(Tree { _treeInfo = Base { _gitId = Right _ } }) = return t
writeTree t = do hash <- doWriteTree t
                 return $ treeInfo.gitId .~ Right hash $ t

doWriteTree :: Tree -> IO COid
doWriteTree t = do
  ptr <- mallocForeignPtr
  r   <- withForeignPtr repo (createFromTreeMap ptr)
  -- when (r < 0) $ throwIO TreeCreateFailed
  return (COid ptr)

  where repo = fromMaybe (error "Repository invalid") $
                         t^.treeInfo.gitRepo.repoObj

createFromTreeMap t repoPtr = alloca $ \ptr -> do
  r <- c'git_treebuilder_create ptr nullPtr
  when (r < 0) $ throwIO TreeBuilderCreateFailed
  builder <- peek ptr
  return undefined
{-
  for (M.toList (t^.treeContents)) $ \k v -> do
    v'  <- update v               -- make sure the object is updated
    oid <- objectId v'
    withCStringable k $ \name ->
      c'git_treebuilder_insert nullPtr builder name oid 0
-}

emptyTree :: Repository -> Tree
emptyTree repo =
  Tree { _treeInfo     = newBase repo (Left doWriteTree) Nothing
       , _treeContents = M.empty }

doUpdateTree :: [Text] -> TreeOrBlob -> Tree -> Tree
doUpdateTree (x:xs) item t =
  treeInfo     .~ newTreeBase t $
  treeContents .~ update' xs    $ t

  where repo       = t^.treeInfo.gitRepo
        treeMap    = t^.treeContents
        update' [] = M.insert x item treeMap
        update' _  = M.insert x subTree treeMap
        subTree    = Right $ doUpdateTree xs item tree'
        tree'      = case M.lookup x treeMap of
                       Just (Right m) -> m
                       _ -> emptyTree repo
doUpdateTree [] _ _ = undefined

updateTree :: FilePath -> TreeOrBlob -> Tree -> Tree
updateTree = doUpdateTree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

-- Tree.hs
