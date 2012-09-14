{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Tree where

import Bindings.Libgit2
import Control.Lens
import Data.Either
import Data.Git.Common
import Data.Git.Foreign
import Data.Git.Blob
import Data.Git.Errors
import Data.Git.Repository
import Data.Map as M hiding (map)
import Data.Maybe
import Data.Text as T hiding (map)
import Filesystem.Path.CurrentOS
import Prelude hiding (FilePath)

default (Text)

type TreeOrBlob = Either Blob Tree
type TreeMap    = Map Text TreeOrBlob

data Tree = Tree { _treeInfo     :: Base Tree
                 , _treeContents :: TreeMap
                 , _treeObj      :: ObjPtr C'git_tree }

makeClassy ''Tree

instance Show Tree where
  show x = case x^.treeInfo^.gitId of
    Left _  -> "Tree"
    Right y -> "Tree#" ++ show y

newTreeBase :: Tree -> Base Tree
newTreeBase t = newBase (t^.treeInfo^.gitRepo) doWriteTree

-- | Create a new tree, starting it with the contents at the given path.
--
--   Note that since empty trees cannot exist in Git, no means is provided for
--   creating one.
createTree :: Repository -> FilePath -> TreeOrBlob -> Tree
createTree repo path item = updateTree path item (emptyTree repo)

doWriteTree :: Tree -> IO Hash
doWriteTree = undefined
{-
  alloca $ \ptr ->
  tb <- c'git_treebuilder_create ptr str
        when (r < 0) $ throwIO (RepositoryNotExist p)
        ptr' <- peek ptr
        let finalizer = newForeignPtr p'git_repository_free ptr'
        return $ Repository { _repoPath = path
                            , _repoObj  = finalizer }
-}

emptyTree :: Repository -> Tree
emptyTree repo = Tree { _treeInfo     = newBase repo doWriteTree
                      , _treeContents = M.empty
                      , _treeObj      = Nothing }

doUpdateTree :: [Text] -> TreeOrBlob -> Tree -> Tree
doUpdateTree (x:xs) item t =
  treeInfo     .~ newTreeBase t $
  treeContents .~ update' xs    $ t

  where repo       = t^.treeInfo^.gitRepo
        treeMap    = t^.treeContents
        update' [] = insert x item treeMap
        update' _  = insert x subTree treeMap
        subTree    = Right $ doUpdateTree xs item tree'
        tree'      = case M.lookup x treeMap of
                       Just (Right m) -> m
                       _ -> emptyTree repo
doUpdateTree [] _ _ = undefined

updateTree :: FilePath -> TreeOrBlob -> Tree -> Tree
updateTree = doUpdateTree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = splitOn "/" text
  where text = case toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

-- Tree.hs
