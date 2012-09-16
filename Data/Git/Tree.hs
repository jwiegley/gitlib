{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

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
import           Prelude hiding (FilePath, sequence)

default (Text)

data TreeEntry = BlobEntry { blobEntry :: Blob
                           , blobEntryIsExe :: Bool }
               | TreeEntry { treeEntry :: Tree }

type TreeMap = M.Map Text TreeEntry

data Tree = Tree { _treeInfo     :: Base Tree
                 , _treeContents :: TreeMap }

makeClassy ''Tree

instance Show Tree where
  show x = case x^.treeInfo.gitId of
    Left _  -> "Tree"
    Right y -> "Tree#" ++ show y

instance Updatable Tree where
  update     = writeTree
  objectId t = case t^.treeInfo.gitId of
    Left f  -> Oid <$> (f t)
    Right x -> return $ Oid x

newTreeBase :: Tree -> Base Tree
newTreeBase t =
  newBase (t^.treeInfo.gitRepo) (Left (doWriteTree >=> return . snd)) Nothing

-- | Create a new tree, starting it with the contents at the given path.
--
--   Note that since empty trees cannot exist in Git, no means is provided for
--   creating one.
createTree :: Repository -> FilePath -> TreeEntry -> Tree
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
writeTree t = fst <$> doWriteTree t

doWriteTree :: Tree -> IO (Tree, COid)
doWriteTree t = alloca $ \ptr ->
  withForeignPtr repo $ \repoPtr -> do
    r <- c'git_treebuilder_create ptr nullPtr
    when (r < 0) $ throwIO TreeBuilderCreateFailed
    builder <- peek ptr

    newList <-
      for (M.toList (t^.treeContents)) $ \(k, v) -> do
        newObj <-
          case v of
            BlobEntry bl exe ->
              flip BlobEntry exe <$>
                insertObject builder k bl (if exe
                                           then 0o100755
                                           else 0o100644)
            TreeEntry tr ->
              TreeEntry <$> insertObject builder k tr 0o040000
        return (k, newObj)

    coid <- mallocForeignPtr
    withForeignPtr coid $ \coid' -> do
      r3 <- c'git_treebuilder_write coid' repoPtr builder
      when (r3 < 0) $ throwIO TreeBuilderWriteFailed

    return (treeInfo.gitId .~ Right (COid coid) $
            treeContents   .~ M.fromList newList $ t, COid coid)

  where
    repo = fromMaybe (error "Repository invalid") $
                     t^.treeInfo.gitRepo.repoObj

    insertObject :: (CStringable a, Updatable b)
                 => Ptr C'git_treebuilder -> a -> b -> CUInt -> IO b
    insertObject builder key obj attrs = do
      obj'            <- update obj
      Oid (COid coid) <- objectId obj'

      withForeignPtr coid $ \coid' ->
        withCStringable key $ \name -> do
          r2 <- c'git_treebuilder_insert nullPtr builder name coid' attrs
          when (r2 < 0) $ throwIO TreeBuilderInsertFailed

      return obj'

emptyTree :: Repository -> Tree
emptyTree repo =
  Tree { _treeInfo     =
            newBase repo (Left (doWriteTree >=> return . snd)) Nothing
       , _treeContents = M.empty }

doUpdateTree :: [Text] -> TreeEntry -> Tree -> Tree
doUpdateTree (x:xs) item t =
  treeInfo     .~ newTreeBase t $
  treeContents .~ update' xs    $ t

  where repo       = t^.treeInfo.gitRepo
        treeMap    = t^.treeContents
        update' [] = M.insert x item treeMap
        update' _  = M.insert x subTree treeMap
        subTree    = TreeEntry $ doUpdateTree xs item tree'
        tree'      = case M.lookup x treeMap of
                       Just (TreeEntry m) -> m
                       _ -> emptyTree repo
doUpdateTree [] _ _ = undefined

updateTree :: FilePath -> TreeEntry -> Tree -> Tree
updateTree = doUpdateTree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

-- Tree.hs
