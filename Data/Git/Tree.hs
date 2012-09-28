{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Git.Tree where

import           Bindings.Libgit2
import           Control.Lens
import           Data.Git.Blob
import           Data.Git.Common
import           Data.Git.Errors
import           Data.Git.Internal
import           Data.List as L
import qualified Data.Map as M
import           Data.Text as T hiding (map)
import           Filesystem.Path.CurrentOS as F hiding ((<.>))
import           Prelude hiding (FilePath, sequence)

default (Text)

data TreeEntry = BlobEntry { blobEntry :: Blob
                           , blobEntryIsExe :: Bool }
               | TreeEntry { treeEntry :: Tree }

-- instance Eq TreeEntry where
--   (BlobEntry x x2) == (BlobEntry y y2) = x == y && x2 == y2
--   (TreeEntry x) == (TreeEntry y) = x == y
--   _ == _ = False

type TreeMap = M.Map Text TreeEntry

data Tree = Tree { _treeInfo     :: Base Tree
                 , _treeContents :: TreeMap }

makeClassy ''Tree

-- instance Eq Tree where
--   x == y = case (x^.treeInfo.gitId, y^.treeInfo.gitId) of
--              (Stored x2, Stored y2) -> x2 == y2
--              _ -> undefined

instance Show Tree where
  show x = case x^.treeInfo.gitId of
    Pending _ -> "Tree"
    Stored y  -> "Tree#" ++ show y

instance Updatable Tree where
  update     = writeTree
  objectId t = case t^.treeInfo.gitId of
    Pending f -> Oid <$> (f t)
    Stored x  -> return $ Oid x

newTreeBase :: Tree -> Base Tree
newTreeBase t =
  newBase (t^.treeInfo.gitRepo)
          (Pending (doWriteTree >=> return . snd)) Nothing

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
createTree :: Repository -> Tree
createTree repo =
  Tree { _treeInfo     =
            newBase repo (Pending (doWriteTree >=> return . snd)) Nothing
       , _treeContents = M.empty }

lookupTree :: Repository -> Oid -> IO (Maybe Tree)
lookupTree repo oid =
  lookupObject' repo oid c'git_tree_lookup c'git_tree_lookup_prefix
    (\coid obj _ ->
      return Tree { _treeInfo =
                       newBase repo (Stored coid) (Just obj)
                  , _treeContents = M.empty })

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTree :: Tree -> IO Tree
writeTree t@(Tree { _treeInfo = Base { _gitId = Stored _ } }) = return t
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

    return (treeInfo.gitId .~ Stored (COid coid) $
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

doModifyTree
  :: [Text] -> (Maybe TreeEntry -> Either a (Maybe TreeEntry)) -> Bool
  -> Tree -> Either a Tree
doModifyTree [] _ _ _     = throw TreeLookupFailed
doModifyTree (name:names) f createIfNotExist t =
  -- Lookup the current name in this tree.  If it doesn't exist, and there are
  -- more names in the path and 'createIfNotExist' is True, create a new Tree
  -- and descend into it.  Otherwise, if it exists we'll have @Just (TreeEntry
  -- {})@, and if not we'll have Nothing.
  let y = case M.lookup name (t^.treeContents) of
            Nothing ->
              if createIfNotExist && not (L.null names)
              then Just $ TreeEntry (createTree (t^.treeInfo.gitRepo))
              else Nothing
            j -> j
  in if L.null names
     then do
       -- If there are no further names in the path, call the transformer
       -- function, f.  It receives a @Maybe TreeEntry@ to indicate if there
       -- was a previous entry at this path.  It should return a 'Left' value
       -- to propagate out a user-defined error, or a @Maybe TreeEntry@ to
       -- indicate whether the entry at this path should be deleted or
       -- replaced with something new.
       --
       -- NOTE: There is no provision for leaving the entry unchanged!  It is
       -- assumed to always be changed, as we have no reliable method of
       -- testing object equality that is not O(n).
       z <- f y
       return $
         treeInfo     .~ newTreeBase t $
         treeContents .~
           (case z of
              Nothing -> M.delete name (t^.treeContents)
              Just z' -> M.insert name z' (t^.treeContents)) $ t
     else
       -- If there are further names in the path, descend them now.  If
       -- 'createIfNotExist' was False and there is no 'Tree' under the
       -- current name, or if we encountered a 'Blob' when a 'Tree' was
       -- required, throw an exception to avoid colliding with user-defined
       -- 'Left' values.
       case y of
         Nothing             -> throw TreeLookupFailed
         Just (BlobEntry {}) -> throw TreeCannotTraverseBlob
         Just (TreeEntry t') -> do
           st <- doModifyTree names f createIfNotExist t'
           return $
             treeInfo     .~ newTreeBase t $
             treeContents .~
               (if M.null (st^.treeContents)
                then M.delete name (t^.treeContents)
                else M.insert name (TreeEntry st) (t^.treeContents)) $ t

modifyTree
  :: FilePath -> (Maybe TreeEntry -> Either a (Maybe TreeEntry)) -> Bool
  -> Tree -> Either a Tree
modifyTree = doModifyTree . splitPath

doUpdateTree :: [Text] -> TreeEntry -> Tree -> Tree
doUpdateTree xs item t =
  case doModifyTree xs (const (Right (Just item))) True t of
    Right tr -> tr
    _ -> undefined

updateTree :: FilePath -> TreeEntry -> Tree -> Tree
updateTree = doUpdateTree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

-- Tree.hs
