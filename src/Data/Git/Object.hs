{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Object
       ( Object(..)
       , stringToOid
       , lookupObject )
       where

import Data.ByteString as B hiding (map)
import Data.Git.Blob
import Data.Git.Commit
import Data.Git.Internal
import Data.Git.Tag
import Data.Git.Tree
import qualified Data.Map as M
import Data.Text as T hiding (map)
import Prelude hiding (FilePath)

default (Text)

data Object = BlobObj   Blob
            | TreeObj   Tree
            | CommitObj Commit
            | TagObj    Tag

stringToOid :: String -> IO (Maybe Oid)
stringToOid str = do
  oid <- mallocForeignPtr
  withCString str $ \cstr ->
    withForeignPtr oid $ \ptr -> do
      r <- c'git_oid_fromstr ptr cstr
      if r < 0
        then return Nothing
        else return (Just oid)

lookupObject :: Repository -> Oid -> IO (Maybe Object)
lookupObject repo oid =
  lookupObject' repo oid
                (\x y z -> c'git_object_lookup x y z c'GIT_OBJ_ANY)
                (\x y   -> c'git_object_type y >>= createObject' x)
  where
    newBase' = Base { _gitId   = Right oid
                    , _gitRepo = repo }

    createObject' fptr typ
      | typ == c'GIT_OBJ_BLOB =
        return $ BlobObj Blob { _blobInfo     = newBase'
                              , _blobContents = B.empty
                              , _blobObj      = Just fptr }

      | typ == c'GIT_OBJ_TREE =
        return $ TreeObj Tree { _treeInfo     = newBase'
                              , _treeContents = M.empty
                              , _treeObj      = Just fptr }

      | otherwise = return undefined

-- Object.hs
