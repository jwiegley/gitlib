{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Object
       ( Object(..)
       , ObjectType(..)
       , NamedRef(..)
       , revParse
       , lookupObject )
       where

import           Data.ByteString as B hiding (map)
import           Data.Git.Blob
import           Data.Git.Commit
import           Data.Git.Internal
import           Data.Git.Reference
import           Data.Git.Tag
import           Data.Git.Tree
import qualified Data.Map as M

data ObjectType = BlobType | TreeType | CommitType | TagType

data Object = BlobObj   Blob
            | TreeObj   Tree
            | CommitObj Commit
            | TagObj    Tag

data NamedRef a = FullHash a
                | PartialHash a
                | BranchName a
                | TagName a
                | RefName a
                | FullRefName a
                | Specifier a

revParse :: CStringable a => NamedRef a -> IO (Maybe Oid)
revParse (FullHash r)    = parseOid r
revParse (PartialHash r) = parseOid r
revParse (BranchName r)  = undefined
revParse (TagName r)     = undefined
revParse (RefName r)     = undefined
revParse (FullRefName r) = undefined
revParse (Specifier r)   = undefined

lookupObject :: Repository -> Oid -> IO (Maybe Object)
lookupObject repo oid =
  lookupObject' repo oid
    (\x y z    -> c'git_object_lookup x y z c'GIT_OBJ_ANY)
    (\x y z l  -> c'git_object_lookup_prefix x y z l c'GIT_OBJ_ANY)
    (\coid x y -> c'git_object_type y >>= createObject repo coid x)

lookupRefObject :: Repository -> Text -> IO (Maybe Object)
lookupRefObject repo ref = do
  oid <- resolveRef repo ref
  maybe (return Nothing) (lookupObject repo) oid

createObject :: Repository -> COid -> ForeignPtr C'git_object -> C'git_otype
             -> IO Object
createObject repo coid obj typ
  | typ == c'GIT_OBJ_BLOB =
    return $ BlobObj Blob { blobInfo =
                              newBase repo (Stored coid) (Just obj)
                          , blobContents = BlobEmpty }

  | typ == c'GIT_OBJ_TREE =
    return $ TreeObj Tree { treeInfo =
                              newBase repo (Stored coid) (Just obj)
                          , treeContents = M.empty }

  | otherwise = return undefined

-- Object.hs
