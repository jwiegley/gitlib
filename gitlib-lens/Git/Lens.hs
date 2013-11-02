{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module Git.Lens where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.ByteString
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Maybe
import           Data.Tagged
import           Data.Text (Text)
import           Git
import           Prelude hiding (FilePath)

{- Examples of use:

>>> withRepository lgFactory "/data/Home/fpco/gitlib" $ "f400bb98128804ddb5628641b88f851d6fcaf52c" ^! _oid._commit._tree._blob "README.md"
Just "gitlib\n======\n\nMain repository for gitlib and related projects. This is a mega-repo.\n"
-}

makeClassyFor "HasCommit" "commit" [ ("commitTree", "_treeOid") ] ''Commit

type RepoGetter m a b =
    forall (p :: * -> * -> *) (f :: * -> *) r.
        (Conjoined p, Effective m r f, Repository m)
        => p b (f b) -> p a (f a)

_oid :: Repository m => RepoGetter m Text (Tagged o (Oid m))
_oid = act parseObjOid

_commit :: Repository m => RepoGetter m (CommitOid m) (Commit m)
_commit = act lookupCommit

_tree :: Repository m => RepoGetter m (Commit m) (Tree m)
_tree = act $ lookupTree . commitTree

_ref :: Repository m => RepoGetter m Text (Maybe (Commit m))
_ref = act $ resolveReference
           >=> maybe (return Nothing) (fmap Just . lookupCommit)

_rtree :: Repository m => RepoGetter m (TreeOid m) (Tree m)
_rtree = act lookupTree

_rblob :: Repository m => RepoGetter m (BlobOid m) (Blob m)
_rblob = act lookupBlob

_entry :: (Conjoined p, Effective m r f, Repository m)
       => Text -> p (Maybe (TreeEntry m)) (f (Maybe (TreeEntry m)))
       -> p (Tree m) (f (Tree m))
_entry n = act $ treeEntry ?? n

_entries :: Repository m => RepoGetter m (Tree m) (HashMap Text (TreeEntry m))
_entries = act $ fmap HashMap.fromList . listTreeEntries

_centries :: Repository m
          => RepoGetter m (Commit m) (HashMap Text (TreeEntry m))
_centries = _tree._entries

_blob :: (Conjoined p, Effective m r f, Repository m)
      => Text -> p (Maybe ByteString) (f (Maybe ByteString))
      -> p (Tree m) (f (Tree m))
_blob n = act (treeEntry ?? n) . act f
  where f (Just (BlobEntry oid _)) = Just <$> catBlob oid
        f _ = return Nothing
