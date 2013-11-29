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
import           Git.Classy
import           Prelude hiding (FilePath)

makeClassy_ ''Commit

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
           >=> maybe (return Nothing) (fmap Just . lookupCommit . Tagged)

_rtree :: Repository m => RepoGetter m (TreeOid m) (Tree m)
_rtree = act lookupTree

_rblob :: Repository m => RepoGetter m (BlobOid m) (Blob m)
_rblob = act lookupBlob

_entry :: (Conjoined p, Effective m r f, Repository m)
       => TreeFilePath -> p (Maybe (TreeEntry m)) (f (Maybe (TreeEntry m)))
       -> p (Tree m) (f (Tree m))
_entry n = act $ treeEntry ?? n

_entries :: Repository m
         => RepoGetter m (Tree m) (HashMap TreeFilePath (TreeEntry m))
_entries = act $ fmap HashMap.fromList . listTreeEntries

_centries :: Repository m
          => RepoGetter m (Commit m) (HashMap TreeFilePath (TreeEntry m))
_centries = _tree._entries

_blob :: (Conjoined p, Effective m r f, Repository m)
      => TreeFilePath -> p (Maybe ByteString) (f (Maybe ByteString))
      -> p (Tree m) (f (Tree m))
_blob n = act (treeEntry ?? n) . act f
  where f (Just (BlobEntry oid _)) = Just <$> catBlob oid
        f _ = return Nothing
