{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Git.Sample
       ( SampleRepository(..), Repository(..)
       , Git.Oid(..), BlobOid(..), TreeOid(..), CommitOid(..)
       , Tree(..), Commit(..)
       , TreeRef(..), CommitRef(..), Reference(..)
       , sampleFactory
       , sampleGet
       ) where

import           Control.Applicative
import           Control.Failure
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Tagged
import           Filesystem.Path.CurrentOS as F
import           Prelude hiding (FilePath)
import qualified Git as Git

data Void

type M m = (Failure Git.GitException m, MonadIO m, Applicative m)

instance M m => Git.RepositoryBase (SampleRepository m) where
    data Oid (SampleRepository m)     = Oid Void
    data Tree (SampleRepository m)    = Tree Void
    data Commit (SampleRepository m)  = Commit Void
    data Tag (SampleRepository m)     = Tag Void
    data Options (SampleRepository m) = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    parseOid     = undefined
    renderOid    = undefined
    lookupRef    = undefined
    createRef    = undefined
    updateRef    = undefined
    deleteRef    = undefined
    resolveRef   = undefined
    pushRef      = undefined
    allRefNames  = undefined
    lookupCommit = undefined
    lookupTree   = undefined
    lookupBlob   = undefined
    lookupTag    = undefined
    lookupObject = undefined
    existsObject = undefined
    newTree      = undefined
    hashContents = undefined
    createBlob   = undefined
    createCommit = undefined
    createTag    = undefined

    deleteRepository = undefined

instance Show (Git.Oid (SampleRepository m)) where
    show (Oid coid) = undefined

instance Ord (Git.Oid (SampleRepository m)) where
    Oid coid1 `compare` Oid coid2 = undefined

instance Eq (Git.Oid (SampleRepository m)) where
    oid1 == oid2 = oid1 `compare` oid2 == EQ

type TreeEntry m = Git.TreeEntry (SampleRepository m)

instance M m => Git.Treeish (Tree m) where
    type TreeRepository (Tree m) = SampleRepository m

    modifyTree      = undefined
    writeTree       = undefined
    traverseEntries = undefined

instance M m => Git.Commitish (Commit m) where
    type CommitRepository (Commit m) = SampleRepository m
    commitOid       = undefined
    commitParents   = undefined
    commitTree      = undefined
    commitAuthor    = undefined
    commitCommitter = undefined
    commitLog       = undefined
    commitEncoding  = undefined

instance M m => Git.Treeish (Commit m) where
    type TreeRepository (Commit m) = SampleRepository m

    modifyTree      = Git.defaultCommitModifyTree
    writeTree       = Git.defaultCommitWriteTree
    traverseEntries = Git.defaultCommitTraverseEntries

data Repository = Repository Void

instance Eq Repository where
  x == y = undefined

instance Show Repository where
  show x = undefined

newtype SampleRepository m a = SampleRepository
    { sampleRepositoryReaderT :: ReaderT Repository m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type Oid m       = Git.Oid (SampleRepository m)

type BlobOid m   = Git.BlobOid (SampleRepository m)
type TreeOid m   = Git.TreeOid (SampleRepository m)
type CommitOid m = Git.CommitOid (SampleRepository m)
type TagOid m    = Git.TagOid (SampleRepository m)

type Tree m      = Git.Tree (SampleRepository m)
type Commit m    = Git.Commit (SampleRepository m)

type TreeRef m   = Git.TreeRef (SampleRepository m)
type CommitRef m = Git.CommitRef (SampleRepository m)

type Reference m = Git.Reference (SampleRepository m) (Commit m)

sampleGet :: Monad m => SampleRepository m Repository
sampleGet = SampleRepository ask

sampleFactory :: M m => Git.RepositoryFactory (SampleRepository m) m Repository
sampleFactory = Git.RepositoryFactory
    { Git.openRepository  = openSampleRepository
    , Git.runRepository   = runSampleRepository
    , Git.closeRepository = closeSampleRepository
    , Git.defaultOptions  = defaultSampleOptions
    }

openSampleRepository :: M m => Git.RepositoryOptions -> m Repository
openSampleRepository opts = undefined

runSampleRepository :: M m => Repository -> SampleRepository m a -> m a
runSampleRepository repo action =
    runReaderT (sampleRepositoryReaderT action) repo

closeSampleRepository :: M m => Repository -> m ()
closeSampleRepository = const (return ())

defaultSampleOptions :: Git.RepositoryOptions
defaultSampleOptions = Git.RepositoryOptions F.empty False False

-- Sample.hs
