{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Git.Libgit2.Types where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Monoid
import           Data.Stringable
import           Filesystem.Path.CurrentOS (FilePath)
import           Foreign.ForeignPtr
import qualified Git
import           Prelude hiding (FilePath)

type M m = (Failure Git.GitException m, MonadIO m, Applicative m)

data Repository m = Repository
    { repoOptions :: Git.RepositoryOptions m
    , repoObj     :: ForeignPtr C'git_repository }

repoPath = Git.repoPath . repoOptions

instance Eq (Repository (LgRepository m)) where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

instance Show (Repository (LgRepository m)) where
  show x = "Repository " <> toString (repoPath x)

newtype LgRepository m a = LgRepository
    { lgRepositoryReaderT :: ReaderT (Repository (LgRepository m)) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance MonadTrans LgRepository where
    lift = LgRepository . ReaderT . const

type Oid m       = Git.Oid (LgRepository m)

type BlobOid m   = Git.BlobOid (LgRepository m)
type TreeOid m   = Git.TreeOid (LgRepository m)
type CommitOid m = Git.CommitOid (LgRepository m)

type Tree m      = Git.Tree (LgRepository m)
type Commit m    = Git.Commit (LgRepository m)
type Tag m       = Git.Tag (LgRepository m)

type TreeRef m   = Git.TreeRef (LgRepository m)
type CommitRef m = Git.CommitRef (LgRepository m)

type Reference m = Git.Reference (LgRepository m) (Commit m)

type Context m   = Git.Context (LgRepository m)
type Options m   = Git.Options (LgRepository m)

lgGet :: Monad m => LgRepository m (Repository (LgRepository m))
lgGet = LgRepository ask

-- Types.hs
