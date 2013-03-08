{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
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

data Repository = Repository
    { repoPath :: FilePath
    , repoObj  :: ForeignPtr C'git_repository }

instance Eq Repository where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

instance Show Repository where
  show x = "Repository " <> toString (repoPath x)

newtype LgRepository m a = LgRepository
    { runLgRepository :: ReaderT Repository m a }
    deriving (Functor, Applicative, Monad, MonadIO, MonadTrans)

type Oid m     = Git.Oid (LgRepository m)

type BlobOid m   = Git.BlobOid (LgRepository m)
type TreeOid m   = Git.TreeOid (LgRepository m)
type CommitOid m = Git.CommitOid (LgRepository m)

type Tree m      = Git.Tree (LgRepository m)
type Commit m    = Git.Commit (LgRepository m)

type TreeRef m   = Git.TreeRef (LgRepository m)
type CommitRef m = Git.CommitRef (LgRepository m)

type Reference m = Git.Reference (LgRepository m) (Commit m)

lgGet :: Monad m => LgRepository m Repository
lgGet = LgRepository ask

-- Types.hs
