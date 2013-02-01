{-# LANGUAGE OverloadedStrings #-}
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

newtype LgRepository a = LgRepository
    { runLgRepository :: ReaderT Repository IO a }

type Oid       = Git.Oid LgRepository
type Tree      = Git.Tree LgRepository
type Commit    = Git.Commit LgRepository
type Reference = Git.Reference LgRepository Commit

type TreeRef   = Git.TreeRef LgRepository
type CommitRef = Git.CommitRef LgRepository

instance Functor LgRepository where
    fmap f (LgRepository x) = LgRepository (fmap f x)

instance Applicative LgRepository where
    pure = LgRepository . pure
    LgRepository f <*> LgRepository x = LgRepository (f <*> x)

instance Monad LgRepository where
    return = LgRepository . return
    LgRepository m >>= f = LgRepository (m >>= runLgRepository . f)

instance MonadIO LgRepository where
    liftIO m = LgRepository (liftIO m)

instance Failure Git.Exception LgRepository where
    failure = liftIO . throwIO

lgGet = LgRepository ask

-- Types.hs
