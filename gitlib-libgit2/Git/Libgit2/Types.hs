{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Git.Libgit2.Types where

import           Bindings.Libgit2
import           Control.Monad (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Control
import           Data.IORef
import           Foreign.ForeignPtr
import qualified Git

data LgRepo = LgRepo
    { repoOptions :: Git.RepositoryOptions
    , repoObj     :: ForeignPtr C'git_repository
    , repoExcTrap :: IORef (Maybe Git.GitException)
    }

lgRepoPath :: LgRepo -> FilePath
lgRepoPath = Git.repoPath . repoOptions

class HasLgRepo env where
    getLgRepo :: env -> LgRepo

instance HasLgRepo LgRepo where
    getLgRepo = id

instance HasLgRepo (env, LgRepo) where
    getLgRepo = snd

type BlobOid     = Git.BlobOid LgRepo
type TreeOid     = Git.TreeOid LgRepo
type CommitOid   = Git.CommitOid LgRepo

type Tree        = Git.Tree LgRepo
type TreeEntry   = Git.TreeEntry LgRepo
type Commit      = Git.Commit LgRepo
type Tag         = Git.Tag LgRepo

type Object      = Git.Object LgRepo
type ObjectOid   = Git.ObjectOid LgRepo
type RefTarget   = Git.RefTarget LgRepo

type TreeBuilder = Git.TreeBuilder LgRepo
type Options     = Git.Options LgRepo

type MonadLg m = (Git.MonadGit LgRepo m,
                  MonadIO m, MonadBaseControl IO m, MonadLogger m)

lgExcTrap :: MonadLg m => m (IORef (Maybe Git.GitException))
lgExcTrap = repoExcTrap `liftM` Git.getRepository

-- Types.hs
