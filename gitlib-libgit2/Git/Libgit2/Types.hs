{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Git.Libgit2.Types where

import           Bindings.Libgit2
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Reader.Class
import           Data.IORef
import           Foreign.ForeignPtr
import qualified Git

data LgRepo = LgRepo
    { -- repoOptions :: Git.RepositoryOptions
    -- , 
      repoObj     :: ForeignPtr C'git_repository
    , repoExcTrap :: IORef (Maybe Git.GitException)
    }

lgRepoPath :: LgRepo -> FilePath
lgRepoPath = error "NYI: lgRepoPath" -- Git.repoPath . repoOptions

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
-- type Options     = Git.Options LgRepo

type MonadExcept m = (MonadThrow m, MonadCatch m, MonadMask m)

type MonadLgBase m = (Applicative m, MonadExcept m,
                      MonadIO m, MonadBaseControl IO m)

type MonadLg m = (MonadLgBase m, MonadReader LgRepo m)

-- Types.hs
