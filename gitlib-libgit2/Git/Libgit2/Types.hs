{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Git.Libgit2.Types where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Exception
import           Control.Monad (liftM)
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Morph
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Conduit
import           Data.IORef
import           Data.Monoid
import           Foreign.ForeignPtr
import qualified Git

data Repository = Repository
    { repoOptions :: Git.RepositoryOptions
    , repoObj     :: ForeignPtr C'git_repository
    , repoExcTrap :: IORef (Maybe Git.GitException)
    }

repoPath :: Repository -> FilePath
repoPath = Git.repoPath . repoOptions

instance Eq Repository where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

instance Show Repository where
  show x = "Repository " <> repoPath x

newtype LgRepository m a = LgRepository
    { lgRepositoryReaderT :: ReaderT Repository m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, MonadIO m, Applicative m)
         => MonadBase IO (LgRepository m) where
    liftBase = liftIO

instance Monad m => MonadThrow (LgRepository m) where
    -- monadThrow :: Exception e => e -> m a
    monadThrow = throw

instance MonadLogger m => MonadLogger (LgRepository m) where
    monadLoggerLog a b c d = LgRepository $ monadLoggerLog a b c d

instance MonadTrans LgRepository where
    lift = LgRepository . ReaderT . const

instance MFunctor LgRepository where
    hoist nat m = LgRepository $ ReaderT $ \i ->
        nat (runReaderT (lgRepositoryReaderT m) i)

instance MonadTransControl LgRepository where
    newtype StT LgRepository a = StLgRepository
        { unLgRepository :: StT (ReaderT Repository) a
        }
    liftWith = defaultLiftWith LgRepository
                   lgRepositoryReaderT StLgRepository
    restoreT = defaultRestoreT LgRepository unLgRepository

instance (MonadIO m, MonadBaseControl IO m)
         => MonadBaseControl IO (LgRepository m) where
    newtype StM (LgRepository m) a = StMT
        { unStMT :: ComposeSt LgRepository m a
        }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM unStMT

type BlobOid m     = Git.BlobOid (LgRepository m)
type TreeOid m     = Git.TreeOid (LgRepository m)
type CommitOid m   = Git.CommitOid (LgRepository m)

type Tree m        = Git.Tree (LgRepository m)
type Commit m      = Git.Commit (LgRepository m)
type Tag m         = Git.Tag (LgRepository m)

type Object m      = Git.Object (LgRepository m)
type ObjectOid m   = Git.ObjectOid (LgRepository m)
type RefTarget m   = Git.RefTarget (LgRepository m)

type TreeBuilder m = Git.TreeBuilder (LgRepository m)
type Options m     = Git.Options (LgRepository m)

type MonadLg m     = (Git.MonadGit m, MonadLogger m)

lgGet :: Monad m => LgRepository m Repository
lgGet = LgRepository ask

lgExcTrap :: Monad m => LgRepository m (IORef (Maybe Git.GitException))
lgExcTrap = repoExcTrap `liftM` lgGet

-- Types.hs
