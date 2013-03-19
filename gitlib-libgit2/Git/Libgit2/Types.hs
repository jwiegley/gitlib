{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Git.Libgit2.Types where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Conduit
import           Data.Monoid
import           Data.Stringable
import           Filesystem.Path.CurrentOS (FilePath)
import           Foreign.ForeignPtr
import qualified Git
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

data Repository = Repository
    { repoOptions :: Git.RepositoryOptions
    , repoObj     :: ForeignPtr C'git_repository }

repoPath = Git.repoPath . repoOptions

instance Eq Repository where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

instance Show Repository where
  show x = "Repository " <> toString (repoPath x)

newtype LgRepository m a = LgRepository
    { lgRepositoryReaderT :: ReaderT Repository m a }
    deriving (Functor, Applicative, Monad, MonadIO)

instance (Monad m, MonadIO m, Applicative m)
         => MonadBase IO (LgRepository m) where
    liftBase = liftIO

instance Monad m => MonadUnsafeIO (LgRepository m) where
    unsafeLiftIO = return . unsafePerformIO

instance Monad m => MonadThrow (LgRepository m) where
    -- monadThrow :: Exception e => e -> m a
    monadThrow = throw

instance MonadTrans LgRepository where
    lift = LgRepository . ReaderT . const

instance MonadTransControl LgRepository where
    newtype StT LgRepository a =
        StLgRepository {unLgRepository :: StT (ReaderT Repository) a}
    liftWith = defaultLiftWith LgRepository lgRepositoryReaderT StLgRepository
    restoreT = defaultRestoreT LgRepository unLgRepository

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

type Options m   = Git.Options (LgRepository m)

lgGet :: Monad m => LgRepository m Repository
lgGet = LgRepository ask

-- Types.hs
