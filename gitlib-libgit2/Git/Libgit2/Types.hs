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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import           Data.Conduit
import           Data.IORef
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text as T (pack, unpack)
import           Filesystem.Path.CurrentOS (FilePath, toText)
import           Foreign.ForeignPtr
import qualified Git
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

data Repository = Repository
    { repoOptions :: Git.RepositoryOptions
    , repoObj     :: ForeignPtr C'git_repository
    , repoExcTrap :: IORef (Maybe Git.GitException)
    }

repoPath :: Repository -> FilePath
repoPath = Git.repoPath . repoOptions

instance Eq Repository where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

pathText :: FilePath -> Text
pathText = go . toText
  where
    go (Left e) =
        throw . Git.BackendError $ "Could not render path: " <> T.pack (show e)
    go (Right p) = p

pathStr :: FilePath -> String
pathStr = T.unpack . pathText

instance Show Repository where
  show x = "Repository " <> pathStr (repoPath x)

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

type Oid m        = Git.Oid (LgRepository m)

type BlobOid m    = Git.BlobOid (LgRepository m)
type TreeOid m    = Git.TreeOid (LgRepository m)
type CommitOid m  = Git.CommitOid (LgRepository m)

type Tree m       = Git.Tree (LgRepository m)
type Commit m     = Git.Commit (LgRepository m)
type Tag m        = Git.Tag (LgRepository m)

type TreeRef m    = Git.TreeRef (LgRepository m)
type CommitRef m  = Git.CommitRef (LgRepository m)
type CommitName m = Git.CommitName (LgRepository m)

type Reference m  = Git.Reference (LgRepository m) (Commit m)
type Object m     = Git.Object (LgRepository m)

type Options m    = Git.Options (LgRepository m)

lgGet :: Monad m => LgRepository m Repository
lgGet = LgRepository ask

lgExcTrap :: Monad m => LgRepository m (IORef (Maybe Git.GitException))
lgExcTrap = repoExcTrap `liftM` lgGet

-- Types.hs
