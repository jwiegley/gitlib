{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Repository
       ( Repository
       , HasRepository(..)

       , openRepository )
       where

import Bindings.Libgit2
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Data.Either
import Data.Git.Foreign
import Data.Git.Errors
import Data.Maybe
import Data.Text as T hiding (map)
import Filesystem.Path.CurrentOS
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import Prelude hiding (FilePath)

default (Text)

data Repository = Repository { _repoPath :: FilePath
                             , _repoObj  :: ObjPtr C'git_repository }

makeClassy ''Repository

instance Show Repository where
  show x = T.unpack $
           T.append "Repository " (either id id (toText (x^.repoPath)))

openRepository :: FilePath -> IO Repository
openRepository path = alloca $ \ptr ->
  case T.unpack <$> toText path of
    Left p  -> throwIO (RepositoryNotExist (T.unpack p))
    Right p ->
      withCString p $ \str -> do
        r <- c'git_repository_open ptr str
        when (r < 0) $ throwIO (RepositoryNotExist p)
        ptr' <- peek ptr
        fptr <- newForeignPtr p'git_repository_free ptr'
        return Repository { _repoPath = path
                          , _repoObj  = Just fptr }

-- Repository.hs
