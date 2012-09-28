{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Commit where

import Bindings.Libgit2
import Control.Lens
import Data.Either
import Data.Git.Common
import Data.Git.Internal
import Data.Git.Tree
import Data.Text as T hiding (map)
import Prelude hiding (FilePath)

default (Text)

data Commit = Commit { _commitInfo :: Base Commit
                     , _commitWho  :: WhoWhen
                     , _commitLog  :: Text
                     , _commitTree :: Tree
                     , _commitObj  :: ObjPtr C'git_commit }

makeClassy ''Commit

instance Show Commit where
  show x = case x^.commitInfo.gitId of
    Pending _ -> "Commit"
    Stored y  -> "Commit#" ++ show y

-- Commit.hs
