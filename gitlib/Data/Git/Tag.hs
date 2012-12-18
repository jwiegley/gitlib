{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Tag where

import           Data.Git.Common
import           Data.Git.Internal
import qualified Data.Text as T
import qualified Prelude

default (Text)

data Tag = Tag { tagInfo :: Base Tag
               , tagRef  :: Oid }

instance Show Tag where
  show x = case gitId (tagInfo x) of
    Pending _ -> "Tag"
    Stored y  -> "Tag#" ++ show y

-- Tag.hs
