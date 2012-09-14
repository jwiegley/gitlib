{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Tag where

import Control.Lens
import Data.Either
import Data.Git.Common
import Data.Git.Foreign
import Data.Text as T hiding (map)
import Prelude hiding (FilePath)

default (Text)

data Tag = Tag { _tagInfo :: Base Tag
               , _tagRef  :: Hash }

makeClassy ''Tag

instance Show Tag where
  show x = case x^.tagInfo^.gitId of
    Left _  -> "Tag"
    Right y -> "Tag#" ++ show y

-- Tag.hs
