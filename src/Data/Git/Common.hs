{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Common where

import Control.Lens
import Data.Either
import Data.Git.Foreign
import Data.Git.Repository
import Data.Text as T hiding (map)
import Data.Time
import Prelude hiding (FilePath)

default (Text)

data Author = Author { _authorName  :: Text
                     , _authorEmail :: Text }
            deriving (Show, Eq)

makeClassy ''Author

data WhoWhen = WhoWhen { _whoAuthor        :: Author
                       , _whoAuthorDate    :: UTCTime
                       , _whoCommitter     :: Author
                       , _whoCommitterDate :: UTCTime }
           deriving (Show, Eq)

makeClassy ''WhoWhen

data Base a = Base { _gitId   :: Ident a
                   , _gitRepo :: Repository }

makeClassy ''Base

instance Show (Base a) where
  show x = case x^.gitId of
    Left _  -> "Base"
    Right y -> "Base#" ++ show y

newBase :: Repository -> (a -> IO Hash) -> Base a
newBase repo f = Base { _gitId   = Left f
                      , _gitRepo = repo }

-- Common.hs
