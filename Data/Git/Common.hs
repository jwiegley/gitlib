{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Common
       ( Author, HasAuthor(..)
       , WhoWhen, HasWhoWhen(..)
       , Base(..), gitId, gitRepo
       , newBase )
       where

import Control.Lens
import Data.Either
import Data.Git.Internal
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

-- Common.hs
