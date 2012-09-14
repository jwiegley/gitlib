{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Object where

import Bindings.Libgit2
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Data.ByteString as B hiding (map)
import Data.ByteString.Unsafe
import Data.Either
import Data.Git.Blob
import Data.Git.Tree
import Data.Git.Commit
import Data.Git.Tag
import Data.Map as M hiding (map)
import Data.Maybe
import Data.Text as T hiding (map)
import Data.Time
import Data.Typeable
import Filesystem.Path.CurrentOS
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (FilePath)
import Unsafe.Coerce

default (Text)

data Object = BlobO   Blob
            | TreeO   Tree
            | CommitO Commit
            | TagO    Tag

-- Object.hs
