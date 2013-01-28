{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.Libgit2.Tree where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Either
import qualified Data.Map as M
import           Data.Stringable
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git
import           Git.Libgit2.Blob
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Prelude hiding (FilePath)

-- Tree.hs
