{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module Git.Libgit2.Blob where

import           Bindings.Libgit2
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString.Unsafe
import           Data.Tagged
import           Foreign.ForeignPtr
import           Foreign.Ptr
import qualified Git
import           Git.Libgit2.Internal
import           Git.Libgit2.Types

-- Blob.hs
