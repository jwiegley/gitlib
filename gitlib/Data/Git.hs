{-| @gitlib@ is a high-level, lazy and conduit-aware type wrapper around the
    libgit2 C library (<http://libgit2.github.com>).  The aim is both
    type-safety and convenience of use for Haskell users, combined with high
    performance and minimal memory footprint by taking advantage of Haskell's
    laziness and the conduit library's deterministic resource cleanup.

    For further information, as well as typical use cases, see
    "Data.Git.Tutorial".
-}

module Data.Git (
    module Data.Git.Repository,
    -- module Data.Git.Config,
    -- module Data.Git.Tag,
    -- module Data.Git.Reflog,
    module Data.Git.Commit,
    -- module Data.Git.Index,
    module Data.Git.Blob,
    module Data.Git.Error,
    module Data.Git.Oid,
    module Data.Git.Tree,
    module Data.Git.Reference,
    -- module Data.Git.Odb,
    module Data.Git.Common,
    module Data.Git.Object
) where

import Data.Git.Repository
-- import Data.Git.Config
-- import Data.Git.Tag
-- import Data.Git.Reflog
import Data.Git.Commit
-- import Data.Git.Index
import Data.Git.Blob
import Data.Git.Error
import Data.Git.Oid
import Data.Git.Tree
import Data.Git.Reference
-- import Data.Git.Odb
import Data.Git.Common
import Data.Git.Object
