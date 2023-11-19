{-# LANGUAGE CPP #-}

module Bindings.Libgit2
    ( module Bindings.Libgit2.AnnotatedCommit
    , module Bindings.Libgit2.Apply
    , module Bindings.Libgit2.Attr
    , module Bindings.Libgit2.Blame
    , module Bindings.Libgit2.Blob
    , module Bindings.Libgit2.Branch
    , module Bindings.Libgit2.Buffer
    , module Bindings.Libgit2.Cert
    , module Bindings.Libgit2.Checkout
    , module Bindings.Libgit2.Cherrypick
    , module Bindings.Libgit2.Clone
    , module Bindings.Libgit2.Commit
    , module Bindings.Libgit2.Common
    , module Bindings.Libgit2.Config
    , module Bindings.Libgit2.CredentialHelpers
    , module Bindings.Libgit2.Credential
    , module Bindings.Libgit2.Describe
    , module Bindings.Libgit2.Diff
    , module Bindings.Libgit2.Errors
    , module Bindings.Libgit2.Experimental
    , module Bindings.Libgit2.Filter
    , module Bindings.Libgit2.Global
    , module Bindings.Libgit2.Graph
    , module Bindings.Libgit2.Ignore
    , module Bindings.Libgit2.Indexer
    , module Bindings.Libgit2.Index
    , module Bindings.Libgit2.Mailmap
    , module Bindings.Libgit2.Merge
    , module Bindings.Libgit2.Message
    , module Bindings.Libgit2.Net
    , module Bindings.Libgit2.Notes
    , module Bindings.Libgit2.Object
    , module Bindings.Libgit2.OdbBackend
    , module Bindings.Libgit2.Odb
    , module Bindings.Libgit2.Oidarray
    , module Bindings.Libgit2.Oid
    , module Bindings.Libgit2.Pack
    , module Bindings.Libgit2.Patch
    , module Bindings.Libgit2.Pathspec
    , module Bindings.Libgit2.Proxy
    , module Bindings.Libgit2.Rebase
    , module Bindings.Libgit2.Refdb
    , module Bindings.Libgit2.Reflog
    , module Bindings.Libgit2.Refs
    , module Bindings.Libgit2.Refspec
    , module Bindings.Libgit2.Remote
    , module Bindings.Libgit2.Repository
    , module Bindings.Libgit2.Reset
    , module Bindings.Libgit2.Revert
    , module Bindings.Libgit2.Revparse
    , module Bindings.Libgit2.Revwalk
    , module Bindings.Libgit2.Signature
    , module Bindings.Libgit2.Stash
    , module Bindings.Libgit2.Status
    , module Bindings.Libgit2.Stdint
    , module Bindings.Libgit2.Strarray
    , module Bindings.Libgit2.Submodule
    , module Bindings.Libgit2.Tag
    , module Bindings.Libgit2.Trace
    , module Bindings.Libgit2.Transaction
    , module Bindings.Libgit2.Transport
    , module Bindings.Libgit2.Tree
    , module Bindings.Libgit2.Types
    , module Bindings.Libgit2.Version
#ifdef WINDOWS
    , module Bindings.Libgit2.Windows
#endif
    , withLibGitDo
    ) where
import Bindings.Libgit2.Attr
import Bindings.Libgit2.AnnotatedCommit
import Bindings.Libgit2.Apply
import Bindings.Libgit2.Attr
import Bindings.Libgit2.Blame
import Bindings.Libgit2.Blob
import Bindings.Libgit2.Branch
import Bindings.Libgit2.Buffer
import Bindings.Libgit2.Cert
import Bindings.Libgit2.Checkout
import Bindings.Libgit2.Cherrypick
import Bindings.Libgit2.Clone
import Bindings.Libgit2.Commit
import Bindings.Libgit2.Common
import Bindings.Libgit2.Config
import Bindings.Libgit2.CredentialHelpers
import Bindings.Libgit2.Credential
import Bindings.Libgit2.Describe
import Bindings.Libgit2.Diff
import Bindings.Libgit2.Errors
import Bindings.Libgit2.Experimental
import Bindings.Libgit2.Filter
import Bindings.Libgit2.Global
import Bindings.Libgit2.Graph
import Bindings.Libgit2.Ignore
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Index
import Bindings.Libgit2.Mailmap
import Bindings.Libgit2.Merge
import Bindings.Libgit2.Message
import Bindings.Libgit2.Net
import Bindings.Libgit2.Notes
import Bindings.Libgit2.Object
import Bindings.Libgit2.OdbBackend
import Bindings.Libgit2.Odb
import Bindings.Libgit2.Oidarray
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Pack
import Bindings.Libgit2.Patch
import Bindings.Libgit2.Pathspec
import Bindings.Libgit2.Proxy
import Bindings.Libgit2.Rebase
import Bindings.Libgit2.Refdb
import Bindings.Libgit2.Reflog
import Bindings.Libgit2.Refs
import Bindings.Libgit2.Refspec
import Bindings.Libgit2.Remote
import Bindings.Libgit2.Repository
import Bindings.Libgit2.Reset
import Bindings.Libgit2.Revert
import Bindings.Libgit2.Revparse
import Bindings.Libgit2.Revwalk
import Bindings.Libgit2.Signature
import Bindings.Libgit2.Stash
import Bindings.Libgit2.Status
import Bindings.Libgit2.Stdint
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Submodule
import Bindings.Libgit2.Tag
import Bindings.Libgit2.Trace
import Bindings.Libgit2.Transaction
import Bindings.Libgit2.Transport
import Bindings.Libgit2.Tree
import Bindings.Libgit2.Types
import Bindings.Libgit2.Version
#ifdef WINDOWS
import Bindings.Libgit2.Windows
#endif

-- import Control.Monad.IO.Class (MonadIO, liftIO)
-- import Control.Monad.Trans.Control (MonadBaseControl)
-- import Control.Exception.Lifted (finally)
import Control.Exception (finally)
import System.Mem (performGC)

-- withLibGitDoT :: (MonadIO m, MonadBaseControl IO m) => m a -> m a
-- withLibGitDoT f = do
--     liftIO c'git_threads_init
--     finally f (liftIO $ performGC >> c'git_threads_shutdown)

-- | Write an IO action so that proper initialization and shutdown of the
--   thread libgit2 library is performed.
withLibGitDo :: IO a -> IO a
withLibGitDo f = do
    r <- c'git_libgit2_init
    if r < 0
        then error "c'git_threads_init failed"
        else finally f (performGC >> c'git_libgit2_shutdown)
