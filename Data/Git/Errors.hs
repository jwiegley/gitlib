{-# LANGUAGE DeriveDataTypeable #-}

-- | Error types which may be thrown during Git operations, using
--   'Control.Exception.throwIO'.
module Data.Git.Errors
       ( GitException(..) )
       where

import Control.Exception
import Data.Typeable
import Prelude hiding (FilePath)

-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data GitException = RepositoryNotExist String
                  | RepositoryInvalid
                  | BlobCreateFailed
                  | TreeCreateFailed
                  | TreeBuilderCreateFailed
                  | TreeBuilderInsertFailed
                  | TreeBuilderWriteFailed
                  | TreeLookupFailed
                  | TreeCannotTraverseBlob
                  | TreeEntryLookupFailed
                  | CommitCreateFailed
                  | CommitLookupFailed
                  | ReferenceCreateFailed
                  | RefCannotCreateFromPartialOid
                  | ReferenceLookupFailed
                  | ObjectLookupFailed
                  | ObjectIdTooLong
                  | ObjectRefRequiresFullOid
                  | OidCopyFailed
                  deriving (Show, Typeable)

instance Exception GitException

-- Errors.hs
