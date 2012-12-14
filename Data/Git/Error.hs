{-# LANGUAGE DeriveDataTypeable #-}

-- | Error types which may be thrown during Git operations, using
--   'Control.Exception.throwIO'.
module Data.Git.Error
       ( GitException(..), GitError(..) )
       where

import Control.Exception
import Data.Typeable
import Data.Text
import Prelude hiding (FilePath)

-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data GitException = RepositoryNotExist String
                  | RepositoryInvalid
                  | BlobCreateFailed
                  | BlobEmptyCreateFailed
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

data GitErrorClass = GitErrorNoMemory
                   | GitErrorOS
                   | GitErrorInvalid
                   | GitErrorReference
                   | GitErrorZLib
                   | GitErrorRepository
                   | GitErrorConfig
                   | GitErrorRegex
                   | GitErrorODB
                   | GitErrorIndex
                   | GitErrorObject
                   | GitErrorNet
                   | GitErrorTag
                   | GitErrorTree
                   | GitErrorIndexer

data GitError = GitError { errorMessage :: Text
                         , errorClass   :: GitErrorClass }

instance Exception GitException

-- Error.hs
