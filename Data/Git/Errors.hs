{-# LANGUAGE DeriveDataTypeable #-}

module Data.Git.Errors
       ( GitException(..) )
       where

import Control.Exception
import Data.Typeable
import Prelude hiding (FilePath)

data GitException = RepositoryNotExist String
                  | RepositoryInvalid
                  | BlobCreateFailed
                  | TreeCreateFailed
                  | TreeBuilderCreateFailed
                  | TreeBuilderInsertFailed
                  | TreeBuilderWriteFailed
                  | ObjectLookupFailed
                  | ObjectIdTooLong
                  | OidCopyFailed
                  deriving (Show, Typeable)

instance Exception GitException

-- Errors.hs
