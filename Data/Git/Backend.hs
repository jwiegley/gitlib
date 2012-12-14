{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Git.Backend
       ( Backend(..) )
       where

import Data.ByteString as B hiding (map)
import Data.ByteString.Unsafe
import Data.Conduit
import Data.Git.Common
import Data.Git.Error
import Data.Git.Internal
import Data.Git.Object

type Result a = Either GitError a

type BackendReadCallback = Backend -> Oid -> IO (Result (Int, Object))
type BackendReadHeaderCallback = Backend -> Oid -> IO (Result (Int, ObjectType))
type BackendReadStreamCallback =
  Backend -> Oid -> IO (Result (GSource IO ByteString))
type BackendWriteCallback = Backend -> Object -> IO (Result Oid)
type BackendWriteStreamCallback =
  Backend -> Object -> GSource IO ByteString -> IO (Result Oid)
type BackendExistsCallback = Backend -> Oid -> IO (Result Bool)
type BackendFreeCallback = Backend -> IO ()

default (Text)

data Backend =
  Backend { backendOdb         :: ObjPtr C'git_odb_backend
          , backendRead        :: Maybe BackendReadCallback
          , backendReadHeader  :: Maybe BackendReadHeaderCallback
          , backendReadStream  :: Maybe BackendReadStreamCallback
          , backendWrite       :: Maybe BackendWriteCallback
          , backendWriteStream :: Maybe BackendWriteStreamCallback
          , backendExists      :: Maybe BackendExistsCallback
          , backendFree        :: Maybe BackendFreeCallback }

-- Backend.hs
