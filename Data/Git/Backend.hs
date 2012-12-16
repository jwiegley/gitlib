{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Git.Backend
       ( Backend(..), BackendObj(..) )
       where

import Data.ByteString as B hiding (map)
-- import Data.Conduit
import Data.Git.Error
import Data.Git.Internal
import Data.Git.Object

data BackendObj = BackendObj { resultOid  :: COid
                             , resultSize :: Int
                             , resultType :: ObjectType
                             , resultData :: Maybe ByteString }

type BackendReadCallback       = COid -> IO (Result BackendObj)
type BackendReadPrefixCallback = COid -> Int -> IO (Result BackendObj)
type BackendReadHeaderCallback = COid -> IO (Result BackendObj)
type BackendWriteCallback      = BackendObj -> IO (Result COid)
type BackendExistsCallback     = COid -> IO (Result Bool)

-- type BackendReadStreamCallback =
--   Backend -> Oid -> IO (Result (GSource IO ByteString))
-- type BackendWriteStreamCallback =
--   Backend -> Object -> GSource IO ByteString -> IO (Result Oid)

-- type BackendFreeCallback = Backend -> IO ()

default (Text)

data Backend =
  Backend { backendOdb         :: ObjPtr C'git_odb_backend
          , backendRead        :: Maybe BackendReadCallback
          , backendReadPrefix  :: Maybe BackendReadPrefixCallback
          , backendReadHeader  :: Maybe BackendReadHeaderCallback
          -- , backendReadStream  :: Maybe BackendReadStreamCallback
          , backendWrite       :: Maybe BackendWriteCallback
          -- , backendWriteStream :: Maybe BackendWriteStreamCallback
          , backendExists      :: Maybe BackendExistsCallback
          -- , backendFree        :: Maybe BackendFreeCallback
          }

-- odbAddBackend :: ObjectDB -> Backend -> Int -> ObjectDB
-- odbAddBackend odb backend priority = undefined
--   -- jww (2012-12-14): Using backend, create a C'git_odb_backend and add that.

backendReadCallbackThunk ::
  BackendReadCallback ->
    Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend
      -> Ptr C'git_oid -> IO CInt
backendReadCallbackThunk cb data_p len_p type_p _be oid = do
  return 0

backendReadPrefixCallbackThunk ::
  BackendReadPrefixCallback ->
    Ptr C'git_oid -> Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype
      -> Ptr C'git_odb_backend -> Ptr C'git_oid -> CUInt -> IO CInt
backendReadPrefixCallbackThunk cb out_oid oid_p len_p type_p _be oid len = do
  return 0

backendReadHeaderCallbackThunk ::
  BackendReadHeaderCallback ->
    Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend -> Ptr C'git_oid
      -> IO CInt
backendReadHeaderCallbackThunk cb len_p type_p _be oid = do
  return 0

backendWriteCallbackThunk ::
  BackendWriteCallback ->
    Ptr C'git_oid -> Ptr C'git_odb_backend -> Ptr () -> CSize -> C'git_otype
      -> IO CInt
backendWriteCallbackThunk cb oid _be obj_data len obj_type = do
  return 0

backendExistsCallbackThunk ::
  BackendExistsCallback -> Ptr C'git_odb_backend -> Ptr C'git_oid -> IO CInt
backendExistsCallbackThunk cb _be oid = do
  return 0

-- BackendReadstreamCallbackThunk :: BackendReadstreamCallback -> Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
-- BackendWritestreamCallbackThunk :: BackendWritestreamCallback -> Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> CSize -> <git_otype> -> IO CInt

-- BackendFreeCallbackThunk :: BackendFreeCallback -> Ptr <git_odb_backend> -> IO ()

-- Backend.hs
