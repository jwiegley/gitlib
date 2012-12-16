{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Git.Backend.Trace ( traceBackend ) where

import Data.ByteString.Unsafe
import Data.Git.Backend
import Data.Git.Error
import Data.Git.Internal
import Data.Git.Oid

-- data TraceBackend

traceBackendReadCallback ::
  Ptr C'git_odb_backend ->
    Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend
      -> Ptr C'git_oid -> IO CInt
traceBackendReadCallback be data_p len_p type_p _ oid = do
  oidStr <- oidToStr oid
  putStrLn $ "Read " ++ oidStr
  read_callback <- peek (p'git_odb_backend'read be)
  (mK'git_odb_backend_read_callback read_callback)
    data_p len_p type_p be oid

traceBackendReadPrefixCallback ::
  Ptr C'git_odb_backend ->
    Ptr C'git_oid -> Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype
      -> Ptr C'git_odb_backend -> Ptr C'git_oid -> CUInt -> IO CInt
traceBackendReadPrefixCallback be out_oid oid_p len_p type_p _ oid len = do
  oidStr <- oidToStr oid
  putStrLn $ "Read Prefix " ++ oidStr ++ " " ++ show len
  read_prefix_callback <- peek (p'git_odb_backend'read_prefix be)
  (mK'git_odb_backend_read_prefix_callback read_prefix_callback)
    out_oid oid_p len_p type_p be oid len

-- BackendReadstreamCallback ::
--   BackendReadstreamCallback ->
--     Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> Ptr <git_oid>
--       -> IO CInt

traceBackendReadHeaderCallback ::
  Ptr C'git_odb_backend ->
    Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend -> Ptr C'git_oid
      -> IO CInt
traceBackendReadHeaderCallback be len_p type_p _ oid = do
  oidStr <- oidToStr oid
  putStrLn $ "Read Header " ++ oidStr
  read_header_callback <- peek (p'git_odb_backend'read_header be)
  (mK'git_odb_backend_read_header_callback read_header_callback)
    len_p type_p be oid

traceBackendWriteCallback ::
  Ptr C'git_odb_backend ->
    Ptr C'git_oid -> Ptr C'git_odb_backend -> Ptr () -> CSize -> C'git_otype
      -> IO CInt
traceBackendWriteCallback be oid _ obj_data len obj_type = do
  oidStr <- oidToStr oid
  putStrLn $ "Write " ++ oidStr ++ " len " ++ show len
  write_callback <- peek (p'git_odb_backend'write be)
  (mK'git_odb_backend_write_callback write_callback)
    oid be obj_data len obj_type

-- BackendWritestreamCallback ::
--   BackendWritestreamCallback ->
--     Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> CSize
--       -> <git_otype> -> IO CInt

traceBackendExistsCallback ::
  Ptr C'git_odb_backend ->
    Ptr C'git_odb_backend -> Ptr C'git_oid -> IO CInt
traceBackendExistsCallback be _ oid = do
  oidStr <- oidToStr oid
  putStrLn $ "Exists " ++ oidStr
  exists_callback <- peek (p'git_odb_backend'exists be)
  (mK'git_odb_backend_exists_callback exists_callback) be oid

-- BackendFreeCallback ::
--   BackendFreeCallback -> Ptr <git_odb_backend> -> IO ()

traceBackend :: Ptr C'git_odb_backend -> IO (Ptr C'git_odb_backend)
traceBackend be = do
  readFun <- mk'git_odb_backend_read_callback (traceBackendReadCallback be)
  readPrefixFun <-
    mk'git_odb_backend_read_prefix_callback (traceBackendReadPrefixCallback be)
  readHeaderFun <-
    mk'git_odb_backend_read_header_callback (traceBackendReadHeaderCallback be)
  writeFun <- mk'git_odb_backend_write_callback (traceBackendWriteCallback be)
  existsFun <-
    mk'git_odb_backend_exists_callback (traceBackendExistsCallback be)

  tracer <- malloc
  poke tracer $ C'git_odb_backend {
      c'git_odb_backend'odb         = nullPtr
    , c'git_odb_backend'read        = readFun
    , c'git_odb_backend'read_prefix = readPrefixFun
    , c'git_odb_backend'readstream  = nullFunPtr
    , c'git_odb_backend'read_header = readHeaderFun
    , c'git_odb_backend'write       = writeFun
    , c'git_odb_backend'writestream = nullFunPtr
    , c'git_odb_backend'exists      = existsFun
    , c'git_odb_backend'free        = nullFunPtr }
  return tracer

-- Trace.hs
