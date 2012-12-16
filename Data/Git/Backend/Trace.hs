{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Data.Git.Backend.Trace ( traceBackend ) where

import Data.ByteString as B hiding (map, putStrLn)
import Data.Git.Error
import Data.Git.Internal
import Data.Git.Object
import Data.Git.Backend

-- data TraceBackend

traceBackendReadCallback :: COid -> IO (Result BackendObj)
traceBackendReadCallback oid = do
  putStrLn $ "Read " ++ show oid
  return (Left (ErrorMsg "trace" c'GIT_PASSTHROUGH))

traceBackendReadPrefixCallback :: COid -> Int -> IO (Result BackendObj)
traceBackendReadPrefixCallback oid len = do
  putStrLn $ "Read Prefix " ++ show oid ++ " " ++ show len
  return (Left (ErrorMsg "trace" c'GIT_PASSTHROUGH))

traceBackendReadHeaderCallback :: COid -> IO (Result BackendObj)
traceBackendReadHeaderCallback oid = do
  putStrLn $ "Read Header " ++ show oid
  return (Left (ErrorMsg "trace" c'GIT_PASSTHROUGH))

traceBackendWriteCallback :: BackendObj -> IO (Result COid)
traceBackendWriteCallback obj = do
  putStrLn $ "Write " ++ show (resultOid obj)
  return (Left (ErrorMsg "trace" c'GIT_PASSTHROUGH))

traceBackendExistsCallback :: COid -> IO (Result Bool)
traceBackendExistsCallback oid = do
  putStrLn $ "Exists " ++ show oid
  return (Left (ErrorMsg "trace" c'GIT_PASSTHROUGH))

traceBackend :: Backend
traceBackend =
  Backend { backendOdb         = Nothing
          , backendRead        = Just traceBackendReadCallback
          , backendReadPrefix  = Just traceBackendReadPrefixCallback
          , backendReadHeader  = Just traceBackendReadHeaderCallback
          , backendWrite       = Just traceBackendWriteCallback
          , backendExists      = Just traceBackendExistsCallback }

-- Trace.hs
