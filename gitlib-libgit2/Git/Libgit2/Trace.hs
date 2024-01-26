{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}

module Git.Libgit2.Trace where

import Bindings.Libgit2
import Control.Applicative
import Control.Monad
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Utils
import Foreign.Ptr
import Foreign.Storable
import Git.Libgit2.Backend
import Prelude hiding (mapM_)

--data TraceBackend = TraceBackend { traceParent :: C'git_odb_backend
--                                 , traceNext   :: Ptr C'git_odb_backend }
--
--instance Storable TraceBackend where
--  sizeOf _ = sizeOf (undefined :: C'git_odb_backend) +
--             sizeOf (undefined :: Ptr C'git_odb_backend)
--  alignment p = alignment (traceParent p)
--  peek p = do
--    v0 <- peekByteOff p 0
--    v1 <- peekByteOff p (sizeOf (undefined :: C'git_odb_backend))
--    return (TraceBackend v0 v1)
--  poke p (TraceBackend v0 v1) = do
--    pokeByteOff p 0 v0
--    pokeByteOff p (sizeOf (undefined :: C'git_odb_backend)) v1
--    return ()

oidToStr :: Ptr C'git_oid -> Int -> IO String
oidToStr oid len = do 
  ptr <- mallocForeignPtrArray0 len
  withForeignPtr ptr $ \ptr' -> do
    _ <- c'git_oid_fmt ptr' oid 
    str <- peekCString ptr'
    return $ take len str

--traceBackendReadCallback :: F'git_odb_backend_read_callback
--traceBackendReadCallback data_p len_p type_p be oid = do
--  oidStr <- oidToStr oid 40
--  putStrLn $ "Read " ++ oidStr
--  tb <- peek (castPtr be :: Ptr TraceBackend)
--  tn <- peek (traceNext tb)
--  mK'git_odb_backend_read_callback
--      (c'git_odb_backend'read tn)
--      data_p
--      len_p
--      type_p
--      (traceNext tb)
--      oid
--
--traceBackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
--traceBackendReadPrefixCallback out_oid oid_p len_p type_p be oid len = do
--  oidStr <- oidToStr oid 40
--  putStrLn $ "Read Prefix " ++ oidStr ++ " " ++ show len
--  tb <- peek (castPtr be :: Ptr TraceBackend)
--  tn <- peek (traceNext tb)
--  mK'git_odb_backend_read_prefix_callback
--      (c'git_odb_backend'read_prefix tn)
--      out_oid
--      oid_p
--      len_p
--      type_p
--      (traceNext tb)
--      oid
--      len
--
--traceBackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
--traceBackendReadHeaderCallback len_p type_p be oid = do
--  oidStr <- oidToStr oid 40
--  putStrLn $ "Read Header " ++ oidStr
--  tb <- peek (castPtr be :: Ptr TraceBackend)
--  tn <- peek (traceNext tb)
--  mK'git_odb_backend_read_header_callback
--      (c'git_odb_backend'read_header tn)
--      len_p
--      type_p
--      (traceNext tb)
--      oid
--
--traceBackendWriteCallback :: F'git_odb_backend_write_callback
--traceBackendWriteCallback oid be obj_data len obj_type = do
--  r <- c'git_odb_hash oid obj_data len obj_type
--  case r of
--    0 -> do
--      oidStr <- oidToStr oid 40
--      putStrLn $ "Write " ++ oidStr ++ " len " ++ show len
--      tb <- peek (castPtr be :: Ptr TraceBackend)
--      tn <- peek (traceNext tb)
--      mK'git_odb_backend_write_callback
--          (c'git_odb_backend'write tn)
--          oid
--          (traceNext tb)
--          obj_data
--          len
--          obj_type
--    n -> return n
--
--traceBackendExistsCallback :: F'git_odb_backend_exists_callback
--traceBackendExistsCallback be oid confirmNotExists = do
--  oidStr <- oidToStr oid 40
--  putStrLn $ "Exists " ++ oidStr
--  tb <- peek (castPtr be :: Ptr TraceBackend)
--  tn <- peek (traceNext tb)
--  mK'git_odb_backend_exists_callback
--      (c'git_odb_backend'exists tn)
--      (traceNext tb)
--      oid
--      confirmNotExists
--
--traceBackendFreeCallback :: F'git_odb_backend_free_callback
--traceBackendFreeCallback be = do
--  backend <- peek be
--  freeHaskellFunPtr (c'git_odb_backend'read backend)
--  freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
--  freeHaskellFunPtr (c'git_odb_backend'read_header backend)
--  freeHaskellFunPtr (c'git_odb_backend'write backend)
--  freeHaskellFunPtr (c'git_odb_backend'exists backend)
--
--foreign export ccall "traceBackendFreeCallback"
--  traceBackendFreeCallback :: F'git_odb_backend_free_callback
--foreign import ccall "&traceBackendFreeCallback"
--  traceBackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback
--
--traceBackend :: Ptr C'git_odb_backend -> IO (Ptr C'git_odb_backend)
--traceBackend be = do
--  readFun <- mk'git_odb_backend_read_callback traceBackendReadCallback
--  readPrefixFun <-
--    mk'git_odb_backend_read_prefix_callback traceBackendReadPrefixCallback
--  readHeaderFun <-
--    mk'git_odb_backend_read_header_callback traceBackendReadHeaderCallback
--  writeFun <- mk'git_odb_backend_write_callback traceBackendWriteCallback
--  existsFun <- mk'git_odb_backend_exists_callback traceBackendExistsCallback
--
--  castPtr <$> new TraceBackend {
--    traceParent = C'git_odb_backend {
--         c'git_odb_backend'version     = 1
--       , c'git_odb_backend'odb         = nullPtr
--       , c'git_odb_backend'read        = readFun
--       , c'git_odb_backend'read_prefix = readPrefixFun
--       , c'git_odb_backend'readstream  = nullFunPtr
--       , c'git_odb_backend'read_header = readHeaderFun
--       , c'git_odb_backend'write       = writeFun
--       , c'git_odb_backend'writestream = nullFunPtr
--       , c'git_odb_backend'exists      = existsFun
--       , c'git_odb_backend'refresh     = undefined
--       , c'git_odb_backend'foreach     = undefined
--       , c'git_odb_backend'writepack   = undefined
--       , c'git_odb_backend'free        = traceBackendFreeCallbackPtr
--       }
--    , traceNext = be }

-- Trace.hs
