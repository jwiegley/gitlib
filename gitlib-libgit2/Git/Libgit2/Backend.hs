{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.Libgit2.Backend
       ( odbBackendAdd

       , F'git_odb_backend_read_callback
       , F'git_odb_backend_read_prefix_callback
       , F'git_odb_backend_readstream_callback
       , F'git_odb_backend_read_header_callback
       , F'git_odb_backend_write_callback
       , F'git_odb_backend_writestream_callback
       , F'git_odb_backend_exists_callback
       , F'git_odb_backend_refresh_callback
       , F'git_odb_backend_foreach_callback
       , F'git_odb_backend_writepack_callback
       , F'git_odb_backend_free_callback

       , F'git_odb_writepack_add_callback
       , F'git_odb_writepack_commit_callback
       , F'git_odb_writepack_free_callback
       )
       where

import Bindings.Libgit2
import Foreign.C.Types
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Git.Libgit2.Types

type F'git_odb_backend_read_callback =
    Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_object_t -> Ptr C'git_odb_backend
        -> Ptr C'git_oid -> IO CInt
type F'git_odb_backend_read_prefix_callback =
    Ptr C'git_oid -> Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_object_t
        -> Ptr C'git_odb_backend -> Ptr C'git_oid -> CSize -> IO CInt
type F'git_odb_backend_readstream_callback =
    Ptr (Ptr C'git_odb_stream) -> Ptr C'git_odb_backend -> Ptr C'git_oid
        -> IO CInt
type F'git_odb_backend_read_header_callback =
    Ptr CSize -> Ptr C'git_object_t -> Ptr C'git_odb_backend -> Ptr C'git_oid
        -> IO CInt
type F'git_odb_backend_write_callback =
    Ptr C'git_oid -> Ptr C'git_odb_backend -> Ptr () -> CSize -> C'git_object_t
        -> IO CInt
type F'git_odb_backend_writestream_callback =
    Ptr (Ptr C'git_odb_stream) -> Ptr C'git_odb_backend -> CSize
        -> C'git_object_t -> IO CInt
type F'git_odb_backend_exists_callback =
    Ptr C'git_odb_backend -> Ptr C'git_oid -> CInt -> IO CInt
type F'git_odb_backend_refresh_callback = Ptr C'git_odb_backend -> IO CInt
type F'git_odb_backend_foreach_callback =
    Ptr C'git_odb_backend -> C'git_odb_foreach_cb -> Ptr () -> IO CInt
type F'git_odb_backend_writepack_callback =
    Ptr (Ptr C'git_odb_writepack) -> Ptr C'git_odb_backend
        -> C'git_indexer_progress_cb -> Ptr () -> IO CInt
type F'git_odb_backend_free_callback = Ptr C'git_odb_backend -> IO ()

type F'git_odb_writepack_add_callback =
    Ptr C'git_odb_writepack -> Ptr () -> CSize -> Ptr C'git_indexer_progress_cb
        -> IO CInt
type F'git_odb_writepack_commit_callback =
    Ptr C'git_odb_writepack -> Ptr C'git_indexer_progress_cb -> IO CInt
type F'git_odb_writepack_free_callback = Ptr C'git_odb_writepack -> IO ()


odbBackendAdd :: LgRepo -> Ptr C'git_odb_backend -> Int
              -> IO (Either String LgRepo)
odbBackendAdd repo backend priority =
  withForeignPtr (repoObj repo) $ \repoPtr ->
    alloca $ \odbPtr -> do
      r <- c'git_repository_odb odbPtr repoPtr
      if r < 0
        then return (Left "Cannot get repository ODB")
        else do
        odb <- peek odbPtr
        r2 <- c'git_odb_add_backend odb backend (fromIntegral priority)
        c'git_odb_free odb
        if r2 < 0
          then return (Left "Cannot add backend to repository ODB")
          else return (Right repo)

-- Backend.hs
