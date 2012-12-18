{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Backend
       ( odbBackendAdd

       , F'git_odb_backend_read_callback
       , F'git_odb_backend_read_prefix_callback
       , F'git_odb_backend_readstream_callback
       , F'git_odb_backend_read_header_callback
       , F'git_odb_backend_write_callback
       , F'git_odb_backend_writestream_callback
       , F'git_odb_backend_exists_callback
       , F'git_odb_backend_free_callback
       )
       where

import           Data.Git.Error
import           Data.Git.Internal
import qualified Prelude

default (Text)

type F'git_odb_backend_read_callback =
  Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend
    -> Ptr C'git_oid -> IO CInt
type F'git_odb_backend_read_prefix_callback =
  Ptr C'git_oid -> Ptr (Ptr ()) -> Ptr CSize -> Ptr C'git_otype
    -> Ptr C'git_odb_backend -> Ptr C'git_oid -> CUInt -> IO CInt
type F'git_odb_backend_readstream_callback =
  Ptr (Ptr C'git_odb_stream) -> Ptr C'git_odb_backend -> Ptr C'git_oid
    -> IO CInt
type F'git_odb_backend_read_header_callback =
  Ptr CSize -> Ptr C'git_otype -> Ptr C'git_odb_backend -> Ptr C'git_oid
    -> IO CInt
type F'git_odb_backend_write_callback =
  Ptr C'git_oid -> Ptr C'git_odb_backend -> Ptr () -> CSize -> C'git_otype
    -> IO CInt
type F'git_odb_backend_writestream_callback =
  Ptr (Ptr C'git_odb_stream) -> Ptr C'git_odb_backend -> CSize
    -> C'git_otype -> IO CInt
type F'git_odb_backend_exists_callback =
  Ptr C'git_odb_backend -> Ptr C'git_oid -> IO CInt
type F'git_odb_backend_free_callback = Ptr C'git_odb_backend -> IO ()

odbBackendAdd :: Repository -> Ptr C'git_odb_backend -> Int
                 -> IO (Result Repository)
odbBackendAdd repo' backend priority =
  withForeignPtr repo $ \repoPtr ->
    alloca $ \odbPtr -> do
      r <- c'git_repository_odb odbPtr repoPtr
      if r < 0
        then return (Left "Cannot get repository ODB")
        else do
        odb <- peek odbPtr
        r2 <- c'git_odb_add_backend odb backend (fromIntegral priority)
        if r2 < 0
          then return (Left "Cannot add backend to repository ODB")
          else return (Right repo')

  where
    repo = fromMaybe (throw RepositoryInvalid) (repoObj repo')

-- Backend.hs
