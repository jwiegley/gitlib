{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/odb_backend.h>
module Bindings.Libgit2.OdbBackend where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Oid
{- typedef struct {
            unsigned int version; git_oid_t oid_type;
        } git_odb_backend_pack_options; -}
#starttype git_odb_backend_pack_options
#field version , CUInt
#field oid_type , <git_oid_t>
#stoptype
#ccall git_odb_backend_pack , Ptr (Ptr <struct git_odb_backend>) -> CString -> IO CInt
#ccall git_odb_backend_one_pack , Ptr (Ptr <struct git_odb_backend>) -> CString -> IO CInt
{- typedef enum {
            GIT_ODB_BACKEND_LOOSE_FSYNC = 1 << 0
        } git_odb_backend_loose_flag_t; -}
#integral_t git_odb_backend_loose_flag_t
#num GIT_ODB_BACKEND_LOOSE_FSYNC
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            int compression_level;
            unsigned int dir_mode;
            unsigned int file_mode;
            git_oid_t oid_type;
        } git_odb_backend_loose_options; -}
#starttype git_odb_backend_loose_options
#field version , CUInt
#field flags , CUInt
#field compression_level , CInt
#field dir_mode , CUInt
#field file_mode , CUInt
#field oid_type , <git_oid_t>
#stoptype
#ccall git_odb_backend_loose , Ptr (Ptr <struct git_odb_backend>) -> CString -> CInt -> CInt -> CUInt -> CUInt -> IO CInt
{- typedef enum {
            GIT_STREAM_RDONLY = 1 << 1,
            GIT_STREAM_WRONLY = 1 << 2,
            GIT_STREAM_RW = GIT_STREAM_RDONLY | GIT_STREAM_WRONLY
        } git_odb_stream_t; -}
#integral_t git_odb_stream_t
#num GIT_STREAM_RDONLY
#num GIT_STREAM_WRONLY
#num GIT_STREAM_RW
{- struct git_odb_stream {
    git_odb_backend * backend;
    unsigned int mode;
    void * hash_ctx;
    git_object_size_t declared_size;
    git_object_size_t received_bytes;
    int (* read)(git_odb_stream * stream, char * buffer, size_t len);
    int (* write)(git_odb_stream * stream,
                  const char * buffer,
                  size_t len);
    int (* finalize_write)(git_odb_stream * stream,
                           const git_oid * oid);
    void (* free)(git_odb_stream * stream);
}; -}
#starttype struct git_odb_stream
#field backend , Ptr <struct git_odb_backend>
#field mode , CUInt
#field hash_ctx , Ptr ()
#field declared_size , CULong
#field received_bytes , CULong
#field read , FunPtr (Ptr <struct git_odb_stream> -> CString -> CSize -> CInt)
#field write , FunPtr (Ptr <struct git_odb_stream> -> CString -> CSize -> CInt)
#field finalize_write , FunPtr (Ptr <struct git_odb_stream> -> Ptr <struct git_oid> -> CInt)
#field free , FunPtr (Ptr <struct git_odb_stream> -> IO ())
#stoptype
{- struct git_odb_writepack {
    git_odb_backend * backend;
    int (* append)(git_odb_writepack * writepack,
                   const void * data,
                   size_t size,
                   git_indexer_progress * stats);
    int (* commit)(git_odb_writepack * writepack,
                   git_indexer_progress * stats);
    void (* free)(git_odb_writepack * writepack);
}; -}
#starttype struct git_odb_writepack
#field backend , Ptr <struct git_odb_backend>
#field append , FunPtr (Ptr <struct git_odb_writepack> -> Ptr () -> CSize -> Ptr <struct git_indexer_progress> -> CInt)
#field commit , FunPtr (Ptr <struct git_odb_writepack> -> Ptr <struct git_indexer_progress> -> CInt)
#field free , FunPtr (Ptr <struct git_odb_writepack> -> IO ())
#stoptype
