{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/blob.h>
module Bindings.Libgit2.Blob where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
import Bindings.Libgit2.Buffer
#ccall git_blob_lookup , Ptr (Ptr <struct git_blob>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> IO CInt
#ccall git_blob_lookup_prefix , Ptr (Ptr <struct git_blob>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> CSize -> IO CInt
#ccall git_blob_free , Ptr <struct git_blob> -> IO ()
#ccall git_blob_id , Ptr <struct git_blob> -> IO (Ptr <struct git_oid>)
#ccall git_blob_owner , Ptr <struct git_blob> -> IO (Ptr <struct git_repository>)
#ccall git_blob_rawcontent , Ptr <struct git_blob> -> IO (Ptr ())
#ccall git_blob_rawsize , Ptr <struct git_blob> -> IO CULong
{- typedef enum {
            GIT_BLOB_FILTER_CHECK_FOR_BINARY = 1 << 0,
            GIT_BLOB_FILTER_NO_SYSTEM_ATTRIBUTES = 1 << 1,
            GIT_BLOB_FILTER_ATTRIBUTES_FROM_HEAD = 1 << 2,
            GIT_BLOB_FILTER_ATTRIBUTES_FROM_COMMIT = 1 << 3
        } git_blob_filter_flag_t; -}
#integral_t git_blob_filter_flag_t
#num GIT_BLOB_FILTER_CHECK_FOR_BINARY
#num GIT_BLOB_FILTER_NO_SYSTEM_ATTRIBUTES
#num GIT_BLOB_FILTER_ATTRIBUTES_FROM_HEAD
#num GIT_BLOB_FILTER_ATTRIBUTES_FROM_COMMIT
{- typedef struct {
            int version;
            uint32_t flags;
            git_oid * commit_id;
            git_oid attr_commit_id;
        } git_blob_filter_options; -}
#starttype git_blob_filter_options
#field version , CInt
#field flags , CUInt
#field commit_id , Ptr <struct git_oid>
#field attr_commit_id , <struct git_oid>
#stoptype
#ccall git_blob_filter_options_init , Ptr <git_blob_filter_options> -> CUInt -> IO CInt
#ccall git_blob_filter , Ptr <git_buf> -> Ptr <struct git_blob> -> CString -> Ptr <git_blob_filter_options> -> IO CInt
#ccall git_blob_create_from_workdir , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_blob_create_from_disk , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_blob_create_from_stream , Ptr (Ptr <struct git_writestream>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_blob_create_from_stream_commit , Ptr <struct git_oid> -> Ptr <struct git_writestream> -> IO CInt
#ccall git_blob_create_from_buffer , Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr () -> CSize -> IO CInt
#ccall git_blob_is_binary , Ptr <struct git_blob> -> IO CInt
#ccall git_blob_data_is_binary , CString -> CSize -> IO CInt
#ccall git_blob_dup , Ptr (Ptr <struct git_blob>) -> Ptr <struct git_blob> -> IO CInt
