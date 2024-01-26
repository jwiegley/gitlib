{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/indexer.h>
module Bindings.Libgit2.Indexer where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef struct git_indexer git_indexer; -}
#opaque_t struct git_indexer
{- typedef struct git_indexer_progress {
            unsigned int total_objects;
            unsigned int indexed_objects;
            unsigned int received_objects;
            unsigned int local_objects;
            unsigned int total_deltas;
            unsigned int indexed_deltas;
            size_t received_bytes;
        } git_indexer_progress; -}
#starttype struct git_indexer_progress
#field total_objects , CUInt
#field indexed_objects , CUInt
#field received_objects , CUInt
#field local_objects , CUInt
#field total_deltas , CUInt
#field indexed_deltas , CUInt
#field received_bytes , CSize
#stoptype
#callback git_indexer_progress_cb , Ptr <struct git_indexer_progress> -> Ptr () -> IO CInt
{- typedef struct git_indexer_options {
            unsigned int version;
            git_indexer_progress_cb progress_cb;
            void * progress_cb_payload;
            unsigned char verify;
        } git_indexer_options; -}
#starttype struct git_indexer_options
#field version , CUInt
#field progress_cb , <git_indexer_progress_cb>
#field progress_cb_payload , Ptr ()
#field verify , CUChar
#stoptype
#ccall git_indexer_options_init , Ptr <struct git_indexer_options> -> CUInt -> IO CInt
#ccall git_indexer_new , Ptr (Ptr <struct git_indexer>) -> CString -> CUInt -> Ptr <struct git_odb> -> Ptr <struct git_indexer_options> -> IO CInt
#ccall git_indexer_append , Ptr <struct git_indexer> -> Ptr () -> CSize -> Ptr <struct git_indexer_progress> -> IO CInt
#ccall git_indexer_commit , Ptr <struct git_indexer> -> Ptr <struct git_indexer_progress> -> IO CInt
#ccall git_indexer_hash , Ptr <struct git_indexer> -> IO (Ptr <struct git_oid>)
#ccall git_indexer_name , Ptr <struct git_indexer> -> IO CString
#ccall git_indexer_free , Ptr <struct git_indexer> -> IO ()
