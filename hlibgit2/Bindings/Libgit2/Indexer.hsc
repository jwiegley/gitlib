{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Indexer where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
{- typedef struct git_transfer_progress {
            unsigned int total_objects;
            unsigned int indexed_objects;
            unsigned int received_objects;
            size_t received_bytes;
        } git_transfer_progress; -}
#starttype git_transfer_progress
#field total_objects , CUInt
#field indexed_objects , CUInt
#field received_objects , CUInt
#field received_bytes , CSize
#stoptype
{- typedef int (* git_transfer_progress_callback)(const git_transfer_progress * stats,
                                               void * payload); -}
#callback git_transfer_progress_callback , Ptr (<git_transfer_progress>) -> Ptr () -> IO CInt
{- typedef struct git_indexer_stream git_indexer_stream; -}
#opaque_t git_indexer_stream
#ccall git_indexer_stream_new , Ptr (Ptr <git_indexer_stream>) -> CString -> <git_transfer_progress_callback> -> Ptr () -> IO (CInt)
#ccall git_indexer_stream_add , Ptr <git_indexer_stream> -> Ptr () -> CSize -> Ptr <git_transfer_progress> -> IO (CInt)
#ccall git_indexer_stream_finalize , Ptr <git_indexer_stream> -> Ptr <git_transfer_progress> -> IO (CInt)
#ccall git_indexer_stream_hash , Ptr <git_indexer_stream> -> IO (Ptr <git_oid>)
#ccall git_indexer_stream_free , Ptr <git_indexer_stream> -> IO ()
