#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Indexer where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
{- typedef struct git_indexer_stats {
            unsigned int total; unsigned int processed;
        } git_indexer_stats; -}
#starttype git_indexer_stats
#field total , CUInt
#field processed , CUInt
#stoptype
{- typedef struct git_indexer git_indexer; -}
#opaque_t git_indexer
{- typedef struct git_indexer_stream git_indexer_stream; -}
#opaque_t git_indexer_stream
#ccall git_indexer_stream_new , Ptr (Ptr <git_indexer_stream>) -> CString -> IO (CInt)
#ccall git_indexer_stream_add , Ptr <git_indexer_stream> -> Ptr () -> CSize -> Ptr <git_indexer_stats> -> IO (CInt)
#ccall git_indexer_stream_finalize , Ptr <git_indexer_stream> -> Ptr <git_indexer_stats> -> IO (CInt)
#ccall git_indexer_stream_hash , Ptr <git_indexer_stream> -> IO (Ptr <git_oid>)
#ccall git_indexer_stream_free , Ptr <git_indexer_stream> -> IO ()
#ccall git_indexer_new , Ptr (Ptr <git_indexer>) -> CString -> IO (CInt)
#ccall git_indexer_run , Ptr <git_indexer> -> Ptr <git_indexer_stats> -> IO (CInt)
#ccall git_indexer_write , Ptr <git_indexer> -> IO (CInt)
#ccall git_indexer_hash , Ptr <git_indexer> -> IO (Ptr <git_oid>)
#ccall git_indexer_free , Ptr <git_indexer> -> IO ()
