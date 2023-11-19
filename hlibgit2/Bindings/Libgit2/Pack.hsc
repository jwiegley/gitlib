{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/pack.h>
module Bindings.Libgit2.Pack where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Types
import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Buffer
{- typedef enum {
            GIT_PACKBUILDER_ADDING_OBJECTS = 0,
            GIT_PACKBUILDER_DELTAFICATION = 1
        } git_packbuilder_stage_t; -}
#integral_t git_packbuilder_stage_t
#num GIT_PACKBUILDER_ADDING_OBJECTS
#num GIT_PACKBUILDER_DELTAFICATION
#ccall git_packbuilder_new , Ptr (Ptr <struct git_packbuilder>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_packbuilder_set_threads , Ptr <struct git_packbuilder> -> CUInt -> IO CUInt
#ccall git_packbuilder_insert , Ptr <struct git_packbuilder> -> Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_packbuilder_insert_tree , Ptr <struct git_packbuilder> -> Ptr <struct git_oid> -> IO CInt
#ccall git_packbuilder_insert_commit , Ptr <struct git_packbuilder> -> Ptr <struct git_oid> -> IO CInt
#ccall git_packbuilder_insert_walk , Ptr <struct git_packbuilder> -> Ptr <struct git_revwalk> -> IO CInt
#ccall git_packbuilder_insert_recur , Ptr <struct git_packbuilder> -> Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_packbuilder_write_buf , Ptr <git_buf> -> Ptr <struct git_packbuilder> -> IO CInt
#ccall git_packbuilder_write , Ptr <struct git_packbuilder> -> CString -> CUInt -> <git_indexer_progress_cb> -> Ptr () -> IO CInt
#ccall git_packbuilder_hash , Ptr <struct git_packbuilder> -> IO (Ptr <struct git_oid>)
#ccall git_packbuilder_name , Ptr <struct git_packbuilder> -> IO CString
#callback git_packbuilder_foreach_cb , Ptr () -> CSize -> Ptr () -> IO CInt
#ccall git_packbuilder_foreach , Ptr <struct git_packbuilder> -> <git_packbuilder_foreach_cb> -> Ptr () -> IO CInt
#ccall git_packbuilder_object_count , Ptr <struct git_packbuilder> -> IO CSize
#ccall git_packbuilder_written , Ptr <struct git_packbuilder> -> IO CSize
#callback git_packbuilder_progress , CInt -> CUInt -> CUInt -> Ptr () -> IO CInt
#ccall git_packbuilder_set_callbacks , Ptr <struct git_packbuilder> -> <git_packbuilder_progress> -> Ptr () -> IO CInt
#ccall git_packbuilder_free , Ptr <struct git_packbuilder> -> IO ()
