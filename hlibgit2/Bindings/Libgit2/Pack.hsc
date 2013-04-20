{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/pack.h>
module Bindings.Libgit2.Pack where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_packbuilder_new , Ptr (Ptr <git_packbuilder>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_packbuilder_set_threads , Ptr <git_packbuilder> -> CUInt -> IO (CUInt)
#ccall git_packbuilder_insert , Ptr <git_packbuilder> -> Ptr <git_oid> -> CString -> IO (CInt)
#ccall git_packbuilder_insert_tree , Ptr <git_packbuilder> -> Ptr <git_oid> -> IO (CInt)
#ccall git_packbuilder_write , Ptr <git_packbuilder> -> CString -> IO (CInt)
{- typedef int (* git_packbuilder_foreach_cb)(void * buf,
                                           size_t size,
                                           void * payload); -}
#callback git_packbuilder_foreach_cb , Ptr () -> CSize -> Ptr () -> IO CInt
#ccall git_packbuilder_foreach , Ptr <git_packbuilder> -> <git_packbuilder_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_packbuilder_object_count , Ptr <git_packbuilder> -> IO (CUInt)
#ccall git_packbuilder_written , Ptr <git_packbuilder> -> IO (CUInt)
#ccall git_packbuilder_free , Ptr <git_packbuilder> -> IO ()
