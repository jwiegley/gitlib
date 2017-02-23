{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/remote.h>
module Bindings.Libgit2.Push where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- typedef struct {
            unsigned int version; unsigned int pb_parallelism;
        } git_push_options; -}
#starttype git_push_options
#field version , CUInt
#field pb_parallelism , CUInt
#stoptype
#ccall git_push_new , Ptr (Ptr <git_push>) -> Ptr <git_remote> -> IO (CInt)
#ccall git_push_set_options , Ptr <git_push> -> Ptr <git_push_options> -> IO (CInt)
#ccall git_push_add_refspec , Ptr <git_push> -> CString -> IO (CInt)
#ccall git_push_update_tips , Ptr <git_push> -> IO (CInt)
#ccall git_push_finish , Ptr <git_push> -> IO (CInt)
#ccall git_push_unpack_ok , Ptr <git_push> -> IO (CInt)
#ccall git_push_status_foreach , Ptr <git_push> -> FunPtr (CString -> CString -> Ptr () -> CInt) -> Ptr () -> IO (CInt)
#ccall git_push_free , Ptr <git_push> -> IO ()
