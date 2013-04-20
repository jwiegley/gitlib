{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Branch where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
#ccall git_branch_create , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> Ptr <git_commit> -> CInt -> IO (CInt)
#ccall git_branch_delete , Ptr <git_reference> -> IO (CInt)
{- typedef int (* git_branch_foreach_cb)(const char * branch_name,
                                      git_branch_t branch_type,
                                      void * payload); -}
#callback git_branch_foreach_cb , CString -> <git_branch_t> -> Ptr () -> IO (CInt)
#ccall git_branch_foreach , Ptr <git_repository> -> CUInt -> <git_branch_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_branch_move , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> CString -> CInt -> IO (CInt)
#ccall git_branch_lookup , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> <git_branch_t> -> IO (CInt)
#ccall git_branch_name , Ptr (CString) -> Ptr <git_reference> -> IO (CInt)
#ccall git_branch_upstream , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> IO (CInt)
#ccall git_branch_set_upstream , Ptr <git_reference> -> CString -> IO (CInt)
#ccall git_branch_upstream_name , CString -> CSize -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_branch_is_head , Ptr <git_reference> -> IO (CInt)
#ccall git_branch_remote_name , CString -> CSize -> Ptr <git_repository> -> CString -> IO (CInt)
