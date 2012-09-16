#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Branch where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
#ccall git_branch_create , Ptr <git_oid> -> Ptr <git_repository> -> CString -> Ptr <git_object> -> CInt -> IO (CInt)
#ccall git_branch_delete , Ptr <git_repository> -> CString -> <git_branch_t> -> IO (CInt)
#ccall git_branch_list , Ptr <git_strarray> -> Ptr <git_repository> -> CUInt -> IO (CInt)
#ccall git_branch_move , Ptr <git_repository> -> CString -> CString -> CInt -> IO (CInt)
