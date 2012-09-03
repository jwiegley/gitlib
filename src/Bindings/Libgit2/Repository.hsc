
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Repository where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_repository_open , Ptr (Ptr <git_repository>) -> CString -> IO (CInt)
#ccall git_repository_open2 , Ptr (Ptr <git_repository>) -> CString -> CString -> CString -> CString -> IO (CInt)
#ccall git_repository_open3 , Ptr (Ptr <git_repository>) -> CString -> Ptr <git_odb> -> CString -> CString -> IO (CInt)
#ccall git_repository_discover , CString -> CSize -> CString -> CInt -> CString -> IO (CInt)
#ccall git_repository_database , Ptr <git_repository> -> IO (Ptr <git_odb>)
#ccall git_repository_index , Ptr (Ptr <git_index>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_free , Ptr <git_repository> -> IO ()
#ccall git_repository_init , Ptr (Ptr <git_repository>) -> CString -> CUInt -> IO (CInt)
#ccall git_repository_head_detached , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_head_orphan , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_is_empty , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_path , Ptr <git_repository> -> IO (CString)
#ccall git_repository_is_bare , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_config , Ptr (Ptr <git_config>) -> Ptr <git_repository> -> CString -> CString -> IO (CInt)
