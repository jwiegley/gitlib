#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Repository where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_repository_open , Ptr (Ptr <git_repository>) -> CString -> IO (CInt)
#ccall git_repository_discover , CString -> CSize -> CString -> CInt -> CString -> IO (CInt)
{- enum {
    GIT_REPOSITORY_OPEN_NO_SEARCH = 1 << 0,
    GIT_REPOSITORY_OPEN_CROSS_FS = 1 << 1
}; -}
#num GIT_REPOSITORY_OPEN_NO_SEARCH
#num GIT_REPOSITORY_OPEN_CROSS_FS
#ccall git_repository_open_ext , Ptr (Ptr <git_repository>) -> CString -> CUInt -> CString -> IO (CInt)
#ccall git_repository_new , Ptr (Ptr <git_repository>) -> IO (CInt)
#ccall git_repository_free , Ptr <git_repository> -> IO ()
#ccall git_repository_init , Ptr (Ptr <git_repository>) -> CString -> CUInt -> IO (CInt)
#ccall git_repository_head , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_head_detached , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_head_orphan , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_is_empty , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_path , Ptr <git_repository> -> IO (CString)
#ccall git_repository_workdir , Ptr <git_repository> -> IO (CString)
#ccall git_repository_set_workdir , Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_repository_is_bare , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_config , Ptr (Ptr <git_config>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_config , Ptr <git_repository> -> Ptr <git_config> -> IO ()
#ccall git_repository_odb , Ptr (Ptr <git_odb>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_odb , Ptr <git_repository> -> Ptr <git_odb> -> IO ()
#ccall git_repository_index , Ptr (Ptr <git_index>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_index , Ptr <git_repository> -> Ptr <git_index> -> IO ()
