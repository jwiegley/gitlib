#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Status where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- enum {
    GIT_STATUS_CURRENT = 0,
    GIT_STATUS_INDEX_NEW = 1 << 0,
    GIT_STATUS_INDEX_MODIFIED = 1 << 1,
    GIT_STATUS_INDEX_DELETED = 1 << 2,
    GIT_STATUS_WT_NEW = 1 << 3,
    GIT_STATUS_WT_MODIFIED = 1 << 4,
    GIT_STATUS_WT_DELETED = 1 << 5,
    GIT_STATUS_IGNORED = 1 << 6
}; -}
#num GIT_STATUS_CURRENT
#num GIT_STATUS_INDEX_NEW
#num GIT_STATUS_INDEX_MODIFIED
#num GIT_STATUS_INDEX_DELETED
#num GIT_STATUS_WT_NEW
#num GIT_STATUS_WT_MODIFIED
#num GIT_STATUS_WT_DELETED
#num GIT_STATUS_IGNORED
#callback git_status_foreach_callback , CString -> CUInt -> Ptr () -> IO CInt
#ccall git_status_foreach , Ptr <git_repository> -> <git_status_foreach_callback> -> Ptr () -> IO (CInt)
{- typedef enum {
            GIT_STATUS_SHOW_INDEX_AND_WORKDIR = 0,
            GIT_STATUS_SHOW_INDEX_ONLY = 1,
            GIT_STATUS_SHOW_WORKDIR_ONLY = 2,
            GIT_STATUS_SHOW_INDEX_THEN_WORKDIR = 3
        } git_status_show_t; -}
#integral_t git_status_show_t
#num GIT_STATUS_SHOW_INDEX_AND_WORKDIR
#num GIT_STATUS_SHOW_INDEX_ONLY
#num GIT_STATUS_SHOW_WORKDIR_ONLY
#num GIT_STATUS_SHOW_INDEX_THEN_WORKDIR
{- enum {
    GIT_STATUS_OPT_INCLUDE_UNTRACKED = 1 << 0,
    GIT_STATUS_OPT_INCLUDE_IGNORED = 1 << 1,
    GIT_STATUS_OPT_INCLUDE_UNMODIFIED = 1 << 2,
    GIT_STATUS_OPT_EXCLUDE_SUBMODULED = 1 << 3,
    GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS = 1 << 4
}; -}
#num GIT_STATUS_OPT_INCLUDE_UNTRACKED
#num GIT_STATUS_OPT_INCLUDE_IGNORED
#num GIT_STATUS_OPT_INCLUDE_UNMODIFIED
#num GIT_STATUS_OPT_EXCLUDE_SUBMODULED
#num GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS
{- typedef struct {
            git_status_show_t show; unsigned int flags; git_strarray pathspec;
        } git_status_options; -}
#starttype git_status_options
#field show , <git_status_show_t>
#field flags , CUInt
#field pathspec , <git_strarray>
#stoptype
#callback git_status_foreach_ext_callback , CString -> CUInt -> Ptr () -> IO CInt
#ccall git_status_foreach_ext , Ptr <git_repository> -> Ptr <git_status_options> -> <git_status_foreach_ext_callback> -> Ptr () -> IO (CInt)
#ccall git_status_file , Ptr CUInt -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_status_should_ignore , Ptr CInt -> Ptr <git_repository> -> CString -> IO (CInt)
