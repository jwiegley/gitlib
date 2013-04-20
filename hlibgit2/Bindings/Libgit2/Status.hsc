{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Status where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
{- typedef enum {
            GIT_STATUS_CURRENT = 0,
            GIT_STATUS_INDEX_NEW = 1u << 0,
            GIT_STATUS_INDEX_MODIFIED = 1u << 1,
            GIT_STATUS_INDEX_DELETED = 1u << 2,
            GIT_STATUS_INDEX_RENAMED = 1u << 3,
            GIT_STATUS_INDEX_TYPECHANGE = 1u << 4,
            GIT_STATUS_WT_NEW = 1u << 7,
            GIT_STATUS_WT_MODIFIED = 1u << 8,
            GIT_STATUS_WT_DELETED = 1u << 9,
            GIT_STATUS_WT_TYPECHANGE = 1u << 10,
            GIT_STATUS_IGNORED = 1u << 14
        } git_status_t; -}
#integral_t git_status_t
#num GIT_STATUS_CURRENT
#num GIT_STATUS_INDEX_NEW
#num GIT_STATUS_INDEX_MODIFIED
#num GIT_STATUS_INDEX_DELETED
#num GIT_STATUS_INDEX_RENAMED
#num GIT_STATUS_INDEX_TYPECHANGE
#num GIT_STATUS_WT_NEW
#num GIT_STATUS_WT_MODIFIED
#num GIT_STATUS_WT_DELETED
#num GIT_STATUS_WT_TYPECHANGE
#num GIT_STATUS_IGNORED
{- typedef int (* git_status_cb)(const char * path,
                              unsigned int status_flags,
                              void * payload); -}
#callback git_status_cb , CString -> CUInt -> Ptr () -> IO CInt
#ccall git_status_foreach , Ptr <git_repository> -> <git_status_cb> -> Ptr () -> IO (CInt)
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
{- typedef enum {
            GIT_STATUS_OPT_INCLUDE_UNTRACKED = 1u << 0,
            GIT_STATUS_OPT_INCLUDE_IGNORED = 1u << 1,
            GIT_STATUS_OPT_INCLUDE_UNMODIFIED = 1u << 2,
            GIT_STATUS_OPT_EXCLUDE_SUBMODULES = 1u << 3,
            GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS = 1u << 4,
            GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH = 1u << 5,
            GIT_STATUS_OPT_RECURSE_IGNORED_DIRS = 1u << 6
        } git_status_opt_t; -}
#integral_t git_status_opt_t
#num GIT_STATUS_OPT_INCLUDE_UNTRACKED
#num GIT_STATUS_OPT_INCLUDE_IGNORED
#num GIT_STATUS_OPT_INCLUDE_UNMODIFIED
#num GIT_STATUS_OPT_EXCLUDE_SUBMODULES
#num GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS
#num GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH
#num GIT_STATUS_OPT_RECURSE_IGNORED_DIRS
{- typedef struct {
            unsigned int version;
            git_status_show_t show;
            unsigned int flags;
            git_strarray pathspec;
        } git_status_options; -}
#starttype git_status_options
#field version , CUInt
#field show , <git_status_show_t>
#field flags , CUInt
#field pathspec , <git_strarray>
#stoptype
#ccall git_status_foreach_ext , Ptr <git_repository> -> Ptr <git_status_options> -> <git_status_cb> -> Ptr () -> IO (CInt)
#ccall git_status_file , Ptr CUInt -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_status_should_ignore , Ptr CInt -> Ptr <git_repository> -> CString -> IO (CInt)
