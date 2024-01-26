{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/status.h>
module Bindings.Libgit2.Status where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Diff
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
            GIT_STATUS_WT_RENAMED = 1u << 11,
            GIT_STATUS_WT_UNREADABLE = 1u << 12,
            GIT_STATUS_IGNORED = 1u << 14,
            GIT_STATUS_CONFLICTED = 1u << 15
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
#num GIT_STATUS_WT_RENAMED
#num GIT_STATUS_WT_UNREADABLE
#num GIT_STATUS_IGNORED
#num GIT_STATUS_CONFLICTED
#callback git_status_cb , CString -> CUInt -> Ptr () -> IO CInt
{- typedef enum {
            GIT_STATUS_SHOW_INDEX_AND_WORKDIR = 0,
            GIT_STATUS_SHOW_INDEX_ONLY = 1,
            GIT_STATUS_SHOW_WORKDIR_ONLY = 2
        } git_status_show_t; -}
#integral_t git_status_show_t
#num GIT_STATUS_SHOW_INDEX_AND_WORKDIR
#num GIT_STATUS_SHOW_INDEX_ONLY
#num GIT_STATUS_SHOW_WORKDIR_ONLY
{- typedef enum {
            GIT_STATUS_OPT_INCLUDE_UNTRACKED = 1u << 0,
            GIT_STATUS_OPT_INCLUDE_IGNORED = 1u << 1,
            GIT_STATUS_OPT_INCLUDE_UNMODIFIED = 1u << 2,
            GIT_STATUS_OPT_EXCLUDE_SUBMODULES = 1u << 3,
            GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS = 1u << 4,
            GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH = 1u << 5,
            GIT_STATUS_OPT_RECURSE_IGNORED_DIRS = 1u << 6,
            GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX = 1u << 7,
            GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR = 1u << 8,
            GIT_STATUS_OPT_SORT_CASE_SENSITIVELY = 1u << 9,
            GIT_STATUS_OPT_SORT_CASE_INSENSITIVELY = 1u << 10,
            GIT_STATUS_OPT_RENAMES_FROM_REWRITES = 1u << 11,
            GIT_STATUS_OPT_NO_REFRESH = 1u << 12,
            GIT_STATUS_OPT_UPDATE_INDEX = 1u << 13,
            GIT_STATUS_OPT_INCLUDE_UNREADABLE = 1u << 14,
            GIT_STATUS_OPT_INCLUDE_UNREADABLE_AS_UNTRACKED = 1u << 15
        } git_status_opt_t; -}
#integral_t git_status_opt_t
#num GIT_STATUS_OPT_INCLUDE_UNTRACKED
#num GIT_STATUS_OPT_INCLUDE_IGNORED
#num GIT_STATUS_OPT_INCLUDE_UNMODIFIED
#num GIT_STATUS_OPT_EXCLUDE_SUBMODULES
#num GIT_STATUS_OPT_RECURSE_UNTRACKED_DIRS
#num GIT_STATUS_OPT_DISABLE_PATHSPEC_MATCH
#num GIT_STATUS_OPT_RECURSE_IGNORED_DIRS
#num GIT_STATUS_OPT_RENAMES_HEAD_TO_INDEX
#num GIT_STATUS_OPT_RENAMES_INDEX_TO_WORKDIR
#num GIT_STATUS_OPT_SORT_CASE_SENSITIVELY
#num GIT_STATUS_OPT_SORT_CASE_INSENSITIVELY
#num GIT_STATUS_OPT_RENAMES_FROM_REWRITES
#num GIT_STATUS_OPT_NO_REFRESH
#num GIT_STATUS_OPT_UPDATE_INDEX
#num GIT_STATUS_OPT_INCLUDE_UNREADABLE
#num GIT_STATUS_OPT_INCLUDE_UNREADABLE_AS_UNTRACKED
{- typedef struct {
            unsigned int version;
            git_status_show_t show;
            unsigned int flags;
            git_strarray pathspec;
            git_tree * baseline;
            uint16_t rename_threshold;
        } git_status_options; -}
#starttype git_status_options
#field version , CUInt
#field show , <git_status_show_t>
#field flags , CUInt
#field pathspec , <struct git_strarray>
#field baseline , Ptr <struct git_tree>
#field rename_threshold , CUInt
#stoptype
#ccall git_status_options_init , Ptr <git_status_options> -> CUInt -> IO CInt
{- typedef struct {
            git_status_t status;
            git_diff_delta * head_to_index;
            git_diff_delta * index_to_workdir;
        } git_status_entry; -}
#starttype git_status_entry
#field status , <git_status_t>
#field head_to_index , Ptr <git_diff_delta>
#field index_to_workdir , Ptr <git_diff_delta>
#stoptype
#ccall git_status_foreach , Ptr <struct git_repository> -> <git_status_cb> -> Ptr () -> IO CInt
#ccall git_status_foreach_ext , Ptr <struct git_repository> -> Ptr <git_status_options> -> <git_status_cb> -> Ptr () -> IO CInt
#ccall git_status_file , Ptr CUInt -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_status_list_new , Ptr (Ptr <struct git_status_list>) -> Ptr <struct git_repository> -> Ptr <git_status_options> -> IO CInt
#ccall git_status_list_entrycount , Ptr <struct git_status_list> -> IO CSize
#ccall git_status_byindex , Ptr <struct git_status_list> -> CSize -> IO (Ptr <git_status_entry>)
#ccall git_status_list_free , Ptr <struct git_status_list> -> IO ()
#ccall git_status_should_ignore , Ptr CInt -> Ptr <struct git_repository> -> CString -> IO CInt
