{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/checkout.h>
module Bindings.Libgit2.Checkout where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Diff
import Bindings.Libgit2.Strarray
{- typedef enum {
            GIT_CHECKOUT_NONE = 0,
            GIT_CHECKOUT_SAFE = 1u << 0,
            GIT_CHECKOUT_FORCE = 1u << 1,
            GIT_CHECKOUT_RECREATE_MISSING = 1u << 2,
            GIT_CHECKOUT_ALLOW_CONFLICTS = 1u << 4,
            GIT_CHECKOUT_REMOVE_UNTRACKED = 1u << 5,
            GIT_CHECKOUT_REMOVE_IGNORED = 1u << 6,
            GIT_CHECKOUT_UPDATE_ONLY = 1u << 7,
            GIT_CHECKOUT_DONT_UPDATE_INDEX = 1u << 8,
            GIT_CHECKOUT_NO_REFRESH = 1u << 9,
            GIT_CHECKOUT_SKIP_UNMERGED = 1u << 10,
            GIT_CHECKOUT_USE_OURS = 1u << 11,
            GIT_CHECKOUT_USE_THEIRS = 1u << 12,
            GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH = 1u << 13,
            GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES = 1u << 18,
            GIT_CHECKOUT_DONT_OVERWRITE_IGNORED = 1u << 19,
            GIT_CHECKOUT_CONFLICT_STYLE_MERGE = 1u << 20,
            GIT_CHECKOUT_CONFLICT_STYLE_DIFF3 = 1u << 21,
            GIT_CHECKOUT_DONT_REMOVE_EXISTING = 1u << 22,
            GIT_CHECKOUT_DONT_WRITE_INDEX = 1u << 23,
            GIT_CHECKOUT_DRY_RUN = 1u << 24,
            GIT_CHECKOUT_CONFLICT_STYLE_ZDIFF3 = 1u << 25,
            GIT_CHECKOUT_UPDATE_SUBMODULES = 1u << 16,
            GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED = 1u << 17
        } git_checkout_strategy_t; -}
#integral_t git_checkout_strategy_t
#num GIT_CHECKOUT_NONE
#num GIT_CHECKOUT_SAFE
#num GIT_CHECKOUT_FORCE
#num GIT_CHECKOUT_RECREATE_MISSING
#num GIT_CHECKOUT_ALLOW_CONFLICTS
#num GIT_CHECKOUT_REMOVE_UNTRACKED
#num GIT_CHECKOUT_REMOVE_IGNORED
#num GIT_CHECKOUT_UPDATE_ONLY
#num GIT_CHECKOUT_DONT_UPDATE_INDEX
#num GIT_CHECKOUT_NO_REFRESH
#num GIT_CHECKOUT_SKIP_UNMERGED
#num GIT_CHECKOUT_USE_OURS
#num GIT_CHECKOUT_USE_THEIRS
#num GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH
#num GIT_CHECKOUT_SKIP_LOCKED_DIRECTORIES
#num GIT_CHECKOUT_DONT_OVERWRITE_IGNORED
#num GIT_CHECKOUT_CONFLICT_STYLE_MERGE
#num GIT_CHECKOUT_CONFLICT_STYLE_DIFF3
#num GIT_CHECKOUT_DONT_REMOVE_EXISTING
#num GIT_CHECKOUT_DONT_WRITE_INDEX
#num GIT_CHECKOUT_DRY_RUN
#num GIT_CHECKOUT_CONFLICT_STYLE_ZDIFF3
#num GIT_CHECKOUT_UPDATE_SUBMODULES
#num GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED
{- typedef enum {
            GIT_CHECKOUT_NOTIFY_NONE = 0,
            GIT_CHECKOUT_NOTIFY_CONFLICT = 1u << 0,
            GIT_CHECKOUT_NOTIFY_DIRTY = 1u << 1,
            GIT_CHECKOUT_NOTIFY_UPDATED = 1u << 2,
            GIT_CHECKOUT_NOTIFY_UNTRACKED = 1u << 3,
            GIT_CHECKOUT_NOTIFY_IGNORED = 1u << 4,
            GIT_CHECKOUT_NOTIFY_ALL = 0xffffu
        } git_checkout_notify_t; -}
#integral_t git_checkout_notify_t
#num GIT_CHECKOUT_NOTIFY_NONE
#num GIT_CHECKOUT_NOTIFY_CONFLICT
#num GIT_CHECKOUT_NOTIFY_DIRTY
#num GIT_CHECKOUT_NOTIFY_UPDATED
#num GIT_CHECKOUT_NOTIFY_UNTRACKED
#num GIT_CHECKOUT_NOTIFY_IGNORED
#num GIT_CHECKOUT_NOTIFY_ALL
{- typedef struct {
            size_t mkdir_calls; size_t stat_calls; size_t chmod_calls;
        } git_checkout_perfdata; -}
#starttype git_checkout_perfdata
#field mkdir_calls , CSize
#field stat_calls , CSize
#field chmod_calls , CSize
#stoptype
#callback git_checkout_notify_cb , <git_checkout_notify_t> -> CString -> Ptr <git_diff_file> -> Ptr <git_diff_file> -> Ptr <git_diff_file> -> Ptr () -> IO CInt
#callback git_checkout_progress_cb , CString -> CSize -> CSize -> Ptr () -> IO ()
#callback git_checkout_perfdata_cb , Ptr <git_checkout_perfdata> -> Ptr () -> IO ()
{- typedef struct git_checkout_options {
            unsigned int version;
            unsigned int checkout_strategy;
            int disable_filters;
            unsigned int dir_mode;
            unsigned int file_mode;
            int file_open_flags;
            unsigned int notify_flags;
            git_checkout_notify_cb notify_cb;
            void * notify_payload;
            git_checkout_progress_cb progress_cb;
            void * progress_payload;
            git_strarray paths;
            git_tree * baseline;
            git_index * baseline_index;
            const char * target_directory;
            const char * ancestor_label;
            const char * our_label;
            const char * their_label;
            git_checkout_perfdata_cb perfdata_cb;
            void * perfdata_payload;
        } git_checkout_options; -}
#starttype struct git_checkout_options
#field version , CUInt
#field checkout_strategy , CUInt
#field disable_filters , CInt
#field dir_mode , CUInt
#field file_mode , CUInt
#field file_open_flags , CInt
#field notify_flags , CUInt
#field notify_cb , <git_checkout_notify_cb>
#field notify_payload , Ptr ()
#field progress_cb , <git_checkout_progress_cb>
#field progress_payload , Ptr ()
#field paths , <struct git_strarray>
#field baseline , Ptr <struct git_tree>
#field baseline_index , Ptr <struct git_index>
#field target_directory , CString
#field ancestor_label , CString
#field our_label , CString
#field their_label , CString
#field perfdata_cb , <git_checkout_perfdata_cb>
#field perfdata_payload , Ptr ()
#stoptype
#ccall git_checkout_options_init , Ptr <struct git_checkout_options> -> CUInt -> IO CInt
#ccall git_checkout_head , Ptr <struct git_repository> -> Ptr <struct git_checkout_options> -> IO CInt
#ccall git_checkout_index , Ptr <struct git_repository> -> Ptr <struct git_index> -> Ptr <struct git_checkout_options> -> IO CInt
#ccall git_checkout_tree , Ptr <struct git_repository> -> Ptr <struct git_object> -> Ptr <struct git_checkout_options> -> IO CInt
