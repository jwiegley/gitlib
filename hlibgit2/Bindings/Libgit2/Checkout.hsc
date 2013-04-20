{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/checkout.h>
module Bindings.Libgit2.Checkout where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Diff
{- typedef enum {
            GIT_CHECKOUT_NONE = 0,
            GIT_CHECKOUT_SAFE = 1u << 0,
            GIT_CHECKOUT_SAFE_CREATE = 1u << 1,
            GIT_CHECKOUT_FORCE = 1u << 2,
            GIT_CHECKOUT_ALLOW_CONFLICTS = 1u << 4,
            GIT_CHECKOUT_REMOVE_UNTRACKED = 1u << 5,
            GIT_CHECKOUT_REMOVE_IGNORED = 1u << 6,
            GIT_CHECKOUT_UPDATE_ONLY = 1u << 7,
            GIT_CHECKOUT_DONT_UPDATE_INDEX = 1u << 8,
            GIT_CHECKOUT_NO_REFRESH = 1u << 9,
            GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH = 1u << 13,
            GIT_CHECKOUT_SKIP_UNMERGED = 1u << 10,
            GIT_CHECKOUT_USE_OURS = 1u << 11,
            GIT_CHECKOUT_USE_THEIRS = 1u << 12,
            GIT_CHECKOUT_UPDATE_SUBMODULES = 1u << 16,
            GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED = 1u << 17
        } git_checkout_strategy_t; -}
#integral_t git_checkout_strategy_t
#num GIT_CHECKOUT_NONE
#num GIT_CHECKOUT_SAFE
#num GIT_CHECKOUT_SAFE_CREATE
#num GIT_CHECKOUT_FORCE
#num GIT_CHECKOUT_ALLOW_CONFLICTS
#num GIT_CHECKOUT_REMOVE_UNTRACKED
#num GIT_CHECKOUT_REMOVE_IGNORED
#num GIT_CHECKOUT_UPDATE_ONLY
#num GIT_CHECKOUT_DONT_UPDATE_INDEX
#num GIT_CHECKOUT_NO_REFRESH
#num GIT_CHECKOUT_DISABLE_PATHSPEC_MATCH
#num GIT_CHECKOUT_SKIP_UNMERGED
#num GIT_CHECKOUT_USE_OURS
#num GIT_CHECKOUT_USE_THEIRS
#num GIT_CHECKOUT_UPDATE_SUBMODULES
#num GIT_CHECKOUT_UPDATE_SUBMODULES_IF_CHANGED
{- typedef enum {
            GIT_CHECKOUT_NOTIFY_NONE = 0,
            GIT_CHECKOUT_NOTIFY_CONFLICT = 1u << 0,
            GIT_CHECKOUT_NOTIFY_DIRTY = 1u << 1,
            GIT_CHECKOUT_NOTIFY_UPDATED = 1u << 2,
            GIT_CHECKOUT_NOTIFY_UNTRACKED = 1u << 3,
            GIT_CHECKOUT_NOTIFY_IGNORED = 1u << 4
        } git_checkout_notify_t; -}
#integral_t git_checkout_notify_t
#num GIT_CHECKOUT_NOTIFY_NONE
#num GIT_CHECKOUT_NOTIFY_CONFLICT
#num GIT_CHECKOUT_NOTIFY_DIRTY
#num GIT_CHECKOUT_NOTIFY_UPDATED
#num GIT_CHECKOUT_NOTIFY_UNTRACKED
#num GIT_CHECKOUT_NOTIFY_IGNORED
{- typedef int (* git_checkout_notify_cb)(git_checkout_notify_t why,
                                       const char * path,
                                       const git_diff_file * baseline,
                                       const git_diff_file * target,
                                       const git_diff_file * workdir,
                                       void * payload); -}
#callback git_checkout_notify_cb , <git_checkout_notify_t> -> CString -> Ptr (<git_diff_file>) -> Ptr (<git_diff_file>) -> Ptr (<git_diff_file>) -> Ptr () -> IO CInt
{- typedef void (* git_checkout_progress_cb)(const char * path,
                                          size_t completed_steps,
                                          size_t total_steps,
                                          void * payload); -}
#callback git_checkout_progress_cb , CString -> CSize -> CSize -> Ptr () -> IO ()
{- typedef struct git_checkout_opts {
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
        } git_checkout_opts; -}
#starttype git_checkout_opts
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
#field paths , <git_strarray>
#field baseline , Ptr <git_tree>
#stoptype
#ccall git_checkout_head , Ptr <git_repository> -> Ptr <git_checkout_opts> -> IO (CInt)
#ccall git_checkout_index , Ptr <git_repository> -> Ptr <git_index> -> Ptr <git_checkout_opts> -> IO (CInt)
#ccall git_checkout_tree , Ptr <git_repository> -> Ptr <git_object> -> Ptr <git_checkout_opts> -> IO (CInt)
