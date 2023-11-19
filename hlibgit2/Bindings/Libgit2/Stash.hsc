{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/stash.h>
module Bindings.Libgit2.Stash where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Checkout
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Oid
{- typedef enum {
            GIT_STASH_DEFAULT = 0,
            GIT_STASH_KEEP_INDEX = 1 << 0,
            GIT_STASH_INCLUDE_UNTRACKED = 1 << 1,
            GIT_STASH_INCLUDE_IGNORED = 1 << 2,
            GIT_STASH_KEEP_ALL = 1 << 3
        } git_stash_flags; -}
#integral_t git_stash_flags
#num GIT_STASH_DEFAULT
#num GIT_STASH_KEEP_INDEX
#num GIT_STASH_INCLUDE_UNTRACKED
#num GIT_STASH_INCLUDE_IGNORED
#num GIT_STASH_KEEP_ALL
#ccall git_stash_save , Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr <struct git_signature> -> CString -> CUInt -> IO CInt
{- typedef struct git_stash_save_options {
            unsigned int version;
            uint32_t flags;
            const git_signature * stasher;
            const char * message;
            git_strarray paths;
        } git_stash_save_options; -}
#starttype struct git_stash_save_options
#field version , CUInt
#field flags , CUInt
#field stasher , Ptr <struct git_signature>
#field message , CString
#field paths , <struct git_strarray>
#stoptype
#ccall git_stash_save_options_init , Ptr <struct git_stash_save_options> -> CUInt -> IO CInt
#ccall git_stash_save_with_opts , Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr <struct git_stash_save_options> -> IO CInt
{- typedef enum {
            GIT_STASH_APPLY_DEFAULT = 0,
            GIT_STASH_APPLY_REINSTATE_INDEX = 1 << 0
        } git_stash_apply_flags; -}
#integral_t git_stash_apply_flags
#num GIT_STASH_APPLY_DEFAULT
#num GIT_STASH_APPLY_REINSTATE_INDEX
{- typedef enum {
            GIT_STASH_APPLY_PROGRESS_NONE = 0,
            GIT_STASH_APPLY_PROGRESS_LOADING_STASH,
            GIT_STASH_APPLY_PROGRESS_ANALYZE_INDEX,
            GIT_STASH_APPLY_PROGRESS_ANALYZE_MODIFIED,
            GIT_STASH_APPLY_PROGRESS_ANALYZE_UNTRACKED,
            GIT_STASH_APPLY_PROGRESS_CHECKOUT_UNTRACKED,
            GIT_STASH_APPLY_PROGRESS_CHECKOUT_MODIFIED,
            GIT_STASH_APPLY_PROGRESS_DONE
        } git_stash_apply_progress_t; -}
#integral_t git_stash_apply_progress_t
#num GIT_STASH_APPLY_PROGRESS_NONE
#num GIT_STASH_APPLY_PROGRESS_LOADING_STASH
#num GIT_STASH_APPLY_PROGRESS_ANALYZE_INDEX
#num GIT_STASH_APPLY_PROGRESS_ANALYZE_MODIFIED
#num GIT_STASH_APPLY_PROGRESS_ANALYZE_UNTRACKED
#num GIT_STASH_APPLY_PROGRESS_CHECKOUT_UNTRACKED
#num GIT_STASH_APPLY_PROGRESS_CHECKOUT_MODIFIED
#num GIT_STASH_APPLY_PROGRESS_DONE
#callback git_stash_apply_progress_cb , <git_stash_apply_progress_t> -> Ptr () -> IO CInt
{- typedef struct git_stash_apply_options {
            unsigned int version;
            uint32_t flags;
            git_checkout_options checkout_options;
            git_stash_apply_progress_cb progress_cb;
            void * progress_payload;
        } git_stash_apply_options; -}
#starttype struct git_stash_apply_options
#field version , CUInt
#field flags , CUInt
#field checkout_options , <struct git_checkout_options>
#field progress_cb , <git_stash_apply_progress_cb>
#field progress_payload , Ptr ()
#stoptype
#ccall git_stash_apply_options_init , Ptr <struct git_stash_apply_options> -> CUInt -> IO CInt
#ccall git_stash_apply , Ptr <struct git_repository> -> CSize -> Ptr <struct git_stash_apply_options> -> IO CInt
#callback git_stash_cb , CSize -> CString -> Ptr <struct git_oid> -> Ptr () -> IO CInt
#ccall git_stash_foreach , Ptr <struct git_repository> -> <git_stash_cb> -> Ptr () -> IO CInt
#ccall git_stash_drop , Ptr <struct git_repository> -> CSize -> IO CInt
#ccall git_stash_pop , Ptr <struct git_repository> -> CSize -> Ptr <struct git_stash_apply_options> -> IO CInt
