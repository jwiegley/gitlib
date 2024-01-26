{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/submodule.h>
module Bindings.Libgit2.Submodule where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Remote
import Bindings.Libgit2.Checkout
import Bindings.Libgit2.Buffer
{- typedef enum {
            GIT_SUBMODULE_STATUS_IN_HEAD = 1u << 0,
            GIT_SUBMODULE_STATUS_IN_INDEX = 1u << 1,
            GIT_SUBMODULE_STATUS_IN_CONFIG = 1u << 2,
            GIT_SUBMODULE_STATUS_IN_WD = 1u << 3,
            GIT_SUBMODULE_STATUS_INDEX_ADDED = 1u << 4,
            GIT_SUBMODULE_STATUS_INDEX_DELETED = 1u << 5,
            GIT_SUBMODULE_STATUS_INDEX_MODIFIED = 1u << 6,
            GIT_SUBMODULE_STATUS_WD_UNINITIALIZED = 1u << 7,
            GIT_SUBMODULE_STATUS_WD_ADDED = 1u << 8,
            GIT_SUBMODULE_STATUS_WD_DELETED = 1u << 9,
            GIT_SUBMODULE_STATUS_WD_MODIFIED = 1u << 10,
            GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED = 1u << 11,
            GIT_SUBMODULE_STATUS_WD_WD_MODIFIED = 1u << 12,
            GIT_SUBMODULE_STATUS_WD_UNTRACKED = 1u << 13
        } git_submodule_status_t; -}
#integral_t git_submodule_status_t
#num GIT_SUBMODULE_STATUS_IN_HEAD
#num GIT_SUBMODULE_STATUS_IN_INDEX
#num GIT_SUBMODULE_STATUS_IN_CONFIG
#num GIT_SUBMODULE_STATUS_IN_WD
#num GIT_SUBMODULE_STATUS_INDEX_ADDED
#num GIT_SUBMODULE_STATUS_INDEX_DELETED
#num GIT_SUBMODULE_STATUS_INDEX_MODIFIED
#num GIT_SUBMODULE_STATUS_WD_UNINITIALIZED
#num GIT_SUBMODULE_STATUS_WD_ADDED
#num GIT_SUBMODULE_STATUS_WD_DELETED
#num GIT_SUBMODULE_STATUS_WD_MODIFIED
#num GIT_SUBMODULE_STATUS_WD_INDEX_MODIFIED
#num GIT_SUBMODULE_STATUS_WD_WD_MODIFIED
#num GIT_SUBMODULE_STATUS_WD_UNTRACKED
#callback git_submodule_cb , Ptr <struct git_submodule> -> CString -> Ptr () -> IO CInt
{- typedef struct git_submodule_update_options {
            unsigned int version;
            git_checkout_options checkout_opts;
            git_fetch_options fetch_opts;
            int allow_fetch;
        } git_submodule_update_options; -}
#starttype struct git_submodule_update_options
#field version , CUInt
#field checkout_opts , <struct git_checkout_options>
#field fetch_opts , <git_fetch_options>
#field allow_fetch , CInt
#stoptype
#ccall git_submodule_update_options_init , Ptr <struct git_submodule_update_options> -> CUInt -> IO CInt
#ccall git_submodule_update , Ptr <struct git_submodule> -> CInt -> Ptr <struct git_submodule_update_options> -> IO CInt
#ccall git_submodule_lookup , Ptr (Ptr <struct git_submodule>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_submodule_dup , Ptr (Ptr <struct git_submodule>) -> Ptr <struct git_submodule> -> IO CInt
#ccall git_submodule_free , Ptr <struct git_submodule> -> IO ()
#ccall git_submodule_foreach , Ptr <struct git_repository> -> <git_submodule_cb> -> Ptr () -> IO CInt
#ccall git_submodule_add_setup , Ptr (Ptr <struct git_submodule>) -> Ptr <struct git_repository> -> CString -> CString -> CInt -> IO CInt
#ccall git_submodule_clone , Ptr (Ptr <struct git_repository>) -> Ptr <struct git_submodule> -> Ptr <struct git_submodule_update_options> -> IO CInt
#ccall git_submodule_add_finalize , Ptr <struct git_submodule> -> IO CInt
#ccall git_submodule_add_to_index , Ptr <struct git_submodule> -> CInt -> IO CInt
#ccall git_submodule_owner , Ptr <struct git_submodule> -> IO (Ptr <struct git_repository>)
#ccall git_submodule_name , Ptr <struct git_submodule> -> IO CString
#ccall git_submodule_path , Ptr <struct git_submodule> -> IO CString
#ccall git_submodule_url , Ptr <struct git_submodule> -> IO CString
#ccall git_submodule_resolve_url , Ptr <git_buf> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_submodule_branch , Ptr <struct git_submodule> -> IO CString
#ccall git_submodule_set_branch , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_submodule_set_url , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_submodule_index_id , Ptr <struct git_submodule> -> IO (Ptr <struct git_oid>)
#ccall git_submodule_head_id , Ptr <struct git_submodule> -> IO (Ptr <struct git_oid>)
#ccall git_submodule_wd_id , Ptr <struct git_submodule> -> IO (Ptr <struct git_oid>)
#ccall git_submodule_ignore , Ptr <struct git_submodule> -> IO <git_submodule_ignore_t>
#ccall git_submodule_set_ignore , Ptr <struct git_repository> -> CString -> <git_submodule_ignore_t> -> IO CInt
#ccall git_submodule_update_strategy , Ptr <struct git_submodule> -> IO <git_submodule_update_t>
#ccall git_submodule_set_update , Ptr <struct git_repository> -> CString -> <git_submodule_update_t> -> IO CInt
#ccall git_submodule_fetch_recurse_submodules , Ptr <struct git_submodule> -> IO <git_submodule_recurse_t>
#ccall git_submodule_set_fetch_recurse_submodules , Ptr <struct git_repository> -> CString -> <git_submodule_recurse_t> -> IO CInt
#ccall git_submodule_init , Ptr <struct git_submodule> -> CInt -> IO CInt
#ccall git_submodule_repo_init , Ptr (Ptr <struct git_repository>) -> Ptr <struct git_submodule> -> CInt -> IO CInt
#ccall git_submodule_sync , Ptr <struct git_submodule> -> IO CInt
#ccall git_submodule_open , Ptr (Ptr <struct git_repository>) -> Ptr <struct git_submodule> -> IO CInt
#ccall git_submodule_reload , Ptr <struct git_submodule> -> CInt -> IO CInt
#ccall git_submodule_status , Ptr CUInt -> Ptr <struct git_repository> -> CString -> <git_submodule_ignore_t> -> IO CInt
#ccall git_submodule_location , Ptr CUInt -> Ptr <struct git_submodule> -> IO CInt
