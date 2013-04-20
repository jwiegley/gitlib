{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Submodule where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef struct git_submodule git_submodule; -}
#opaque_t git_submodule
{- typedef enum {
            GIT_SUBMODULE_UPDATE_DEFAULT = -1,
            GIT_SUBMODULE_UPDATE_CHECKOUT = 0,
            GIT_SUBMODULE_UPDATE_REBASE = 1,
            GIT_SUBMODULE_UPDATE_MERGE = 2,
            GIT_SUBMODULE_UPDATE_NONE = 3
        } git_submodule_update_t; -}
#integral_t git_submodule_update_t
#num GIT_SUBMODULE_UPDATE_DEFAULT
#num GIT_SUBMODULE_UPDATE_CHECKOUT
#num GIT_SUBMODULE_UPDATE_REBASE
#num GIT_SUBMODULE_UPDATE_MERGE
#num GIT_SUBMODULE_UPDATE_NONE
{- typedef enum {
            GIT_SUBMODULE_IGNORE_DEFAULT = -1,
            GIT_SUBMODULE_IGNORE_NONE = 0,
            GIT_SUBMODULE_IGNORE_UNTRACKED = 1,
            GIT_SUBMODULE_IGNORE_DIRTY = 2,
            GIT_SUBMODULE_IGNORE_ALL = 3
        } git_submodule_ignore_t; -}
#integral_t git_submodule_ignore_t
#num GIT_SUBMODULE_IGNORE_DEFAULT
#num GIT_SUBMODULE_IGNORE_NONE
#num GIT_SUBMODULE_IGNORE_UNTRACKED
#num GIT_SUBMODULE_IGNORE_DIRTY
#num GIT_SUBMODULE_IGNORE_ALL
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
#ccall git_submodule_lookup , Ptr (Ptr <git_submodule>) -> Ptr <git_repository> -> CString -> IO (CInt)
#callback git_submodule_foreach_callback , Ptr (<git_submodule>) -> CString -> Ptr () -> IO CInt
#ccall git_submodule_foreach , Ptr <git_repository> -> <git_submodule_foreach_callback> -> Ptr () -> IO (CInt)
#ccall git_submodule_add_setup , Ptr (Ptr <git_submodule>) -> Ptr <git_repository> -> CString -> CString -> CInt -> IO (CInt)
#ccall git_submodule_add_finalize , Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_add_to_index , Ptr <git_submodule> -> CInt -> IO (CInt)
#ccall git_submodule_save , Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_owner , Ptr <git_submodule> -> IO (Ptr <git_repository>)
#ccall git_submodule_name , Ptr <git_submodule> -> IO (CString)
#ccall git_submodule_path , Ptr <git_submodule> -> IO (CString)
#ccall git_submodule_url , Ptr <git_submodule> -> IO (CString)
#ccall git_submodule_set_url , Ptr <git_submodule> -> CString -> IO (CInt)
#ccall git_submodule_index_id , Ptr <git_submodule> -> IO (Ptr <git_oid>)
#ccall git_submodule_head_id , Ptr <git_submodule> -> IO (Ptr <git_oid>)
#ccall git_submodule_wd_id , Ptr <git_submodule> -> IO (Ptr <git_oid>)
#ccall git_submodule_ignore , Ptr <git_submodule> -> IO (<git_submodule_ignore_t>)
#ccall git_submodule_set_ignore , Ptr <git_submodule> -> <git_submodule_ignore_t> -> IO (<git_submodule_ignore_t>)
#ccall git_submodule_update , Ptr <git_submodule> -> IO (<git_submodule_update_t>)
#ccall git_submodule_set_update , Ptr <git_submodule> -> <git_submodule_update_t> -> IO (<git_submodule_update_t>)
#ccall git_submodule_fetch_recurse_submodules , Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_set_fetch_recurse_submodules , Ptr <git_submodule> -> CInt -> IO (CInt)
#ccall git_submodule_init , Ptr <git_submodule> -> CInt -> IO (CInt)
#ccall git_submodule_sync , Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_open , Ptr (Ptr <git_repository>) -> Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_reload , Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_reload_all , Ptr <git_repository> -> IO (CInt)
#ccall git_submodule_status , Ptr CUInt -> Ptr <git_submodule> -> IO (CInt)
#ccall git_submodule_location , Ptr CUInt -> Ptr <git_submodule> -> IO (CInt)
