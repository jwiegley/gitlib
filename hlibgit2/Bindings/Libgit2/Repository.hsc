{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/repository.h>
module Bindings.Libgit2.Repository where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Buffer
#ccall git_repository_open , Ptr (Ptr <struct git_repository>) -> CString -> IO CInt
#ccall git_repository_open_from_worktree , Ptr (Ptr <struct git_repository>) -> Ptr <struct git_worktree> -> IO CInt
#ccall git_repository_wrap_odb , Ptr (Ptr <struct git_repository>) -> Ptr <struct git_odb> -> IO CInt
#ccall git_repository_discover , Ptr <git_buf> -> CString -> CInt -> CString -> IO CInt
{- typedef enum {
            GIT_REPOSITORY_OPEN_NO_SEARCH = 1 << 0,
            GIT_REPOSITORY_OPEN_CROSS_FS = 1 << 1,
            GIT_REPOSITORY_OPEN_BARE = 1 << 2,
            GIT_REPOSITORY_OPEN_NO_DOTGIT = 1 << 3,
            GIT_REPOSITORY_OPEN_FROM_ENV = 1 << 4
        } git_repository_open_flag_t; -}
#integral_t git_repository_open_flag_t
#num GIT_REPOSITORY_OPEN_NO_SEARCH
#num GIT_REPOSITORY_OPEN_CROSS_FS
#num GIT_REPOSITORY_OPEN_BARE
#num GIT_REPOSITORY_OPEN_NO_DOTGIT
#num GIT_REPOSITORY_OPEN_FROM_ENV
#ccall git_repository_open_ext , Ptr (Ptr <struct git_repository>) -> CString -> CUInt -> CString -> IO CInt
#ccall git_repository_open_bare , Ptr (Ptr <struct git_repository>) -> CString -> IO CInt
#ccall git_repository_free , Ptr <struct git_repository> -> IO ()
#ccall git_repository_init , Ptr (Ptr <struct git_repository>) -> CString -> CUInt -> IO CInt
{- typedef enum {
            GIT_REPOSITORY_INIT_BARE = 1u << 0,
            GIT_REPOSITORY_INIT_NO_REINIT = 1u << 1,
            GIT_REPOSITORY_INIT_NO_DOTGIT_DIR = 1u << 2,
            GIT_REPOSITORY_INIT_MKDIR = 1u << 3,
            GIT_REPOSITORY_INIT_MKPATH = 1u << 4,
            GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE = 1u << 5,
            GIT_REPOSITORY_INIT_RELATIVE_GITLINK = 1u << 6
        } git_repository_init_flag_t; -}
#integral_t git_repository_init_flag_t
#num GIT_REPOSITORY_INIT_BARE
#num GIT_REPOSITORY_INIT_NO_REINIT
#num GIT_REPOSITORY_INIT_NO_DOTGIT_DIR
#num GIT_REPOSITORY_INIT_MKDIR
#num GIT_REPOSITORY_INIT_MKPATH
#num GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE
#num GIT_REPOSITORY_INIT_RELATIVE_GITLINK
{- typedef enum {
            GIT_REPOSITORY_INIT_SHARED_UMASK = 0,
            GIT_REPOSITORY_INIT_SHARED_GROUP = 02775,
            GIT_REPOSITORY_INIT_SHARED_ALL = 02777
        } git_repository_init_mode_t; -}
#integral_t git_repository_init_mode_t
#num GIT_REPOSITORY_INIT_SHARED_UMASK
#num GIT_REPOSITORY_INIT_SHARED_GROUP
#num GIT_REPOSITORY_INIT_SHARED_ALL
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            uint32_t mode;
            const char * workdir_path;
            const char * description;
            const char * template_path;
            const char * initial_head;
            const char * origin_url;
        } git_repository_init_options; -}
#starttype git_repository_init_options
#field version , CUInt
#field flags , CUInt
#field mode , CUInt
#field workdir_path , CString
#field description , CString
#field template_path , CString
#field initial_head , CString
#field origin_url , CString
#stoptype
#ccall git_repository_init_options_init , Ptr <git_repository_init_options> -> CUInt -> IO CInt
#ccall git_repository_init_ext , Ptr (Ptr <struct git_repository>) -> CString -> Ptr <git_repository_init_options> -> IO CInt
#ccall git_repository_head , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_head_for_worktree , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_repository_head_detached , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_head_detached_for_worktree , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_repository_head_unborn , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_is_empty , Ptr <struct git_repository> -> IO CInt
{- typedef enum {
            GIT_REPOSITORY_ITEM_GITDIR,
            GIT_REPOSITORY_ITEM_WORKDIR,
            GIT_REPOSITORY_ITEM_COMMONDIR,
            GIT_REPOSITORY_ITEM_INDEX,
            GIT_REPOSITORY_ITEM_OBJECTS,
            GIT_REPOSITORY_ITEM_REFS,
            GIT_REPOSITORY_ITEM_PACKED_REFS,
            GIT_REPOSITORY_ITEM_REMOTES,
            GIT_REPOSITORY_ITEM_CONFIG,
            GIT_REPOSITORY_ITEM_INFO,
            GIT_REPOSITORY_ITEM_HOOKS,
            GIT_REPOSITORY_ITEM_LOGS,
            GIT_REPOSITORY_ITEM_MODULES,
            GIT_REPOSITORY_ITEM_WORKTREES,
            GIT_REPOSITORY_ITEM__LAST
        } git_repository_item_t; -}
#integral_t git_repository_item_t
#num GIT_REPOSITORY_ITEM_GITDIR
#num GIT_REPOSITORY_ITEM_WORKDIR
#num GIT_REPOSITORY_ITEM_COMMONDIR
#num GIT_REPOSITORY_ITEM_INDEX
#num GIT_REPOSITORY_ITEM_OBJECTS
#num GIT_REPOSITORY_ITEM_REFS
#num GIT_REPOSITORY_ITEM_PACKED_REFS
#num GIT_REPOSITORY_ITEM_REMOTES
#num GIT_REPOSITORY_ITEM_CONFIG
#num GIT_REPOSITORY_ITEM_INFO
#num GIT_REPOSITORY_ITEM_HOOKS
#num GIT_REPOSITORY_ITEM_LOGS
#num GIT_REPOSITORY_ITEM_MODULES
#num GIT_REPOSITORY_ITEM_WORKTREES
#num GIT_REPOSITORY_ITEM__LAST
#ccall git_repository_item_path , Ptr <git_buf> -> Ptr <struct git_repository> -> <git_repository_item_t> -> IO CInt
#ccall git_repository_path , Ptr <struct git_repository> -> IO CString
#ccall git_repository_workdir , Ptr <struct git_repository> -> IO CString
#ccall git_repository_commondir , Ptr <struct git_repository> -> IO CString
#ccall git_repository_set_workdir , Ptr <struct git_repository> -> CString -> CInt -> IO CInt
#ccall git_repository_is_bare , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_is_worktree , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_config , Ptr (Ptr <struct git_config>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_config_snapshot , Ptr (Ptr <struct git_config>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_odb , Ptr (Ptr <struct git_odb>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_refdb , Ptr (Ptr <struct git_refdb>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_index , Ptr (Ptr <struct git_index>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_message , Ptr <git_buf> -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_message_remove , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_state_cleanup , Ptr <struct git_repository> -> IO CInt
#callback git_repository_fetchhead_foreach_cb , CString -> CString -> Ptr <struct git_oid> -> CUInt -> Ptr () -> IO CInt
#ccall git_repository_fetchhead_foreach , Ptr <struct git_repository> -> <git_repository_fetchhead_foreach_cb> -> Ptr () -> IO CInt
#callback git_repository_mergehead_foreach_cb , Ptr <struct git_oid> -> Ptr () -> IO CInt
#ccall git_repository_mergehead_foreach , Ptr <struct git_repository> -> <git_repository_mergehead_foreach_cb> -> Ptr () -> IO CInt
#ccall git_repository_hashfile , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> <git_object_t> -> CString -> IO CInt
#ccall git_repository_set_head , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_repository_set_head_detached , Ptr <struct git_repository> -> Ptr <struct git_oid> -> IO CInt
#ccall git_repository_set_head_detached_from_annotated , Ptr <struct git_repository> -> Ptr <struct git_annotated_commit> -> IO CInt
#ccall git_repository_detach_head , Ptr <struct git_repository> -> IO CInt
{- typedef enum {
            GIT_REPOSITORY_STATE_NONE,
            GIT_REPOSITORY_STATE_MERGE,
            GIT_REPOSITORY_STATE_REVERT,
            GIT_REPOSITORY_STATE_REVERT_SEQUENCE,
            GIT_REPOSITORY_STATE_CHERRYPICK,
            GIT_REPOSITORY_STATE_CHERRYPICK_SEQUENCE,
            GIT_REPOSITORY_STATE_BISECT,
            GIT_REPOSITORY_STATE_REBASE,
            GIT_REPOSITORY_STATE_REBASE_INTERACTIVE,
            GIT_REPOSITORY_STATE_REBASE_MERGE,
            GIT_REPOSITORY_STATE_APPLY_MAILBOX,
            GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE
        } git_repository_state_t; -}
#integral_t git_repository_state_t
#num GIT_REPOSITORY_STATE_NONE
#num GIT_REPOSITORY_STATE_MERGE
#num GIT_REPOSITORY_STATE_REVERT
#num GIT_REPOSITORY_STATE_REVERT_SEQUENCE
#num GIT_REPOSITORY_STATE_CHERRYPICK
#num GIT_REPOSITORY_STATE_CHERRYPICK_SEQUENCE
#num GIT_REPOSITORY_STATE_BISECT
#num GIT_REPOSITORY_STATE_REBASE
#num GIT_REPOSITORY_STATE_REBASE_INTERACTIVE
#num GIT_REPOSITORY_STATE_REBASE_MERGE
#num GIT_REPOSITORY_STATE_APPLY_MAILBOX
#num GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE
#ccall git_repository_state , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_set_namespace , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_repository_get_namespace , Ptr <struct git_repository> -> IO CString
#ccall git_repository_is_shallow , Ptr <struct git_repository> -> IO CInt
#ccall git_repository_ident , Ptr CString -> Ptr CString -> Ptr <struct git_repository> -> IO CInt
#ccall git_repository_set_ident , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_repository_oid_type , Ptr <struct git_repository> -> IO <git_oid_t>
