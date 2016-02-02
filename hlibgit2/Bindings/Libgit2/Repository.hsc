{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Repository where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_repository_open , Ptr (Ptr <git_repository>) -> CString -> IO (CInt)
#ccall git_repository_wrap_odb , Ptr (Ptr <git_repository>) -> Ptr <git_odb> -> IO (CInt)
#ccall git_repository_discover , CString -> CSize -> CString -> CInt -> CString -> IO (CInt)
{- typedef enum {
            GIT_REPOSITORY_OPEN_NO_SEARCH = 1 << 0,
            GIT_REPOSITORY_OPEN_CROSS_FS = 1 << 1
        } git_repository_open_flag_t; -}
#integral_t git_repository_open_flag_t
#num GIT_REPOSITORY_OPEN_NO_SEARCH
#num GIT_REPOSITORY_OPEN_CROSS_FS
#ccall git_repository_open_ext , Ptr (Ptr <git_repository>) -> CString -> CUInt -> CString -> IO (CInt)
#ccall git_repository_new , Ptr (Ptr <git_repository>) -> IO (CInt)
#ccall git_repository_free , Ptr <git_repository> -> IO ()
#ccall git_repository_init , Ptr (Ptr <git_repository>) -> CString -> CUInt -> IO (CInt)
{- typedef enum {
            GIT_REPOSITORY_INIT_BARE = 1u << 0,
            GIT_REPOSITORY_INIT_NO_REINIT = 1u << 1,
            GIT_REPOSITORY_INIT_NO_DOTGIT_DIR = 1u << 2,
            GIT_REPOSITORY_INIT_MKDIR = 1u << 3,
            GIT_REPOSITORY_INIT_MKPATH = 1u << 4,
            GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE = 1u << 5
        } git_repository_init_flag_t; -}
#integral_t git_repository_init_flag_t
#num GIT_REPOSITORY_INIT_BARE
#num GIT_REPOSITORY_INIT_NO_REINIT
#num GIT_REPOSITORY_INIT_NO_DOTGIT_DIR
#num GIT_REPOSITORY_INIT_MKDIR
#num GIT_REPOSITORY_INIT_MKPATH
#num GIT_REPOSITORY_INIT_EXTERNAL_TEMPLATE
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
#ccall git_repository_init_ext , Ptr (Ptr <git_repository>) -> CString -> Ptr <git_repository_init_options> -> IO (CInt)
#ccall git_repository_head , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_head_detached , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_head_orphan , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_is_empty , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_path , Ptr <git_repository> -> IO (CString)
#ccall git_repository_workdir , Ptr <git_repository> -> IO (CString)
#ccall git_repository_set_workdir , Ptr <git_repository> -> CString -> CInt -> IO (CInt)
#ccall git_repository_is_bare , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_config , Ptr (Ptr <git_config>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_config , Ptr <git_repository> -> Ptr <git_config> -> IO ()
#ccall git_repository_odb , Ptr (Ptr <git_odb>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_odb , Ptr <git_repository> -> Ptr <git_odb> -> IO ()
#ccall git_repository_refdb , Ptr (Ptr <git_refdb>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_refdb , Ptr <git_repository> -> Ptr <git_refdb> -> IO ()
#ccall git_repository_index , Ptr (Ptr <git_index>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_set_index , Ptr <git_repository> -> Ptr <git_index> -> IO ()
#ccall git_repository_message , CString -> CSize -> Ptr <git_repository> -> IO (CInt)
#ccall git_repository_message_remove , Ptr <git_repository> -> IO (CInt)
#ccall git_repository_merge_cleanup , Ptr <git_repository> -> IO (CInt)
{- typedef int (* git_repository_fetchhead_foreach_cb)(const char * ref_name,
                                                    const char * remote_url,
                                                    const git_oid * oid,
                                                    unsigned int is_merge,
                                                    void * payload); -}
#callback git_repository_fetchhead_foreach_cb , CString -> CString -> Ptr (<git_oid>) -> CUInt -> Ptr () -> IO CInt
#ccall git_repository_fetchhead_foreach , Ptr <git_repository> -> <git_repository_fetchhead_foreach_cb> -> Ptr () -> IO (CInt)
{- typedef int (* git_repository_mergehead_foreach_cb)(const git_oid * oid,
                                                    void * payload); -}
#callback git_repository_mergehead_foreach_cb , Ptr (<git_oid>) -> Ptr () -> IO CInt
#ccall git_repository_mergehead_foreach , Ptr <git_repository> -> <git_repository_mergehead_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_repository_hashfile , Ptr <git_oid> -> Ptr <git_repository> -> CString -> <git_otype> -> CString -> IO (CInt)
#ccall git_repository_set_head , Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_repository_set_head_detached , Ptr <git_repository> -> Ptr <git_oid> -> IO (CInt)
#ccall git_repository_detach_head , Ptr <git_repository> -> IO (CInt)
{- typedef enum {
            GIT_REPOSITORY_STATE_NONE,
            GIT_REPOSITORY_STATE_MERGE,
            GIT_REPOSITORY_STATE_REVERT,
            GIT_REPOSITORY_STATE_CHERRY_PICK,
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
#num GIT_REPOSITORY_STATE_CHERRYPICK
#num GIT_REPOSITORY_STATE_BISECT
#num GIT_REPOSITORY_STATE_REBASE
#num GIT_REPOSITORY_STATE_REBASE_INTERACTIVE
#num GIT_REPOSITORY_STATE_REBASE_MERGE
#num GIT_REPOSITORY_STATE_APPLY_MAILBOX
#num GIT_REPOSITORY_STATE_APPLY_MAILBOX_OR_REBASE
#ccall git_repository_state , Ptr <git_repository> -> IO (CInt)
