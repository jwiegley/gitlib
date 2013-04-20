{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Remote where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Repository
import Bindings.Libgit2.Refspec
import Bindings.Libgit2.Net
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Transport
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef int (* git_remote_rename_problem_cb)(const char * problematic_refspec,
                                             void * payload); -}
#callback git_remote_rename_problem_cb , CString -> Ptr () -> Ptr CInt
#ccall git_remote_create , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> CString -> IO (CInt)
#ccall git_remote_create_inmemory , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> CString -> IO (CInt)
#ccall git_remote_load , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_remote_save , Ptr <git_remote> -> IO (CInt)
#ccall git_remote_name , Ptr <git_remote> -> IO (CString)
#ccall git_remote_url , Ptr <git_remote> -> IO (CString)
#ccall git_remote_pushurl , Ptr <git_remote> -> IO (CString)
#ccall git_remote_set_url , Ptr <git_remote> -> CString -> IO (CInt)
#ccall git_remote_set_pushurl , Ptr <git_remote> -> CString -> IO (CInt)
#ccall git_remote_set_fetchspec , Ptr <git_remote> -> CString -> IO (CInt)
#ccall git_remote_fetchspec , Ptr <git_remote> -> IO (Ptr <git_refspec>)
#ccall git_remote_set_pushspec , Ptr <git_remote> -> CString -> IO (CInt)
#ccall git_remote_pushspec , Ptr <git_remote> -> IO (Ptr <git_refspec>)
#ccall git_remote_connect , Ptr <git_remote> -> <git_direction> -> IO (CInt)
#ccall git_remote_ls , Ptr <git_remote> -> <git_headlist_cb> -> Ptr () -> IO (CInt)
#ccall git_remote_download , Ptr <git_remote> -> <git_transfer_progress_callback> -> Ptr () -> IO (CInt)
#ccall git_remote_connected , Ptr <git_remote> -> IO (CInt)
#ccall git_remote_stop , Ptr <git_remote> -> IO ()
#ccall git_remote_disconnect , Ptr <git_remote> -> IO ()
#ccall git_remote_free , Ptr <git_remote> -> IO ()
#ccall git_remote_update_tips , Ptr <git_remote> -> IO (CInt)
#ccall git_remote_valid_url , CString -> IO (CInt)
#ccall git_remote_supported_url , CString -> IO (CInt)
#ccall git_remote_list , Ptr <git_strarray> -> Ptr <git_repository> -> IO (CInt)
#ccall git_remote_check_cert , Ptr <git_remote> -> CInt -> IO ()
#ccall git_remote_set_cred_acquire_cb , Ptr <git_remote> -> <git_cred_acquire_cb> -> Ptr () -> IO ()
#ccall git_remote_set_transport , Ptr <git_remote> -> Ptr <git_transport> -> IO (CInt)
{- typedef enum git_remote_completion_type {
            GIT_REMOTE_COMPLETION_DOWNLOAD,
            GIT_REMOTE_COMPLETION_INDEXING,
            GIT_REMOTE_COMPLETION_ERROR
        } git_remote_completion_type; -}
#integral_t git_remote_completion_type
#num GIT_REMOTE_COMPLETION_DOWNLOAD
#num GIT_REMOTE_COMPLETION_INDEXING
#num GIT_REMOTE_COMPLETION_ERROR
{- struct git_remote_callbacks {
    unsigned int version;
    void (* progress)(const char * str, int len, void * data);
    int (* completion)(git_remote_completion_type type, void * data);
    int (* update_tips)(const char * refname,
                        const git_oid * a,
                        const git_oid * b,
                        void * data);
    void * payload;
}; -}
#callback git_remote_callbacks_progress_callback , CString -> CInt -> Ptr () -> IO ()
#callback git_remote_callbacks_completion_callback , <git_remote_completion_type> -> Ptr () -> IO CInt
#callback git_remote_callbacks_update_tips_callback , CString -> Ptr <git_oid> -> Ptr <git_oid> -> Ptr () -> IO CInt
#starttype git_remote_callbacks
#field version , CUInt
#field progress , <git_remote_callbacks_progress_callback>
#field completion , <git_remote_callbacks_completion_callback>
#field update_tips , <git_remote_callbacks_update_tips_callback>
#field payload , Ptr ()
#stoptype
#ccall git_remote_set_callbacks , Ptr <git_remote> -> Ptr <git_remote_callbacks> -> IO (CInt)
#ccall git_remote_stats , Ptr <git_remote> -> IO (Ptr <git_transfer_progress>)
{- typedef enum {
            GIT_REMOTE_DOWNLOAD_TAGS_UNSET,
            GIT_REMOTE_DOWNLOAD_TAGS_NONE,
            GIT_REMOTE_DOWNLOAD_TAGS_AUTO,
            GIT_REMOTE_DOWNLOAD_TAGS_ALL
        } git_remote_autotag_option_t; -}
#integral_t git_remote_autotag_option_t
#num GIT_REMOTE_DOWNLOAD_TAGS_UNSET
#num GIT_REMOTE_DOWNLOAD_TAGS_NONE
#num GIT_REMOTE_DOWNLOAD_TAGS_AUTO
#num GIT_REMOTE_DOWNLOAD_TAGS_ALL
#ccall git_remote_autotag , Ptr <git_remote> -> IO (<git_remote_autotag_option_t>)
#ccall git_remote_set_autotag , Ptr <git_remote> -> <git_remote_autotag_option_t> -> IO ()
#ccall git_remote_rename , Ptr <git_remote> -> CString -> <git_remote_rename_problem_cb> -> Ptr () -> IO (CInt)
#ccall git_remote_update_fetchhead , Ptr <git_remote> -> IO (CInt)
#ccall git_remote_set_update_fetchhead , Ptr <git_remote> -> CInt -> IO ()
#ccall git_remote_is_valid_name , CString -> IO (CInt)
