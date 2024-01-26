{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/remote.h>
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
import Bindings.Libgit2.Pack
import Bindings.Libgit2.Proxy
import Bindings.Libgit2.Types
import Bindings.Libgit2.Buffer
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Credential
import Bindings.Libgit2.Cert
#ccall git_remote_create , Ptr (Ptr <struct git_remote>) -> Ptr <struct git_repository> -> CString -> CString -> IO CInt
{- typedef enum {
            GIT_REMOTE_REDIRECT_NONE = 1 << 0,
            GIT_REMOTE_REDIRECT_INITIAL = 1 << 1,
            GIT_REMOTE_REDIRECT_ALL = 1 << 2
        } git_remote_redirect_t; -}
#integral_t git_remote_redirect_t
#num GIT_REMOTE_REDIRECT_NONE
#num GIT_REMOTE_REDIRECT_INITIAL
#num GIT_REMOTE_REDIRECT_ALL
{- typedef enum {
            GIT_REMOTE_CREATE_SKIP_INSTEADOF = 1 << 0,
            GIT_REMOTE_CREATE_SKIP_DEFAULT_FETCHSPEC = 1 << 1
        } git_remote_create_flags; -}
#integral_t git_remote_create_flags
#num GIT_REMOTE_CREATE_SKIP_INSTEADOF
#num GIT_REMOTE_CREATE_SKIP_DEFAULT_FETCHSPEC
{- typedef enum {
            GIT_REMOTE_UPDATE_FETCHHEAD = 1 << 0,
            GIT_REMOTE_UPDATE_REPORT_UNCHANGED = 1 << 1
        } git_remote_update_flags; -}
#integral_t git_remote_update_flags
#num GIT_REMOTE_UPDATE_FETCHHEAD
#num GIT_REMOTE_UPDATE_REPORT_UNCHANGED
{- typedef struct git_remote_create_options {
            unsigned int version;
            git_repository * repository;
            const char * name;
            const char * fetchspec;
            unsigned int flags;
        } git_remote_create_options; -}
#starttype struct git_remote_create_options
#field version , CUInt
#field repository , Ptr <struct git_repository>
#field name , CString
#field fetchspec , CString
#field flags , CUInt
#stoptype
#ccall git_remote_create_options_init , Ptr <struct git_remote_create_options> -> CUInt -> IO CInt
#ccall git_remote_create_with_opts , Ptr (Ptr <struct git_remote>) -> CString -> Ptr <struct git_remote_create_options> -> IO CInt
#ccall git_remote_create_with_fetchspec , Ptr (Ptr <struct git_remote>) -> Ptr <struct git_repository> -> CString -> CString -> CString -> IO CInt
#ccall git_remote_create_anonymous , Ptr (Ptr <struct git_remote>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_remote_create_detached , Ptr (Ptr <struct git_remote>) -> CString -> IO CInt
#ccall git_remote_lookup , Ptr (Ptr <struct git_remote>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_remote_dup , Ptr (Ptr <struct git_remote>) -> Ptr <struct git_remote> -> IO CInt
#ccall git_remote_owner , Ptr <struct git_remote> -> IO (Ptr <struct git_repository>)
#ccall git_remote_name , Ptr <struct git_remote> -> IO CString
#ccall git_remote_url , Ptr <struct git_remote> -> IO CString
#ccall git_remote_pushurl , Ptr <struct git_remote> -> IO CString
#ccall git_remote_set_url , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_remote_set_pushurl , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_remote_set_instance_url , Ptr <struct git_remote> -> CString -> IO CInt
#ccall git_remote_set_instance_pushurl , Ptr <struct git_remote> -> CString -> IO CInt
#ccall git_remote_add_fetch , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_remote_get_fetch_refspecs , Ptr <struct git_strarray> -> Ptr <struct git_remote> -> IO CInt
#ccall git_remote_add_push , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_remote_get_push_refspecs , Ptr <struct git_strarray> -> Ptr <struct git_remote> -> IO CInt
#ccall git_remote_refspec_count , Ptr <struct git_remote> -> IO CSize
#ccall git_remote_get_refspec , Ptr <struct git_remote> -> CSize -> IO (Ptr <struct git_refspec>)
#ccall git_remote_ls , Ptr (Ptr (Ptr <struct git_remote_head>)) -> Ptr CSize -> Ptr <struct git_remote> -> IO CInt
#ccall git_remote_connected , Ptr <struct git_remote> -> IO CInt
#ccall git_remote_stop , Ptr <struct git_remote> -> IO CInt
#ccall git_remote_disconnect , Ptr <struct git_remote> -> IO CInt
#ccall git_remote_free , Ptr <struct git_remote> -> IO ()
#ccall git_remote_list , Ptr <struct git_strarray> -> Ptr <struct git_repository> -> IO CInt
{- typedef enum git_remote_completion_t {
            GIT_REMOTE_COMPLETION_DOWNLOAD,
            GIT_REMOTE_COMPLETION_INDEXING,
            GIT_REMOTE_COMPLETION_ERROR
        } git_remote_completion_t; -}
#integral_t enum git_remote_completion_t
#num GIT_REMOTE_COMPLETION_DOWNLOAD
#num GIT_REMOTE_COMPLETION_INDEXING
#num GIT_REMOTE_COMPLETION_ERROR
#callback git_push_transfer_progress_cb , CUInt -> CUInt -> CSize -> Ptr () -> IO CInt
{- typedef struct {
            char * src_refname; char * dst_refname; git_oid src; git_oid dst;
        } git_push_update; -}
#starttype git_push_update
#field src_refname , CString
#field dst_refname , CString
#field src , <struct git_oid>
#field dst , <struct git_oid>
#stoptype
#callback git_push_negotiation , Ptr (Ptr <git_push_update>) -> CSize -> Ptr () -> IO CInt
#callback git_push_update_reference_cb , CString -> CString -> Ptr () -> IO CInt
#callback git_url_resolve_cb , Ptr <git_buf> -> CString -> CInt -> Ptr () -> IO CInt
#callback git_remote_ready_cb , Ptr <struct git_remote> -> CInt -> Ptr () -> IO CInt
{- struct git_remote_callbacks {
    unsigned int version;
    git_transport_message_cb sideband_progress;
    int (* completion)(git_remote_completion_t type, void * data);
    git_credential_acquire_cb credentials;
    git_transport_certificate_check_cb certificate_check;
    git_indexer_progress_cb transfer_progress;
    int (* update_tips)(const char * refname,
                        const git_oid * a,
                        const git_oid * b,
                        void * data);
    git_packbuilder_progress pack_progress;
    git_push_transfer_progress_cb push_transfer_progress;
    git_push_update_reference_cb push_update_reference;
    git_push_negotiation push_negotiation;
    git_transport_cb transport;
    git_remote_ready_cb remote_ready;
    void * payload;
    git_url_resolve_cb resolve_url;
}; -}
#starttype struct git_remote_callbacks
#field version , CUInt
#field sideband_progress , <git_transport_message_cb>
#field completion , FunPtr (<enum git_remote_completion_t> -> Ptr () -> CInt)
#field credentials , <git_credential_acquire_cb>
#field certificate_check , <git_transport_certificate_check_cb>
#field transfer_progress , <git_indexer_progress_cb>
#field update_tips , FunPtr (CString -> Ptr <struct git_oid> -> Ptr <struct git_oid> -> Ptr () -> CInt)
#field pack_progress , <git_packbuilder_progress>
#field push_transfer_progress , <git_push_transfer_progress_cb>
#field push_update_reference , <git_push_update_reference_cb>
#field push_negotiation , <git_push_negotiation>
#field transport , <git_transport_cb>
#field remote_ready , <git_remote_ready_cb>
#field payload , Ptr ()
#field resolve_url , <git_url_resolve_cb>
#stoptype
#ccall git_remote_init_callbacks , Ptr <struct git_remote_callbacks> -> CUInt -> IO CInt
{- typedef enum {
            GIT_FETCH_PRUNE_UNSPECIFIED, GIT_FETCH_PRUNE, GIT_FETCH_NO_PRUNE
        } git_fetch_prune_t; -}
#integral_t git_fetch_prune_t
#num GIT_FETCH_PRUNE_UNSPECIFIED
#num GIT_FETCH_PRUNE
#num GIT_FETCH_NO_PRUNE
{- typedef enum {
            GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED = 0,
            GIT_REMOTE_DOWNLOAD_TAGS_AUTO,
            GIT_REMOTE_DOWNLOAD_TAGS_NONE,
            GIT_REMOTE_DOWNLOAD_TAGS_ALL
        } git_remote_autotag_option_t; -}
#integral_t git_remote_autotag_option_t
#num GIT_REMOTE_DOWNLOAD_TAGS_UNSPECIFIED
#num GIT_REMOTE_DOWNLOAD_TAGS_AUTO
#num GIT_REMOTE_DOWNLOAD_TAGS_NONE
#num GIT_REMOTE_DOWNLOAD_TAGS_ALL
{- typedef enum {
            GIT_FETCH_DEPTH_FULL = 0, GIT_FETCH_DEPTH_UNSHALLOW = 2147483647
        } git_fetch_depth_t; -}
#integral_t git_fetch_depth_t
#num GIT_FETCH_DEPTH_FULL
#num GIT_FETCH_DEPTH_UNSHALLOW
{- typedef struct {
            int version;
            git_remote_callbacks callbacks;
            git_fetch_prune_t prune;
            unsigned int update_flags;
            git_remote_autotag_option_t download_tags;
            git_proxy_options proxy_opts;
            int depth;
            git_remote_redirect_t follow_redirects;
            git_strarray custom_headers;
        } git_fetch_options; -}
#starttype git_fetch_options
#field version , CInt
#field callbacks , <struct git_remote_callbacks>
#field prune , <git_fetch_prune_t>
#field update_flags , CUInt
#field download_tags , <git_remote_autotag_option_t>
#field proxy_opts , <git_proxy_options>
#field depth , CInt
#field follow_redirects , <git_remote_redirect_t>
#field custom_headers , <struct git_strarray>
#stoptype
#ccall git_fetch_options_init , Ptr <git_fetch_options> -> CUInt -> IO CInt
{- typedef struct {
            unsigned int version;
            unsigned int pb_parallelism;
            git_remote_callbacks callbacks;
            git_proxy_options proxy_opts;
            git_remote_redirect_t follow_redirects;
            git_strarray custom_headers;
        } git_push_options; -}
#starttype git_push_options
#field version , CUInt
#field pb_parallelism , CUInt
#field callbacks , <struct git_remote_callbacks>
#field proxy_opts , <git_proxy_options>
#field follow_redirects , <git_remote_redirect_t>
#field custom_headers , <struct git_strarray>
#stoptype
#ccall git_push_options_init , Ptr <git_push_options> -> CUInt -> IO CInt
{- typedef struct {
            unsigned int version;
            git_remote_callbacks callbacks;
            git_proxy_options proxy_opts;
            git_remote_redirect_t follow_redirects;
            git_strarray custom_headers;
        } git_remote_connect_options; -}
#starttype git_remote_connect_options
#field version , CUInt
#field callbacks , <struct git_remote_callbacks>
#field proxy_opts , <git_proxy_options>
#field follow_redirects , <git_remote_redirect_t>
#field custom_headers , <struct git_strarray>
#stoptype
#ccall git_remote_connect_options_init , Ptr <git_remote_connect_options> -> CUInt -> IO CInt
#ccall git_remote_connect , Ptr <struct git_remote> -> <git_direction> -> Ptr <struct git_remote_callbacks> -> Ptr <git_proxy_options> -> Ptr <struct git_strarray> -> IO CInt
#ccall git_remote_connect_ext , Ptr <struct git_remote> -> <git_direction> -> Ptr <git_remote_connect_options> -> IO CInt
#ccall git_remote_download , Ptr <struct git_remote> -> Ptr <struct git_strarray> -> Ptr <git_fetch_options> -> IO CInt
#ccall git_remote_upload , Ptr <struct git_remote> -> Ptr <struct git_strarray> -> Ptr <git_push_options> -> IO CInt
#ccall git_remote_update_tips , Ptr <struct git_remote> -> Ptr <struct git_remote_callbacks> -> CUInt -> <git_remote_autotag_option_t> -> CString -> IO CInt
#ccall git_remote_fetch , Ptr <struct git_remote> -> Ptr <struct git_strarray> -> Ptr <git_fetch_options> -> CString -> IO CInt
#ccall git_remote_prune , Ptr <struct git_remote> -> Ptr <struct git_remote_callbacks> -> IO CInt
#ccall git_remote_push , Ptr <struct git_remote> -> Ptr <struct git_strarray> -> Ptr <git_push_options> -> IO CInt
#ccall git_remote_stats , Ptr <struct git_remote> -> IO (Ptr <struct git_indexer_progress>)
#ccall git_remote_autotag , Ptr <struct git_remote> -> IO <git_remote_autotag_option_t>
#ccall git_remote_set_autotag , Ptr <struct git_repository> -> CString -> <git_remote_autotag_option_t> -> IO CInt
#ccall git_remote_prune_refs , Ptr <struct git_remote> -> IO CInt
#ccall git_remote_rename , Ptr <struct git_strarray> -> Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_remote_name_is_valid , Ptr CInt -> CString -> IO CInt
#ccall git_remote_delete , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_remote_default_branch , Ptr <git_buf> -> Ptr <struct git_remote> -> IO CInt
