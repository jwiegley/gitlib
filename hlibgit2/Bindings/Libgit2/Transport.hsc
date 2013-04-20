{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/transport.h>
module Bindings.Libgit2.Transport where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Net
import Bindings.Libgit2.Types
{- typedef enum {
            GIT_CREDTYPE_USERPASS_PLAINTEXT = 1
        } git_credtype_t; -}
#integral_t git_credtype_t
#num GIT_CREDTYPE_USERPASS_PLAINTEXT
{- typedef struct git_cred {
            git_credtype_t credtype; void (* free)(struct git_cred * cred);
        } git_cred; -}
#callback git_cred_free_callback , Ptr <git_cred> -> IO ()
#starttype git_cred
#field credtype , <git_credtype_t>
#field free , <git_cred_free_callback>
#stoptype
{- typedef struct git_cred_userpass_plaintext {
            git_cred parent; char * username; char * password;
        } git_cred_userpass_plaintext; -}
#starttype git_cred_userpass_plaintext
#field parent , <git_cred>
#field username , CString
#field password , CString
#stoptype
#ccall git_cred_userpass_plaintext_new , Ptr (Ptr <git_cred>) -> CString -> CString -> IO (CInt)
{- typedef int (* git_cred_acquire_cb)(git_cred * * cred,
                                    const char * url,
                                    const char * username_from_url,
                                    unsigned int allowed_types,
                                    void * payload); -}
#callback git_cred_acquire_cb , Ptr (Ptr (<git_cred>)) -> CString -> CString -> CUInt -> Ptr () -> IO CInt
{- typedef enum {
            GIT_TRANSPORTFLAGS_NONE = 0, GIT_TRANSPORTFLAGS_NO_CHECK_CERT = 1
        } git_transport_flags_t; -}
#integral_t git_transport_flags_t
#num GIT_TRANSPORTFLAGS_NONE
#num GIT_TRANSPORTFLAGS_NO_CHECK_CERT
{- typedef void (* git_transport_message_cb)(const char * str,
                                          int len,
                                          void * data); -}
#callback git_transport_message_cb , CString -> CInt -> Ptr () -> IO ()
{- typedef struct git_transport {
            unsigned int version;
            int (* set_callbacks)(struct git_transport * transport,
                                  git_transport_message_cb progress_cb,
                                  git_transport_message_cb error_cb,
                                  void * payload);
            int (* connect)(struct git_transport * transport,
                            const char * url,
                            git_cred_acquire_cb cred_acquire_cb,
                            void * cred_acquire_payload,
                            int direction,
                            int flags);
            int (* ls)(struct git_transport * transport,
                       git_headlist_cb list_cb,
                       void * payload);
            int (* push)(struct git_transport * transport, git_push * push);
            int (* negotiate_fetch)(struct git_transport * transport,
                                    git_repository * repo,
                                    const git_remote_head * const * refs,
                                    size_t count);
            int (* download_pack)(struct git_transport * transport,
                                  git_repository * repo,
                                  git_transfer_progress * stats,
                                  git_transfer_progress_callback progress_cb,
                                  void * progress_payload);
            int (* is_connected)(struct git_transport * transport);
            int (* read_flags)(struct git_transport * transport, int * flags);
            void (* cancel)(struct git_transport * transport);
            int (* close)(struct git_transport * transport);
            void (* free)(struct git_transport * transport);
        } git_transport; -}
#callback git_transport_set_callbacks_callback , Ptr <git_transport> -> <git_transport_message_cb> -> <git_transport_message_cb> -> Ptr () -> IO CInt
#callback git_transport_connect_callback , Ptr <git_transport> -> CString -> <git_cred_acquire_cb> -> Ptr () -> CInt -> CInt -> IO CInt
#callback git_transport_ls_callback , Ptr <git_transport> -> <git_headlist_cb> -> Ptr () -> IO CInt
#callback git_transport_push_callback , Ptr <git_transport> -> Ptr <git_push> -> IO CInt
#callback git_transport_negotiate_fetch_callback , Ptr <git_transport> -> Ptr <git_repository> -> Ptr (Ptr <git_remote_head>) -> CSize -> IO CInt
#callback git_transport_download_pack_callback , Ptr <git_transport> -> Ptr <git_repository> -> Ptr <git_transfer_progress> -> <git_transfer_progress_callback> -> Ptr () -> IO CInt
#callback git_transport_is_connected_callback , Ptr <git_transport> -> IO CInt
#callback git_transport_read_flags_callback , Ptr <git_transport> -> Ptr CInt -> IO CInt
#callback git_transport_cancel_callback , Ptr <git_transport> -> IO ()
#callback git_transport_close_callback , Ptr <git_transport> -> IO CInt
#callback git_transport_free_callback , Ptr <git_transport> -> IO ()
#starttype git_transport
#field version , CUInt
#field set_callbacks , <git_transport_set_callbacks_callback>
#field connect , <git_transport_connect_callback>
#field ls , <git_transport_ls_callback>
#field push , <git_transport_push_callback>
#field negotiate_fetch , <git_transport_negotiate_fetch_callback>
#field download_pack , <git_transport_download_pack_callback>
#field is_connected , <git_transport_is_connected_callback>
#field read_flags , <git_transport_read_flags_callback>
#field cancel , <git_transport_cancel_callback>
#field close , <git_transport_close_callback>
#field free , <git_transport_free_callback>
#stoptype
#ccall git_transport_new , Ptr (Ptr <git_transport>) -> Ptr <git_remote> -> CString -> IO (CInt)
{- typedef int (* git_transport_cb)(git_transport * * out,
                                 git_remote * owner,
                                 void * param); -}
#callback git_transport_cb , Ptr (Ptr (<git_transport>)) -> Ptr (<git_remote>) -> Ptr () -> IO CInt
#ccall git_transport_dummy , Ptr (Ptr <git_transport>) -> Ptr <git_remote> -> Ptr () -> IO (CInt)
#ccall git_transport_local , Ptr (Ptr <git_transport>) -> Ptr <git_remote> -> Ptr () -> IO (CInt)
#ccall git_transport_smart , Ptr (Ptr <git_transport>) -> Ptr <git_remote> -> Ptr () -> IO (CInt)
{- typedef enum {
            GIT_SERVICE_UPLOADPACK_LS = 1,
            GIT_SERVICE_UPLOADPACK = 2,
            GIT_SERVICE_RECEIVEPACK_LS = 3,
            GIT_SERVICE_RECEIVEPACK = 4
        } git_smart_service_t; -}
#integral_t git_smart_service_t
#num GIT_SERVICE_UPLOADPACK_LS
#num GIT_SERVICE_UPLOADPACK
#num GIT_SERVICE_RECEIVEPACK_LS
#num GIT_SERVICE_RECEIVEPACK
{- struct git_smart_subtransport; -}
{- #opaque_t git_smart_subtransport -}
{- typedef struct git_smart_subtransport_stream {
            struct git_smart_subtransport * subtransport;
            int (* read)(struct git_smart_subtransport_stream * stream,
                         char * buffer,
                         size_t buf_size,
                         size_t * bytes_read);
            int (* write)(struct git_smart_subtransport_stream * stream,
                          const char * buffer,
                          size_t len);
            void (* free)(struct git_smart_subtransport_stream * stream);
        } git_smart_subtransport_stream; -}
#callback git_smart_subtransport_stream_read_callback , Ptr <git_smart_subtransport_stream> -> CString -> CSize -> Ptr CSize -> IO CInt
#callback git_smart_subtransport_stream_write_callback , Ptr <git_smart_subtransport_stream> -> CString -> CSize -> IO CInt
#callback git_smart_subtransport_stream_free_callback , Ptr <git_smart_subtransport_stream> -> IO ()
#starttype git_smart_subtransport_stream
#field subtransport , Ptr <git_smart_subtransport>
#field read , <git_smart_subtransport_stream_read_callback>
#field write , <git_smart_subtransport_stream_write_callback>
#field free , <git_smart_subtransport_stream_free_callback>
#stoptype
{- typedef struct git_smart_subtransport {
            int (* action)(git_smart_subtransport_stream * * out,
                           struct git_smart_subtransport * transport,
                           const char * url,
                           git_smart_service_t action);
            int (* close)(struct git_smart_subtransport * transport);
            void (* free)(struct git_smart_subtransport * transport);
        } git_smart_subtransport; -}
#callback git_smart_subtransport_action_callback , Ptr (Ptr <git_smart_subtransport_stream>) -> Ptr <git_smart_subtransport> -> CString -> <git_smart_service_t> -> IO CInt
#callback git_smart_subtransport_close_callback , Ptr <git_smart_subtransport> -> IO CInt
#callback git_smart_subtransport_free_callback , Ptr <git_smart_subtransport> -> IO ()
#starttype git_smart_subtransport
#field action , <git_smart_subtransport_action_callback>
#field close , <git_smart_subtransport_close_callback>
#field free , <git_smart_subtransport_free_callback>
#stoptype
{- typedef int (* git_smart_subtransport_cb)(git_smart_subtransport * * out,
                                          git_transport * owner); -}
#callback git_smart_subtransport_cb , Ptr (Ptr (<git_smart_subtransport>)) -> Ptr (<git_transport>) -> IO CInt
{- typedef struct git_smart_subtransport_definition {
            git_smart_subtransport_cb callback; unsigned rpc;
        } git_smart_subtransport_definition; -}
#starttype git_smart_subtransport_definition
#field callback , <git_smart_subtransport_cb>
#field rpc , CUInt
#stoptype
#ccall git_smart_subtransport_http , Ptr (Ptr <git_smart_subtransport>) -> Ptr <git_transport> -> IO (CInt)
#ccall git_smart_subtransport_git , Ptr (Ptr <git_smart_subtransport>) -> Ptr <git_transport> -> IO (CInt)
