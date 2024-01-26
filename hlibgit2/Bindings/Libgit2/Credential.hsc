{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/credential.h>
module Bindings.Libgit2.Credential where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
{- typedef enum {
            GIT_CREDENTIAL_USERPASS_PLAINTEXT = 1u << 0,
            GIT_CREDENTIAL_SSH_KEY = 1u << 1,
            GIT_CREDENTIAL_SSH_CUSTOM = 1u << 2,
            GIT_CREDENTIAL_DEFAULT = 1u << 3,
            GIT_CREDENTIAL_SSH_INTERACTIVE = 1u << 4,
            GIT_CREDENTIAL_USERNAME = 1u << 5,
            GIT_CREDENTIAL_SSH_MEMORY = 1u << 6
        } git_credential_t; -}
#integral_t git_credential_t
#num GIT_CREDENTIAL_USERPASS_PLAINTEXT
#num GIT_CREDENTIAL_SSH_KEY
#num GIT_CREDENTIAL_SSH_CUSTOM
#num GIT_CREDENTIAL_DEFAULT
#num GIT_CREDENTIAL_SSH_INTERACTIVE
#num GIT_CREDENTIAL_USERNAME
#num GIT_CREDENTIAL_SSH_MEMORY
{- typedef struct git_credential git_credential; -}
#opaque_t struct git_credential
{- typedef struct git_credential_userpass_plaintext git_credential_userpass_plaintext; -}
#opaque_t struct git_credential_userpass_plaintext
{- typedef struct git_credential_username git_credential_username; -}
#opaque_t struct git_credential_username
{- typedef struct git_credential git_credential_default; -}
#synonym_t git_credential_default , <struct git_credential>
{- typedef struct git_credential_ssh_key git_credential_ssh_key; -}
#opaque_t struct git_credential_ssh_key
{- typedef struct git_credential_ssh_interactive git_credential_ssh_interactive; -}
#opaque_t struct git_credential_ssh_interactive
{- typedef struct git_credential_ssh_custom git_credential_ssh_custom; -}
#opaque_t struct git_credential_ssh_custom
#callback git_credential_acquire_cb , Ptr (Ptr <struct git_credential>) -> CString -> CString -> CUInt -> Ptr () -> IO CInt
#ccall git_credential_free , Ptr <struct git_credential> -> IO ()
#ccall git_credential_has_username , Ptr <struct git_credential> -> IO CInt
#ccall git_credential_get_username , Ptr <struct git_credential> -> IO CString
#ccall git_credential_userpass_plaintext_new , Ptr (Ptr <struct git_credential>) -> CString -> CString -> IO CInt
#ccall git_credential_default_new , Ptr (Ptr <struct git_credential>) -> IO CInt
#ccall git_credential_username_new , Ptr (Ptr <struct git_credential>) -> CString -> IO CInt
#ccall git_credential_ssh_key_new , Ptr (Ptr <struct git_credential>) -> CString -> CString -> CString -> CString -> IO CInt
#ccall git_credential_ssh_key_memory_new , Ptr (Ptr <struct git_credential>) -> CString -> CString -> CString -> CString -> IO CInt
{- typedef struct _LIBSSH2_SESSION LIBSSH2_SESSION; -}
#opaque_t struct _LIBSSH2_SESSION
#synonym_t LIBSSH2_SESSION , <struct _LIBSSH2_SESSION>
{- typedef struct _LIBSSH2_USERAUTH_KBDINT_PROMPT LIBSSH2_USERAUTH_KBDINT_PROMPT; -}
#opaque_t struct _LIBSSH2_USERAUTH_KBDINT_PROMPT
#synonym_t LIBSSH2_USERAUTH_KBDINT_PROMPT , <struct _LIBSSH2_USERAUTH_KBDINT_PROMPT>
{- typedef struct _LIBSSH2_USERAUTH_KBDINT_RESPONSE LIBSSH2_USERAUTH_KBDINT_RESPONSE; -}
#opaque_t struct _LIBSSH2_USERAUTH_KBDINT_RESPONSE
#synonym_t LIBSSH2_USERAUTH_KBDINT_RESPONSE , <struct _LIBSSH2_USERAUTH_KBDINT_RESPONSE>
#callback git_credential_ssh_interactive_cb , CString -> CInt -> CString -> CInt -> CInt -> Ptr <struct _LIBSSH2_USERAUTH_KBDINT_PROMPT> -> Ptr <struct _LIBSSH2_USERAUTH_KBDINT_RESPONSE> -> Ptr (Ptr ()) -> IO ()
#ccall git_credential_ssh_interactive_new , Ptr (Ptr <struct git_credential>) -> CString -> <git_credential_ssh_interactive_cb> -> Ptr () -> IO CInt
#ccall git_credential_ssh_key_from_agent , Ptr (Ptr <struct git_credential>) -> CString -> IO CInt
#callback git_credential_sign_cb , Ptr <struct _LIBSSH2_SESSION> -> Ptr (Ptr CUChar) -> Ptr CSize -> Ptr CUChar -> CSize -> Ptr (Ptr ()) -> IO CInt
#ccall git_credential_ssh_custom_new , Ptr (Ptr <struct git_credential>) -> CString -> CString -> CSize -> <git_credential_sign_cb> -> Ptr () -> IO CInt
