{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/cert.h>
module Bindings.Libgit2.Cert where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- typedef enum git_cert_t {
            GIT_CERT_NONE,
            GIT_CERT_X509,
            GIT_CERT_HOSTKEY_LIBSSH2,
            GIT_CERT_STRARRAY
        } git_cert_t; -}
#integral_t enum git_cert_t
#num GIT_CERT_NONE
#num GIT_CERT_X509
#num GIT_CERT_HOSTKEY_LIBSSH2
#num GIT_CERT_STRARRAY
{- struct git_cert {
    git_cert_t cert_type;
}; -}
#starttype struct git_cert
#field cert_type , <enum git_cert_t>
#stoptype
#callback git_transport_certificate_check_cb , Ptr <struct git_cert> -> CInt -> CString -> Ptr () -> IO CInt
{- typedef enum {
            GIT_CERT_SSH_MD5 = 1 << 0,
            GIT_CERT_SSH_SHA1 = 1 << 1,
            GIT_CERT_SSH_SHA256 = 1 << 2,
            GIT_CERT_SSH_RAW = 1 << 3
        } git_cert_ssh_t; -}
#integral_t git_cert_ssh_t
#num GIT_CERT_SSH_MD5
#num GIT_CERT_SSH_SHA1
#num GIT_CERT_SSH_SHA256
#num GIT_CERT_SSH_RAW
{- typedef enum {
            GIT_CERT_SSH_RAW_TYPE_UNKNOWN = 0,
            GIT_CERT_SSH_RAW_TYPE_RSA = 1,
            GIT_CERT_SSH_RAW_TYPE_DSS = 2,
            GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_256 = 3,
            GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_384 = 4,
            GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_521 = 5,
            GIT_CERT_SSH_RAW_TYPE_KEY_ED25519 = 6
        } git_cert_ssh_raw_type_t; -}
#integral_t git_cert_ssh_raw_type_t
#num GIT_CERT_SSH_RAW_TYPE_UNKNOWN
#num GIT_CERT_SSH_RAW_TYPE_RSA
#num GIT_CERT_SSH_RAW_TYPE_DSS
#num GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_256
#num GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_384
#num GIT_CERT_SSH_RAW_TYPE_KEY_ECDSA_521
#num GIT_CERT_SSH_RAW_TYPE_KEY_ED25519
{- typedef struct {
            git_cert parent;
            git_cert_ssh_t type;
            unsigned char hash_md5[16];
            unsigned char hash_sha1[20];
            unsigned char hash_sha256[32];
            git_cert_ssh_raw_type_t raw_type;
            const char * hostkey;
            size_t hostkey_len;
        } git_cert_hostkey; -}
#starttype git_cert_hostkey
#field parent , <struct git_cert>
#field type , <git_cert_ssh_t>
#array_field hash_md5 , CUChar
#array_field hash_sha1 , CUChar
#array_field hash_sha256 , CUChar
#field raw_type , <git_cert_ssh_raw_type_t>
#field hostkey , CString
#field hostkey_len , CSize
#stoptype
{- typedef struct {
            git_cert parent; void * data; size_t len;
        } git_cert_x509; -}
#starttype git_cert_x509
#field parent , <struct git_cert>
#field data , Ptr ()
#field len , CSize
#stoptype
