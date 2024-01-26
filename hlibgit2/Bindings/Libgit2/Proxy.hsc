{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/proxy.h>
module Bindings.Libgit2.Proxy where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Cert
import Bindings.Libgit2.Credential
{- typedef enum {
            GIT_PROXY_NONE, GIT_PROXY_AUTO, GIT_PROXY_SPECIFIED
        } git_proxy_t; -}
#integral_t git_proxy_t
#num GIT_PROXY_NONE
#num GIT_PROXY_AUTO
#num GIT_PROXY_SPECIFIED
{- typedef struct {
            unsigned int version;
            git_proxy_t type;
            const char * url;
            git_credential_acquire_cb credentials;
            git_transport_certificate_check_cb certificate_check;
            void * payload;
        } git_proxy_options; -}
#starttype git_proxy_options
#field version , CUInt
#field type , <git_proxy_t>
#field url , CString
#field credentials , <git_credential_acquire_cb>
#field certificate_check , <git_transport_certificate_check_cb>
#field payload , Ptr ()
#stoptype
#ccall git_proxy_options_init , Ptr <git_proxy_options> -> CUInt -> IO CInt
