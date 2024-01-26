{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/clone.h>
module Bindings.Libgit2.Clone where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Checkout
import Bindings.Libgit2.Remote
import Bindings.Libgit2.Transport
{- typedef enum {
            GIT_CLONE_LOCAL_AUTO,
            GIT_CLONE_LOCAL,
            GIT_CLONE_NO_LOCAL,
            GIT_CLONE_LOCAL_NO_LINKS
        } git_clone_local_t; -}
#integral_t git_clone_local_t
#num GIT_CLONE_LOCAL_AUTO
#num GIT_CLONE_LOCAL
#num GIT_CLONE_NO_LOCAL
#num GIT_CLONE_LOCAL_NO_LINKS
#callback git_remote_create_cb , Ptr (Ptr <struct git_remote>) -> Ptr <struct git_repository> -> CString -> CString -> Ptr () -> IO CInt
#callback git_repository_create_cb , Ptr (Ptr <struct git_repository>) -> CString -> CInt -> Ptr () -> IO CInt
{- typedef struct git_clone_options {
            unsigned int version;
            git_checkout_options checkout_opts;
            git_fetch_options fetch_opts;
            int bare;
            git_clone_local_t local;
            const char * checkout_branch;
            git_repository_create_cb repository_cb;
            void * repository_cb_payload;
            git_remote_create_cb remote_cb;
            void * remote_cb_payload;
        } git_clone_options; -}
#starttype struct git_clone_options
#field version , CUInt
#field checkout_opts , <struct git_checkout_options>
#field fetch_opts , <git_fetch_options>
#field bare , CInt
#field local , <git_clone_local_t>
#field checkout_branch , CString
#field repository_cb , <git_repository_create_cb>
#field repository_cb_payload , Ptr ()
#field remote_cb , <git_remote_create_cb>
#field remote_cb_payload , Ptr ()
#stoptype
#ccall git_clone_options_init , Ptr <struct git_clone_options> -> CUInt -> IO CInt
#ccall git_clone , Ptr (Ptr <struct git_repository>) -> CString -> CString -> Ptr <struct git_clone_options> -> IO CInt
