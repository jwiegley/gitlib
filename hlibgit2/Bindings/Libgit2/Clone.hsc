{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
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
{- typedef struct git_clone_options {
            unsigned int version;
            git_checkout_opts checkout_opts;
            int bare;
            git_transfer_progress_callback fetch_progress_cb;
            void * fetch_progress_payload;
            const char * remote_name;
            const char * pushurl;
            const char * fetch_spec;
            const char * push_spec;
            git_cred_acquire_cb cred_acquire_cb;
            void * cred_acquire_payload;
            git_transport * transport;
            git_remote_callbacks * remote_callbacks;
            git_remote_autotag_option_t remote_autotag;
            const char * checkout_branch;
        } git_clone_options; -}
#starttype git_clone_options
#field version , CUInt
#field checkout_opts , <git_checkout_opts>
#field bare , CInt
#field fetch_progress_cb , CInt
#field fetch_progress_payload , Ptr ()
#field remote_name , CString
#field pushurl , CString
#field fetch_spec , CString
#field push_spec , CString
#field cred_acquire_cb , CInt
#field cred_acquire_payload , Ptr ()
#field transport , Ptr <git_transport>
#field remote_callbacks , Ptr <git_remote_callbacks>
#field remote_autotag , <git_remote_autotag_option_t>
#field checkout_branch , CString
#stoptype
#ccall git_clone , Ptr (Ptr <git_repository>) -> CString -> CString -> Ptr <git_clone_options> -> IO (CInt)
