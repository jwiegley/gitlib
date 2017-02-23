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

{- typedef enum {
	 GIT_CLONE_LOCAL_AUTO,
	 GIT_CLONE_LOCAL,
	 GIT_CLONE_NO_LOCAL,
	 GIT_CLONE_LOCAL_NO_LINKS,
 } git_clone_local_t; -}
#integral_t git_clone_local_t
#num GIT_CLONE_LOCAL_AUTO
#num GIT_CLONE_LOCAL
#num GIT_CLONE_NO_LOCAL
#num GIT_CLONE_LOCAL_NO_LINKS

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
#field checkout_opts , <git_checkout_options>
#field fetch_opts , <git_fetch_options>
#field bare , CInt
#field local , <git_clone_local_t>
#field checkout_branch , CString
#field repository_cb , <git_repository_create_cb>
#field repository_cb_payload , Ptr ()
#field remote_cb , <git_remote_create_cb>
#field remote_cb_payload , Ptr ()
#stoptype

#callback git_remote_create_cb , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> CString -> Ptr () -> IO CInt
#callback git_repository_create_cb , Ptr (Ptr <git_repository>) -> CString -> CInt -> Ptr () -> IO CInt

#ccall git_clone , Ptr (Ptr <git_repository>) -> CString -> CString -> Ptr <git_clone_options> -> IO (CInt)
