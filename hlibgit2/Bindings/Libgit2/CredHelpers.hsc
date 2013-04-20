{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/cred_helpers.h>
module Bindings.Libgit2.CredHelpers where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Transport
{- typedef struct git_cred_userpass_payload {
            char * username; char * password;
        } git_cred_userpass_payload; -}
#starttype git_cred_userpass_payload
#field username , CString
#field password , CString
#stoptype
#ccall git_cred_userpass , Ptr (Ptr <git_cred>) -> CString -> CString -> CUInt -> Ptr () -> IO (CInt)
