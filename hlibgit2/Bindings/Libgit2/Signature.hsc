{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/signature.h>
module Bindings.Libgit2.Signature where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_signature_new , Ptr (Ptr <struct git_signature>) -> CString -> CString -> CLong -> CInt -> IO CInt
#ccall git_signature_now , Ptr (Ptr <struct git_signature>) -> CString -> CString -> IO CInt
#ccall git_signature_default , Ptr (Ptr <struct git_signature>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_signature_from_buffer , Ptr (Ptr <struct git_signature>) -> CString -> IO CInt
#ccall git_signature_dup , Ptr (Ptr <struct git_signature>) -> Ptr <struct git_signature> -> IO CInt
#ccall git_signature_free , Ptr <struct git_signature> -> IO ()
