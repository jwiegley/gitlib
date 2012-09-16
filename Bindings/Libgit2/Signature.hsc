#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Signature where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_signature_new , Ptr (Ptr <git_signature>) -> CString -> CString -> CLong -> CInt -> IO (CInt)
#ccall git_signature_now , Ptr (Ptr <git_signature>) -> CString -> CString -> IO (CInt)
#ccall git_signature_dup , Ptr <git_signature> -> IO (Ptr <git_signature>)
#ccall git_signature_free , Ptr <git_signature> -> IO ()
