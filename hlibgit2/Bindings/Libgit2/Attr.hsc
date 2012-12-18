#include <bindings.dsl.h>
#include <git2.h>
#include <git2/attr.h>
module Bindings.Libgit2.Attr where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types

{- extern __attribute__((visibility("default"))) const char * git_attr__true; -}
{- extern __attribute__((visibility("default"))) const char * git_attr__false; -}
{- extern __attribute__((visibility("default"))) const char * git_attr__unset; -}

#ccall git_attr_get , Ptr (CString) -> Ptr <git_repository> -> CUInt -> CString -> CString -> IO (CInt)
#ccall git_attr_get_many , Ptr (CString) -> Ptr <git_repository> -> CUInt -> CString -> CSize -> Ptr (CString) -> IO (CInt)
#callback git_attr_foreach_callback , CString -> CString -> Ptr () -> IO CInt
#ccall git_attr_foreach , Ptr <git_repository> -> CUInt -> CString -> <git_attr_foreach_callback> -> Ptr () -> IO (CInt)
#ccall git_attr_cache_flush , Ptr <git_repository> -> IO ()
#ccall git_attr_add_macro , Ptr <git_repository> -> CString -> CString -> IO (CInt)
