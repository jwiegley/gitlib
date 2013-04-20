{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/attr.h>
module Bindings.Libgit2.Attr where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- typedef enum {
            GIT_ATTR_UNSPECIFIED_T = 0,
            GIT_ATTR_TRUE_T,
            GIT_ATTR_FALSE_T,
            GIT_ATTR_VALUE_T
        } git_attr_t; -}
#integral_t git_attr_t
#num GIT_ATTR_UNSPECIFIED_T
#num GIT_ATTR_TRUE_T
#num GIT_ATTR_FALSE_T
#num GIT_ATTR_VALUE_T
#ccall git_attr_value , CString -> IO (<git_attr_t>)
#ccall git_attr_get , Ptr (CString) -> Ptr <git_repository> -> CUInt -> CString -> CString -> IO (CInt)
#ccall git_attr_get_many , Ptr (CString) -> Ptr <git_repository> -> CUInt -> CString -> CSize -> Ptr (CString) -> IO (CInt)
#callback git_attr_foreach_callback , CString -> CString -> Ptr () -> IO CInt
#ccall git_attr_foreach , Ptr <git_repository> -> CUInt -> CString -> <git_attr_foreach_callback> -> Ptr () -> IO (CInt)
#ccall git_attr_cache_flush , Ptr <git_repository> -> IO ()
#ccall git_attr_add_macro , Ptr <git_repository> -> CString -> CString -> IO (CInt)
