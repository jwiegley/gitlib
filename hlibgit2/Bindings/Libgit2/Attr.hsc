{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/attr.h>
module Bindings.Libgit2.Attr where
import Foreign.Ptr
import Bindings.Libgit2.Oid
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- typedef enum {
            GIT_ATTR_VALUE_UNSPECIFIED = 0,
            GIT_ATTR_VALUE_TRUE,
            GIT_ATTR_VALUE_FALSE,
            GIT_ATTR_VALUE_STRING
        } git_attr_value_t; -}
#integral_t git_attr_value_t
#num GIT_ATTR_VALUE_UNSPECIFIED
#num GIT_ATTR_VALUE_TRUE
#num GIT_ATTR_VALUE_FALSE
#num GIT_ATTR_VALUE_STRING
#ccall git_attr_value , CString -> IO <git_attr_value_t>
{- typedef struct {
            unsigned int version;
            unsigned int flags;
            git_oid * commit_id;
            git_oid attr_commit_id;
        } git_attr_options; -}
#starttype git_attr_options
#field version , CUInt
#field flags , CUInt
#field commit_id , Ptr <struct git_oid>
#field attr_commit_id , <struct git_oid>
#stoptype
#ccall git_attr_get , Ptr CString -> Ptr <struct git_repository> -> CUInt -> CString -> CString -> IO CInt
#ccall git_attr_get_ext , Ptr CString -> Ptr <struct git_repository> -> Ptr <git_attr_options> -> CString -> CString -> IO CInt
#ccall git_attr_get_many , Ptr CString -> Ptr <struct git_repository> -> CUInt -> CString -> CSize -> Ptr CString -> IO CInt
#ccall git_attr_get_many_ext , Ptr CString -> Ptr <struct git_repository> -> Ptr <git_attr_options> -> CString -> CSize -> Ptr CString -> IO CInt
#callback git_attr_foreach_cb , CString -> CString -> Ptr () -> IO CInt
#ccall git_attr_foreach , Ptr <struct git_repository> -> CUInt -> CString -> <git_attr_foreach_cb> -> Ptr () -> IO CInt
#ccall git_attr_foreach_ext , Ptr <struct git_repository> -> Ptr <git_attr_options> -> CString -> <git_attr_foreach_cb> -> Ptr () -> IO CInt
#ccall git_attr_cache_flush , Ptr <struct git_repository> -> IO CInt
#ccall git_attr_add_macro , Ptr <struct git_repository> -> CString -> CString -> IO CInt
