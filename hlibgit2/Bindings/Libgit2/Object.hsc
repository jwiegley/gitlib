{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/object.h>
module Bindings.Libgit2.Object where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Buffer
#ccall git_object_lookup , Ptr (Ptr <struct git_object>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> <git_object_t> -> IO CInt
#ccall git_object_lookup_prefix , Ptr (Ptr <struct git_object>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> CSize -> <git_object_t> -> IO CInt
#ccall git_object_lookup_bypath , Ptr (Ptr <struct git_object>) -> Ptr <struct git_object> -> CString -> <git_object_t> -> IO CInt
#ccall git_object_id , Ptr <struct git_object> -> IO (Ptr <struct git_oid>)
#ccall git_object_short_id , Ptr <git_buf> -> Ptr <struct git_object> -> IO CInt
#ccall git_object_type , Ptr <struct git_object> -> IO <git_object_t>
#ccall git_object_owner , Ptr <struct git_object> -> IO (Ptr <struct git_repository>)
#ccall git_object_free , Ptr <struct git_object> -> IO ()
#ccall git_object_type2string , <git_object_t> -> IO CString
#ccall git_object_string2type , CString -> IO <git_object_t>
#ccall git_object_typeisloose , <git_object_t> -> IO CInt
#ccall git_object_peel , Ptr (Ptr <struct git_object>) -> Ptr <struct git_object> -> <git_object_t> -> IO CInt
#ccall git_object_dup , Ptr (Ptr <struct git_object>) -> Ptr <struct git_object> -> IO CInt
#ccall git_object_rawcontent_is_valid , Ptr CInt -> CString -> CSize -> <git_object_t> -> IO CInt
