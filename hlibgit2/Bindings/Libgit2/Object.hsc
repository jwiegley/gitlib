#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Object where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_object_lookup , Ptr (Ptr <git_object>) -> Ptr <git_repository> -> Ptr <git_oid> -> <git_otype> -> IO (CInt)
#ccall git_object_lookup_prefix , Ptr (Ptr <git_object>) -> Ptr <git_repository> -> Ptr <git_oid> -> CUInt -> <git_otype> -> IO (CInt)
#ccall git_object_id , Ptr <git_object> -> IO (Ptr <git_oid>)
#ccall git_object_type , Ptr <git_object> -> IO (<git_otype>)
#ccall git_object_owner , Ptr <git_object> -> IO (Ptr <git_repository>)
#ccall git_object_free , Ptr <git_object> -> IO ()
#ccall git_object_type2string , <git_otype> -> IO (CString)
#ccall git_object_string2type , CString -> IO (<git_otype>)
#ccall git_object_typeisloose , <git_otype> -> IO (CInt)
#ccall git_object__size , <git_otype> -> IO (CSize)
