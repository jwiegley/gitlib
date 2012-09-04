#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Refs where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_reference_lookup , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_reference_name_to_oid , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_reference_create_symbolic , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> CString -> CInt -> IO (CInt)
#ccall git_reference_create_oid , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> Ptr <git_oid> -> CInt -> IO (CInt)
#ccall git_reference_oid , Ptr <git_reference> -> IO (Ptr <git_oid>)
#ccall git_reference_target , Ptr <git_reference> -> IO (CString)
#ccall git_reference_type , Ptr <git_reference> -> IO (<git_ref_t>)
#ccall git_reference_name , Ptr <git_reference> -> IO (CString)
#ccall git_reference_resolve , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> IO (CInt)
#ccall git_reference_owner , Ptr <git_reference> -> IO (Ptr <git_repository>)
#ccall git_reference_set_target , Ptr <git_reference> -> CString -> IO (CInt)
#ccall git_reference_set_oid , Ptr <git_reference> -> Ptr <git_oid> -> IO (CInt)
#ccall git_reference_rename , Ptr <git_reference> -> CString -> CInt -> IO (CInt)
#ccall git_reference_delete , Ptr <git_reference> -> IO (CInt)
#ccall git_reference_packall , Ptr <git_repository> -> IO (CInt)
#ccall git_reference_list , Ptr <git_strarray> -> Ptr <git_repository> -> CUInt -> IO (CInt)
#ccall git_reference_foreach , Ptr <git_repository> -> CUInt -> Ptr () -> Ptr () -> IO (CInt)
#ccall git_reference_is_packed , Ptr <git_reference> -> IO (CInt)
#ccall git_reference_reload , Ptr <git_reference> -> IO (CInt)
#ccall git_reference_free , Ptr <git_reference> -> IO ()
#ccall git_reference_cmp , Ptr <git_reference> -> Ptr <git_reference> -> IO (CInt)
