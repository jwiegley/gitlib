{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/refs.h>
module Bindings.Libgit2.Refs where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Strarray
#ccall git_reference_lookup , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_name_to_id , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_dwim , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_symbolic_create_matching , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> CString -> CInt -> CString -> CString -> IO CInt
#ccall git_reference_symbolic_create , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> CString -> CInt -> CString -> IO CInt
#ccall git_reference_create , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> Ptr <struct git_oid> -> CInt -> CString -> IO CInt
#ccall git_reference_create_matching , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> Ptr <struct git_oid> -> CInt -> Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_reference_target , Ptr <struct git_reference> -> IO (Ptr <struct git_oid>)
#ccall git_reference_target_peel , Ptr <struct git_reference> -> IO (Ptr <struct git_oid>)
#ccall git_reference_symbolic_target , Ptr <struct git_reference> -> IO CString
#ccall git_reference_type , Ptr <struct git_reference> -> IO <git_reference_t>
#ccall git_reference_name , Ptr <struct git_reference> -> IO CString
#ccall git_reference_resolve , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> IO CInt
#ccall git_reference_owner , Ptr <struct git_reference> -> IO (Ptr <struct git_repository>)
#ccall git_reference_symbolic_set_target , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> CString -> CString -> IO CInt
#ccall git_reference_set_target , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_reference_rename , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> CString -> CInt -> CString -> IO CInt
#ccall git_reference_delete , Ptr <struct git_reference> -> IO CInt
#ccall git_reference_remove , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_list , Ptr <struct git_strarray> -> Ptr <struct git_repository> -> IO CInt
#callback git_reference_foreach_cb , Ptr <struct git_reference> -> Ptr () -> IO CInt
#callback git_reference_foreach_name_cb , CString -> Ptr () -> IO CInt
#ccall git_reference_foreach , Ptr <struct git_repository> -> <git_reference_foreach_cb> -> Ptr () -> IO CInt
#ccall git_reference_foreach_name , Ptr <struct git_repository> -> <git_reference_foreach_name_cb> -> Ptr () -> IO CInt
#ccall git_reference_dup , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> IO CInt
#ccall git_reference_free , Ptr <struct git_reference> -> IO ()
#ccall git_reference_cmp , Ptr <struct git_reference> -> Ptr <struct git_reference> -> IO CInt
#ccall git_reference_iterator_new , Ptr (Ptr <struct git_reference_iterator>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_reference_iterator_glob_new , Ptr (Ptr <struct git_reference_iterator>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_next , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference_iterator> -> IO CInt
#ccall git_reference_next_name , Ptr CString -> Ptr <struct git_reference_iterator> -> IO CInt
#ccall git_reference_iterator_free , Ptr <struct git_reference_iterator> -> IO ()
#ccall git_reference_foreach_glob , Ptr <struct git_repository> -> CString -> <git_reference_foreach_name_cb> -> Ptr () -> IO CInt
#ccall git_reference_has_log , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_ensure_log , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reference_is_branch , Ptr <struct git_reference> -> IO CInt
#ccall git_reference_is_remote , Ptr <struct git_reference> -> IO CInt
#ccall git_reference_is_tag , Ptr <struct git_reference> -> IO CInt
#ccall git_reference_is_note , Ptr <struct git_reference> -> IO CInt
{- typedef enum {
            GIT_REFERENCE_FORMAT_NORMAL = 0u,
            GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL = 1u << 0,
            GIT_REFERENCE_FORMAT_REFSPEC_PATTERN = 1u << 1,
            GIT_REFERENCE_FORMAT_REFSPEC_SHORTHAND = 1u << 2
        } git_reference_format_t; -}
#integral_t git_reference_format_t
#num GIT_REFERENCE_FORMAT_NORMAL
#num GIT_REFERENCE_FORMAT_ALLOW_ONELEVEL
#num GIT_REFERENCE_FORMAT_REFSPEC_PATTERN
#num GIT_REFERENCE_FORMAT_REFSPEC_SHORTHAND
#ccall git_reference_normalize_name , CString -> CSize -> CString -> CUInt -> IO CInt
#ccall git_reference_peel , Ptr (Ptr <struct git_object>) -> Ptr <struct git_reference> -> <git_object_t> -> IO CInt
#ccall git_reference_name_is_valid , Ptr CInt -> CString -> IO CInt
#ccall git_reference_shorthand , Ptr <struct git_reference> -> IO CString
