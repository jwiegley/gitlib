{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Refs where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Strarray
#ccall git_reference_lookup , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_reference_name_to_id , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_reference_symbolic_create , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> CString -> CInt -> IO (CInt)
#ccall git_reference_create , Ptr (Ptr <git_reference>) -> Ptr <git_repository> -> CString -> Ptr <git_oid> -> CInt -> IO (CInt)
#ccall git_reference_target , Ptr <git_reference> -> IO (Ptr <git_oid>)
#ccall git_reference_symbolic_target , Ptr <git_reference> -> IO (CString)
#ccall git_reference_type , Ptr <git_reference> -> IO (<git_ref_t>)
#ccall git_reference_name , Ptr <git_reference> -> IO (CString)
#ccall git_reference_resolve , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> IO (CInt)
#ccall git_reference_owner , Ptr <git_reference> -> IO (Ptr <git_repository>)
#ccall git_reference_symbolic_set_target , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> CString -> IO (CInt)
#ccall git_reference_set_target , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> Ptr <git_oid> -> IO (CInt)
#ccall git_reference_rename , Ptr (Ptr <git_reference>) -> Ptr <git_reference> -> CString -> CInt -> IO (CInt)
#ccall git_reference_delete , Ptr <git_reference> -> IO (CInt)
#ccall git_reference_list , Ptr <git_strarray> -> Ptr <git_repository> -> CUInt -> IO (CInt)
{- typedef int (* git_reference_foreach_cb)(const char * refname,
                                         void * payload); -}
#callback git_reference_foreach_cb , CString -> Ptr () -> IO CInt
#ccall git_reference_foreach , Ptr <git_repository> -> CUInt -> <git_reference_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_reference_free , Ptr <git_reference> -> IO ()
#ccall git_reference_cmp , Ptr <git_reference> -> Ptr <git_reference> -> IO (CInt)
#ccall git_reference_foreach_glob , Ptr <git_repository> -> CString -> CUInt -> <git_reference_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_reference_has_log , Ptr <git_reference> -> IO (CInt)
#ccall git_reference_is_branch , Ptr <git_reference> -> IO (CInt)
#ccall git_reference_is_remote , Ptr <git_reference> -> IO (CInt)
{- typedef enum {
            GIT_REF_FORMAT_NORMAL = 0,
            GIT_REF_FORMAT_ALLOW_ONELEVEL = 1 << 0,
            GIT_REF_FORMAT_REFSPEC_PATTERN = 1 << 1
        } git_reference_normalize_t; -}
#integral_t git_reference_normalize_t
#num GIT_REF_FORMAT_NORMAL
#num GIT_REF_FORMAT_ALLOW_ONELEVEL
#num GIT_REF_FORMAT_REFSPEC_PATTERN
#ccall git_reference_normalize_name , CString -> CSize -> CString -> CUInt -> IO (CInt)
#ccall git_reference_peel , Ptr (Ptr <git_object>) -> Ptr <git_reference> -> <git_otype> -> IO (CInt)
#ccall git_reference_is_valid_name , CString -> IO (CInt)
