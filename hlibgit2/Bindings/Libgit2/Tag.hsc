{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/tag.h>
module Bindings.Libgit2.Tag where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
import Bindings.Libgit2.Strarray
#ccall git_tag_lookup , Ptr (Ptr <struct git_tag>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> IO CInt
#ccall git_tag_lookup_prefix , Ptr (Ptr <struct git_tag>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> CSize -> IO CInt
#ccall git_tag_free , Ptr <struct git_tag> -> IO ()
#ccall git_tag_id , Ptr <struct git_tag> -> IO (Ptr <struct git_oid>)
#ccall git_tag_owner , Ptr <struct git_tag> -> IO (Ptr <struct git_repository>)
#ccall git_tag_target , Ptr (Ptr <struct git_object>) -> Ptr <struct git_tag> -> IO CInt
#ccall git_tag_target_id , Ptr <struct git_tag> -> IO (Ptr <struct git_oid>)
#ccall git_tag_target_type , Ptr <struct git_tag> -> IO <git_object_t>
#ccall git_tag_name , Ptr <struct git_tag> -> IO CString
#ccall git_tag_tagger , Ptr <struct git_tag> -> IO (Ptr <struct git_signature>)
#ccall git_tag_message , Ptr <struct git_tag> -> IO CString
#ccall git_tag_create , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_object> -> Ptr <struct git_signature> -> CString -> CInt -> IO CInt
#ccall git_tag_annotation_create , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_object> -> Ptr <struct git_signature> -> CString -> IO CInt
#ccall git_tag_create_from_buffer , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> CInt -> IO CInt
#ccall git_tag_create_lightweight , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_object> -> CInt -> IO CInt
#ccall git_tag_delete , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_tag_list , Ptr <struct git_strarray> -> Ptr <struct git_repository> -> IO CInt
#ccall git_tag_list_match , Ptr <struct git_strarray> -> CString -> Ptr <struct git_repository> -> IO CInt
#callback git_tag_foreach_cb , CString -> Ptr <struct git_oid> -> Ptr () -> IO CInt
#ccall git_tag_foreach , Ptr <struct git_repository> -> <git_tag_foreach_cb> -> Ptr () -> IO CInt
#ccall git_tag_peel , Ptr (Ptr <struct git_object>) -> Ptr <struct git_tag> -> IO CInt
#ccall git_tag_dup , Ptr (Ptr <struct git_tag>) -> Ptr <struct git_tag> -> IO CInt
#ccall git_tag_name_is_valid , Ptr CInt -> CString -> IO CInt
