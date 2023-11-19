{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/commit.h>
module Bindings.Libgit2.Commit where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
import Bindings.Libgit2.Buffer
#ccall git_commit_lookup , Ptr (Ptr <struct git_commit>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> IO CInt
#ccall git_commit_lookup_prefix , Ptr (Ptr <struct git_commit>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> CSize -> IO CInt
#ccall git_commit_free , Ptr <struct git_commit> -> IO ()
#ccall git_commit_id , Ptr <struct git_commit> -> IO (Ptr <struct git_oid>)
#ccall git_commit_owner , Ptr <struct git_commit> -> IO (Ptr <struct git_repository>)
#ccall git_commit_message_encoding , Ptr <struct git_commit> -> IO CString
#ccall git_commit_message , Ptr <struct git_commit> -> IO CString
#ccall git_commit_message_raw , Ptr <struct git_commit> -> IO CString
#ccall git_commit_summary , Ptr <struct git_commit> -> IO CString
#ccall git_commit_body , Ptr <struct git_commit> -> IO CString
#ccall git_commit_time , Ptr <struct git_commit> -> IO CLong
#ccall git_commit_time_offset , Ptr <struct git_commit> -> IO CInt
#ccall git_commit_committer , Ptr <struct git_commit> -> IO (Ptr <struct git_signature>)
#ccall git_commit_author , Ptr <struct git_commit> -> IO (Ptr <struct git_signature>)
#ccall git_commit_committer_with_mailmap , Ptr (Ptr <struct git_signature>) -> Ptr <struct git_commit> -> Ptr <struct git_mailmap> -> IO CInt
#ccall git_commit_author_with_mailmap , Ptr (Ptr <struct git_signature>) -> Ptr <struct git_commit> -> Ptr <struct git_mailmap> -> IO CInt
#ccall git_commit_raw_header , Ptr <struct git_commit> -> IO CString
#ccall git_commit_tree , Ptr (Ptr <struct git_tree>) -> Ptr <struct git_commit> -> IO CInt
#ccall git_commit_tree_id , Ptr <struct git_commit> -> IO (Ptr <struct git_oid>)
#ccall git_commit_parentcount , Ptr <struct git_commit> -> IO CUInt
#ccall git_commit_parent , Ptr (Ptr <struct git_commit>) -> Ptr <struct git_commit> -> CUInt -> IO CInt
#ccall git_commit_parent_id , Ptr <struct git_commit> -> CUInt -> IO (Ptr <struct git_oid>)
#ccall git_commit_nth_gen_ancestor , Ptr (Ptr <struct git_commit>) -> Ptr <struct git_commit> -> CUInt -> IO CInt
#ccall git_commit_header_field , Ptr <git_buf> -> Ptr <struct git_commit> -> CString -> IO CInt
#ccall git_commit_extract_signature , Ptr <git_buf> -> Ptr <git_buf> -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_commit_create , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> CString -> CString -> Ptr <struct git_tree> -> CSize -> Ptr (Ptr <struct git_commit>) -> IO CInt
#ccall git_commit_create_v , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> CString -> CString -> Ptr <struct git_tree> -> CSize -> IO CInt
#ccall git_commit_amend , Ptr <struct git_oid> -> Ptr <struct git_commit> -> CString -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> CString -> CString -> Ptr <struct git_tree> -> IO CInt
#ccall git_commit_create_buffer , Ptr <git_buf> -> Ptr <struct git_repository> -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> CString -> CString -> Ptr <struct git_tree> -> CSize -> Ptr (Ptr <struct git_commit>) -> IO CInt
#ccall git_commit_create_with_signature , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> CString -> CString -> IO CInt
#ccall git_commit_dup , Ptr (Ptr <struct git_commit>) -> Ptr <struct git_commit> -> IO CInt
#callback git_commit_create_cb , Ptr <struct git_oid> -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> CString -> CString -> Ptr <struct git_tree> -> CSize -> Ptr (Ptr <struct git_commit>) -> Ptr () -> IO CInt
