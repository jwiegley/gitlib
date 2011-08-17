
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Commit where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
#ccall git_commit_id , Ptr <git_commit> -> IO (Ptr <git_oid>)
#ccall git_commit_message_short , Ptr <git_commit> -> IO (CString)
#ccall git_commit_message , Ptr <git_commit> -> IO (CString)
#ccall git_commit_time , Ptr <git_commit> -> IO (CTime)
#ccall git_commit_time_offset , Ptr <git_commit> -> IO (CInt)
#ccall git_commit_committer , Ptr <git_commit> -> IO (Ptr <git_signature>)
#ccall git_commit_author , Ptr <git_commit> -> IO (Ptr <git_signature>)
#ccall git_commit_tree , Ptr (Ptr <git_tree>) -> Ptr <git_commit> -> IO (CInt)
#ccall git_commit_tree_oid , Ptr <git_commit> -> IO (Ptr <git_oid>)
#ccall git_commit_parentcount , Ptr <git_commit> -> IO (CUInt)
#ccall git_commit_parent , Ptr (Ptr <git_commit>) -> Ptr <git_commit> -> CUInt -> IO (CInt)
#ccall git_commit_parent_oid , Ptr <git_commit> -> CUInt -> IO (Ptr <git_oid>)
#ccall git_commit_create , Ptr <git_oid> -> Ptr <git_repository> -> CString -> Ptr <git_signature> -> Ptr <git_signature> -> CString -> Ptr <git_tree> -> CInt -> Ptr (Ptr <git_commit>) -> IO (CInt)
#cinline git_commit_lookup , Ptr (Ptr <git_commit>) -> Ptr <git_repository> -> Ptr <git_oid> -> IO (CInt)
#cinline git_commit_lookup_prefix , Ptr (Ptr <git_commit>) -> Ptr <git_repository> -> Ptr <git_oid> -> CUInt -> IO (CInt)
#cinline git_commit_close , Ptr <git_commit> -> IO ()
