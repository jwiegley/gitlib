{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/branch.h>
module Bindings.Libgit2.Branch where
import Foreign.Ptr
import Bindings.Libgit2.Buffer
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
#ccall git_branch_create , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> Ptr <struct git_commit> -> CInt -> IO CInt
#ccall git_branch_create_from_annotated , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> Ptr <struct git_annotated_commit> -> CInt -> IO CInt
#ccall git_branch_delete , Ptr <struct git_reference> -> IO CInt
{- typedef struct git_branch_iterator git_branch_iterator; -}
#opaque_t struct git_branch_iterator
#ccall git_branch_iterator_new , Ptr (Ptr <struct git_branch_iterator>) -> Ptr <struct git_repository> -> <git_branch_t> -> IO CInt
#ccall git_branch_next , Ptr (Ptr <struct git_reference>) -> Ptr <git_branch_t> -> Ptr <struct git_branch_iterator> -> IO CInt
#ccall git_branch_iterator_free , Ptr <struct git_branch_iterator> -> IO ()
#ccall git_branch_move , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> CString -> CInt -> IO CInt
#ccall git_branch_lookup , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> <git_branch_t> -> IO CInt
#ccall git_branch_name , Ptr CString -> Ptr <struct git_reference> -> IO CInt
#ccall git_branch_upstream , Ptr (Ptr <struct git_reference>) -> Ptr <struct git_reference> -> IO CInt
#ccall git_branch_set_upstream , Ptr <struct git_reference> -> CString -> IO CInt
#ccall git_branch_upstream_name , Ptr <git_buf> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_branch_is_head , Ptr <struct git_reference> -> IO CInt
#ccall git_branch_is_checked_out , Ptr <struct git_reference> -> IO CInt
#ccall git_branch_remote_name , Ptr <git_buf> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_branch_upstream_remote , Ptr <git_buf> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_branch_upstream_merge , Ptr <git_buf> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_branch_name_is_valid , Ptr CInt -> CString -> IO CInt
