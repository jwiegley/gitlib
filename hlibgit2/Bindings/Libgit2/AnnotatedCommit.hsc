{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/annotated_commit.h>
module Bindings.Libgit2.AnnotatedCommit where
import Foreign.Ptr
import Bindings.Libgit2.Oid
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Repository
import Bindings.Libgit2.Types
#ccall git_annotated_commit_from_ref , Ptr (Ptr <struct git_annotated_commit>) -> Ptr <struct git_repository> -> Ptr <struct git_reference> -> IO CInt
#ccall git_annotated_commit_from_fetchhead , Ptr (Ptr <struct git_annotated_commit>) -> Ptr <struct git_repository> -> CString -> CString -> Ptr <struct git_oid> -> IO CInt
#ccall git_annotated_commit_lookup , Ptr (Ptr <struct git_annotated_commit>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> IO CInt
#ccall git_annotated_commit_from_revspec , Ptr (Ptr <struct git_annotated_commit>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_annotated_commit_id , Ptr <struct git_annotated_commit> -> IO (Ptr <struct git_oid>)
#ccall git_annotated_commit_ref , Ptr <struct git_annotated_commit> -> IO CString
#ccall git_annotated_commit_free , Ptr <struct git_annotated_commit> -> IO ()
