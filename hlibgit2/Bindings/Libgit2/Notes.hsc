{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/notes.h>
module Bindings.Libgit2.Notes where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
import Bindings.Libgit2.Buffer
#callback git_note_foreach_cb , Ptr <struct git_oid> -> Ptr <struct git_oid> -> Ptr () -> IO CInt
{- typedef struct git_iterator git_note_iterator; -}
#opaque_t struct git_iterator
#synonym_t git_note_iterator , <struct git_iterator>
#ccall git_note_iterator_new , Ptr (Ptr <struct git_iterator>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_note_commit_iterator_new , Ptr (Ptr <struct git_iterator>) -> Ptr <struct git_commit> -> IO CInt
#ccall git_note_iterator_free , Ptr <struct git_iterator> -> IO ()
#ccall git_note_next , Ptr <struct git_oid> -> Ptr <struct git_oid> -> Ptr <struct git_iterator> -> IO CInt
#ccall git_note_read , Ptr (Ptr <struct git_note>) -> Ptr <struct git_repository> -> CString -> Ptr <struct git_oid> -> IO CInt
#ccall git_note_commit_read , Ptr (Ptr <struct git_note>) -> Ptr <struct git_repository> -> Ptr <struct git_commit> -> Ptr <struct git_oid> -> IO CInt
#ccall git_note_author , Ptr <struct git_note> -> IO (Ptr <struct git_signature>)
#ccall git_note_committer , Ptr <struct git_note> -> IO (Ptr <struct git_signature>)
#ccall git_note_message , Ptr <struct git_note> -> IO CString
#ccall git_note_id , Ptr <struct git_note> -> IO (Ptr <struct git_oid>)
#ccall git_note_create , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> Ptr <struct git_oid> -> CString -> CInt -> IO CInt
#ccall git_note_commit_create , Ptr <struct git_oid> -> Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr <struct git_commit> -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> Ptr <struct git_oid> -> CString -> CInt -> IO CInt
#ccall git_note_remove , Ptr <struct git_repository> -> CString -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> Ptr <struct git_oid> -> IO CInt
#ccall git_note_commit_remove , Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr <struct git_commit> -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> Ptr <struct git_oid> -> IO CInt
#ccall git_note_free , Ptr <struct git_note> -> IO ()
#ccall git_note_default_ref , Ptr <git_buf> -> Ptr <struct git_repository> -> IO CInt
#ccall git_note_foreach , Ptr <struct git_repository> -> CString -> <git_note_foreach_cb> -> Ptr () -> IO CInt
