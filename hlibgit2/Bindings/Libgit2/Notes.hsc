{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Notes where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
{- typedef int (* git_note_foreach_cb)(const git_oid * blob_id,
                                    const git_oid * annotated_object_id,
                                    void * payload); -}
#callback git_note_foreach_cb , Ptr (<git_oid>) -> Ptr (<git_oid>) -> Ptr () -> IO CInt
{- typedef struct git_iterator git_note_iterator; -}
#opaque_t git_iterator
#synonym_t git_note_iterator , <git_iterator>
#ccall git_note_iterator_new , Ptr (Ptr <git_iterator>) -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_note_iterator_free , Ptr <git_iterator> -> IO ()
#ccall git_note_next , Ptr <git_oid> -> Ptr <git_oid> -> Ptr <git_iterator> -> IO (CInt)
#ccall git_note_read , Ptr (Ptr <git_note>) -> Ptr <git_repository> -> CString -> Ptr <git_oid> -> IO (CInt)
#ccall git_note_message , Ptr <git_note> -> IO (CString)
#ccall git_note_oid , Ptr <git_note> -> IO (Ptr <git_oid>)
#ccall git_note_create , Ptr <git_oid> -> Ptr <git_repository> -> Ptr <git_signature> -> Ptr <git_signature> -> CString -> Ptr <git_oid> -> CString -> CInt -> IO (CInt)
#ccall git_note_remove , Ptr <git_repository> -> CString -> Ptr <git_signature> -> Ptr <git_signature> -> Ptr <git_oid> -> IO (CInt)
#ccall git_note_free , Ptr <git_note> -> IO ()
#ccall git_note_default_ref , Ptr (CString) -> Ptr <git_repository> -> IO (CInt)
#ccall git_note_foreach , Ptr <git_repository> -> CString -> <git_note_foreach_cb> -> Ptr () -> IO (CInt)
