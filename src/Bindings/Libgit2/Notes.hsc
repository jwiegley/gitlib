#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Notes where
#strict_import

import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
#ccall git_note_read , Ptr (Ptr <git_note>) -> Ptr <git_repository> -> CString -> Ptr <git_oid> -> IO (CInt)
#ccall git_note_message , Ptr <git_note> -> IO (CString)
#ccall git_note_oid , Ptr <git_note> -> IO (Ptr <git_oid>)
#ccall git_note_create , Ptr <git_oid> -> Ptr <git_repository> -> Ptr <git_signature> -> Ptr <git_signature> -> CString -> Ptr <git_oid> -> CString -> IO (CInt)
#ccall git_note_remove , Ptr <git_repository> -> CString -> Ptr <git_signature> -> Ptr <git_signature> -> Ptr <git_oid> -> IO (CInt)
#ccall git_note_free , Ptr <git_note> -> IO ()
#ccall git_note_default_ref , Ptr (CString) -> Ptr <git_repository> -> IO (CInt)
{- typedef struct {
            git_oid blob_oid; git_oid annotated_object_oid;
        } git_note_data; -}
#starttype git_note_data
#field blob_oid , <git_oid>
#field annotated_object_oid , <git_oid>
#stoptype
#ccall git_note_foreach , Ptr <git_repository> -> CString -> Ptr () -> Ptr () -> IO (CInt)
