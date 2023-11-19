{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/rebase.h>
module Bindings.Libgit2.Rebase where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.AnnotatedCommit
import Bindings.Libgit2.Merge
import Bindings.Libgit2.Checkout
import Bindings.Libgit2.Commit
import Bindings.Libgit2.Buffer
{- typedef struct {
            unsigned int version;
            int quiet;
            int inmemory;
            const char * rewrite_notes_ref;
            git_merge_options merge_options;
            git_checkout_options checkout_options;
            git_commit_create_cb commit_create_cb;
            int (* signing_cb)(git_buf *, git_buf *, const char *, void *);
            void * payload;
        } git_rebase_options; -}
#starttype git_rebase_options
#field version , CUInt
#field quiet , CInt
#field inmemory , CInt
#field rewrite_notes_ref , CString
#field merge_options , <git_merge_options>
#field checkout_options , <struct git_checkout_options>
#field commit_create_cb , <git_commit_create_cb>
#field signing_cb , FunPtr (Ptr <git_buf> -> Ptr <git_buf> -> CString -> Ptr () -> CInt)
#field payload , Ptr ()
#stoptype
{- typedef enum {
            GIT_REBASE_OPERATION_PICK = 0,
            GIT_REBASE_OPERATION_REWORD,
            GIT_REBASE_OPERATION_EDIT,
            GIT_REBASE_OPERATION_SQUASH,
            GIT_REBASE_OPERATION_FIXUP,
            GIT_REBASE_OPERATION_EXEC
        } git_rebase_operation_t; -}
#integral_t git_rebase_operation_t
#num GIT_REBASE_OPERATION_PICK
#num GIT_REBASE_OPERATION_REWORD
#num GIT_REBASE_OPERATION_EDIT
#num GIT_REBASE_OPERATION_SQUASH
#num GIT_REBASE_OPERATION_FIXUP
#num GIT_REBASE_OPERATION_EXEC
{- typedef struct {
            git_rebase_operation_t type; const git_oid id; const char * exec;
        } git_rebase_operation; -}
#starttype git_rebase_operation
#field type , <git_rebase_operation_t>
#field id , <struct git_oid>
#field exec , CString
#stoptype
#ccall git_rebase_options_init , Ptr <git_rebase_options> -> CUInt -> IO CInt
#ccall git_rebase_init , Ptr (Ptr <struct git_rebase>) -> Ptr <struct git_repository> -> Ptr <struct git_annotated_commit> -> Ptr <struct git_annotated_commit> -> Ptr <struct git_annotated_commit> -> Ptr <git_rebase_options> -> IO CInt
#ccall git_rebase_open , Ptr (Ptr <struct git_rebase>) -> Ptr <struct git_repository> -> Ptr <git_rebase_options> -> IO CInt
#ccall git_rebase_orig_head_name , Ptr <struct git_rebase> -> IO CString
#ccall git_rebase_orig_head_id , Ptr <struct git_rebase> -> IO (Ptr <struct git_oid>)
#ccall git_rebase_onto_name , Ptr <struct git_rebase> -> IO CString
#ccall git_rebase_onto_id , Ptr <struct git_rebase> -> IO (Ptr <struct git_oid>)
#ccall git_rebase_operation_entrycount , Ptr <struct git_rebase> -> IO CSize
#ccall git_rebase_operation_current , Ptr <struct git_rebase> -> IO CSize
#ccall git_rebase_operation_byindex , Ptr <struct git_rebase> -> CSize -> IO (Ptr <git_rebase_operation>)
#ccall git_rebase_next , Ptr (Ptr <git_rebase_operation>) -> Ptr <struct git_rebase> -> IO CInt
#ccall git_rebase_inmemory_index , Ptr (Ptr <struct git_index>) -> Ptr <struct git_rebase> -> IO CInt
#ccall git_rebase_commit , Ptr <struct git_oid> -> Ptr <struct git_rebase> -> Ptr <struct git_signature> -> Ptr <struct git_signature> -> CString -> CString -> IO CInt
#ccall git_rebase_abort , Ptr <struct git_rebase> -> IO CInt
#ccall git_rebase_finish , Ptr <struct git_rebase> -> Ptr <struct git_signature> -> IO CInt
#ccall git_rebase_free , Ptr <struct git_rebase> -> IO ()
