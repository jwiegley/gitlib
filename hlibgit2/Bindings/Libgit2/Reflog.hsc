{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Reflog where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_reflog_read , Ptr (Ptr <git_reflog>) -> Ptr <git_reference> -> IO (CInt)
#ccall git_reflog_write , Ptr <git_reflog> -> IO (CInt)
#ccall git_reflog_append , Ptr <git_reflog> -> Ptr <git_oid> -> Ptr <git_signature> -> CString -> IO (CInt)
#ccall git_reflog_rename , Ptr <git_reference> -> CString -> IO (CInt)
#ccall git_reflog_delete , Ptr <git_reference> -> IO (CInt)
#ccall git_reflog_entrycount , Ptr <git_reflog> -> IO (CSize)
#ccall git_reflog_entry_byindex , Ptr <git_reflog> -> CSize -> IO (Ptr <git_reflog_entry>)
#ccall git_reflog_drop , Ptr <git_reflog> -> CSize -> CInt -> IO (CInt)
#ccall git_reflog_entry_id_old , Ptr <git_reflog_entry> -> IO (Ptr <git_oid>)
#ccall git_reflog_entry_id_new , Ptr <git_reflog_entry> -> IO (Ptr <git_oid>)
#ccall git_reflog_entry_committer , Ptr <git_reflog_entry> -> IO (Ptr <git_signature>)
#ccall git_reflog_entry_message , Ptr <git_reflog_entry> -> IO (CString)
#ccall git_reflog_free , Ptr <git_reflog> -> IO ()
