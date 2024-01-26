{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/reflog.h>
module Bindings.Libgit2.Reflog where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_reflog_read , Ptr (Ptr <struct git_reflog>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reflog_write , Ptr <struct git_reflog> -> IO CInt
#ccall git_reflog_append , Ptr <struct git_reflog> -> Ptr <struct git_oid> -> Ptr <struct git_signature> -> CString -> IO CInt
#ccall git_reflog_rename , Ptr <struct git_repository> -> CString -> CString -> IO CInt
#ccall git_reflog_delete , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_reflog_entrycount , Ptr <struct git_reflog> -> IO CSize
#ccall git_reflog_entry_byindex , Ptr <struct git_reflog> -> CSize -> IO (Ptr <struct git_reflog_entry>)
#ccall git_reflog_drop , Ptr <struct git_reflog> -> CSize -> CInt -> IO CInt
#ccall git_reflog_entry_id_old , Ptr <struct git_reflog_entry> -> IO (Ptr <struct git_oid>)
#ccall git_reflog_entry_id_new , Ptr <struct git_reflog_entry> -> IO (Ptr <struct git_oid>)
#ccall git_reflog_entry_committer , Ptr <struct git_reflog_entry> -> IO (Ptr <struct git_signature>)
#ccall git_reflog_entry_message , Ptr <struct git_reflog_entry> -> IO CString
#ccall git_reflog_free , Ptr <struct git_reflog> -> IO ()
