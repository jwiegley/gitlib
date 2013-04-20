{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Odb where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.OdbBackend
import Bindings.Libgit2.Indexer
#ccall git_odb_new , Ptr (Ptr <git_odb>) -> IO (CInt)
#ccall git_odb_open , Ptr (Ptr <git_odb>) -> CString -> IO (CInt)
#ccall git_odb_add_backend , Ptr <git_odb> -> Ptr <git_odb_backend> -> CInt -> IO (CInt)
#ccall git_odb_add_alternate , Ptr <git_odb> -> Ptr <git_odb_backend> -> CInt -> IO (CInt)
#ccall git_odb_add_disk_alternate , Ptr <git_odb> -> CString -> IO (CInt)
#ccall git_odb_free , Ptr <git_odb> -> IO ()
#ccall git_odb_read , Ptr (Ptr <git_odb_object>) -> Ptr <git_odb> -> Ptr <git_oid> -> IO (CInt)
#ccall git_odb_read_prefix , Ptr (Ptr <git_odb_object>) -> Ptr <git_odb> -> Ptr <git_oid> -> CSize -> IO (CInt)
#ccall git_odb_read_header , Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb> -> Ptr <git_oid> -> IO (CInt)
#ccall git_odb_exists , Ptr <git_odb> -> Ptr <git_oid> -> IO (CInt)
#ccall git_odb_refresh , Ptr <git_odb> -> IO (CInt)
#ccall git_odb_foreach , Ptr <git_odb> -> <git_odb_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_odb_write , Ptr <git_oid> -> Ptr <git_odb> -> Ptr () -> CSize -> <git_otype> -> IO (CInt)
#ccall git_odb_open_wstream , Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb> -> CSize -> <git_otype> -> IO (CInt)
#ccall git_odb_open_rstream , Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb> -> Ptr <git_oid> -> IO (CInt)
#ccall git_odb_write_pack , Ptr (Ptr <git_odb_writepack>) -> Ptr <git_odb> -> <git_transfer_progress_callback> -> Ptr () -> IO (CInt)
#ccall git_odb_hash , Ptr <git_oid> -> Ptr () -> CSize -> <git_otype> -> IO (CInt)
#ccall git_odb_hashfile , Ptr <git_oid> -> CString -> <git_otype> -> IO (CInt)
#ccall git_odb_object_free , Ptr <git_odb_object> -> IO ()
#ccall git_odb_object_id , Ptr <git_odb_object> -> IO (Ptr <git_oid>)
#ccall git_odb_object_data , Ptr <git_odb_object> -> IO (Ptr ())
#ccall git_odb_object_size , Ptr <git_odb_object> -> IO (CSize)
#ccall git_odb_object_type , Ptr <git_odb_object> -> IO (<git_otype>)
