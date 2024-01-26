{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/odb.h>
module Bindings.Libgit2.Odb where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Oidarray
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.OdbBackend
{- typedef enum {
            GIT_ODB_LOOKUP_NO_REFRESH = 1 << 0
        } git_odb_lookup_flags_t; -}
#integral_t git_odb_lookup_flags_t
#num GIT_ODB_LOOKUP_NO_REFRESH
#callback git_odb_foreach_cb , Ptr <struct git_oid> -> Ptr () -> IO CInt
{- typedef struct {
            unsigned int version; git_oid_t oid_type;
        } git_odb_options; -}
#starttype git_odb_options
#field version , CUInt
#field oid_type , <git_oid_t>
#stoptype
#ccall git_odb_new , Ptr (Ptr <struct git_odb>) -> IO CInt
#ccall git_odb_open , Ptr (Ptr <struct git_odb>) -> CString -> IO CInt
#ccall git_odb_add_disk_alternate , Ptr <struct git_odb> -> CString -> IO CInt
#ccall git_odb_free , Ptr <struct git_odb> -> IO ()
#ccall git_odb_read , Ptr (Ptr <struct git_odb_object>) -> Ptr <struct git_odb> -> Ptr <struct git_oid> -> IO CInt
#ccall git_odb_read_prefix , Ptr (Ptr <struct git_odb_object>) -> Ptr <struct git_odb> -> Ptr <struct git_oid> -> CSize -> IO CInt
#ccall git_odb_read_header , Ptr CSize -> Ptr <git_object_t> -> Ptr <struct git_odb> -> Ptr <struct git_oid> -> IO CInt
#ccall git_odb_exists , Ptr <struct git_odb> -> Ptr <struct git_oid> -> IO CInt
#ccall git_odb_exists_ext , Ptr <struct git_odb> -> Ptr <struct git_oid> -> CUInt -> IO CInt
#ccall git_odb_exists_prefix , Ptr <struct git_oid> -> Ptr <struct git_odb> -> Ptr <struct git_oid> -> CSize -> IO CInt
{- typedef struct git_odb_expand_id {
            git_oid id; unsigned short length; git_object_t type;
        } git_odb_expand_id; -}
#starttype struct git_odb_expand_id
#field id , <struct git_oid>
#field length , CUShort
#field type , <git_object_t>
#stoptype
#ccall git_odb_expand_ids , Ptr <struct git_odb> -> Ptr <struct git_odb_expand_id> -> CSize -> IO CInt
#ccall git_odb_refresh , Ptr <struct git_odb> -> IO CInt
#ccall git_odb_foreach , Ptr <struct git_odb> -> <git_odb_foreach_cb> -> Ptr () -> IO CInt
#ccall git_odb_write , Ptr <struct git_oid> -> Ptr <struct git_odb> -> Ptr () -> CSize -> <git_object_t> -> IO CInt
#ccall git_odb_open_wstream , Ptr (Ptr <struct git_odb_stream>) -> Ptr <struct git_odb> -> CULong -> <git_object_t> -> IO CInt
#ccall git_odb_stream_write , Ptr <struct git_odb_stream> -> CString -> CSize -> IO CInt
#ccall git_odb_stream_finalize_write , Ptr <struct git_oid> -> Ptr <struct git_odb_stream> -> IO CInt
#ccall git_odb_stream_read , Ptr <struct git_odb_stream> -> CString -> CSize -> IO CInt
#ccall git_odb_stream_free , Ptr <struct git_odb_stream> -> IO ()
#ccall git_odb_open_rstream , Ptr (Ptr <struct git_odb_stream>) -> Ptr CSize -> Ptr <git_object_t> -> Ptr <struct git_odb> -> Ptr <struct git_oid> -> IO CInt
#ccall git_odb_write_pack , Ptr (Ptr <struct git_odb_writepack>) -> Ptr <struct git_odb> -> <git_indexer_progress_cb> -> Ptr () -> IO CInt
#ccall git_odb_write_multi_pack_index , Ptr <struct git_odb> -> IO CInt
#ccall git_odb_hash , Ptr <struct git_oid> -> Ptr () -> CSize -> <git_object_t> -> IO CInt
#ccall git_odb_hashfile , Ptr <struct git_oid> -> CString -> <git_object_t> -> IO CInt
#ccall git_odb_object_dup , Ptr (Ptr <struct git_odb_object>) -> Ptr <struct git_odb_object> -> IO CInt
#ccall git_odb_object_free , Ptr <struct git_odb_object> -> IO ()
#ccall git_odb_object_id , Ptr <struct git_odb_object> -> IO (Ptr <struct git_oid>)
#ccall git_odb_object_data , Ptr <struct git_odb_object> -> IO (Ptr ())
#ccall git_odb_object_size , Ptr <struct git_odb_object> -> IO CSize
#ccall git_odb_object_type , Ptr <struct git_odb_object> -> IO <git_object_t>
#ccall git_odb_add_backend , Ptr <struct git_odb> -> Ptr <struct git_odb_backend> -> CInt -> IO CInt
#ccall git_odb_add_alternate , Ptr <struct git_odb> -> Ptr <struct git_odb_backend> -> CInt -> IO CInt
#ccall git_odb_num_backends , Ptr <struct git_odb> -> IO CSize
#ccall git_odb_get_backend , Ptr (Ptr <struct git_odb_backend>) -> Ptr <struct git_odb> -> CSize -> IO CInt
#ccall git_odb_set_commit_graph , Ptr <struct git_odb> -> Ptr <struct git_commit_graph> -> IO CInt
