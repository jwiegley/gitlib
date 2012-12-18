#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Blob where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
#cinline git_blob_lookup , Ptr (Ptr <git_blob>) -> Ptr <git_repository> -> Ptr <git_oid> -> IO (CInt)
#cinline git_blob_lookup_prefix , Ptr (Ptr <git_blob>) -> Ptr <git_repository> -> Ptr <git_oid> -> CUInt -> IO (CInt)
#cinline git_blob_free , Ptr <git_blob> -> IO ()
#ccall git_blob_rawcontent , Ptr <git_blob> -> IO (Ptr ())
#ccall git_blob_rawsize , Ptr <git_blob> -> IO (CSize)
#ccall git_blob_create_fromfile , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_blob_create_fromdisk , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_blob_create_frombuffer , Ptr <git_oid> -> Ptr <git_repository> -> Ptr () -> CSize -> IO (CInt)
