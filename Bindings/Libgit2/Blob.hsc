
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Blob where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
#ccall git_blob_rawcontent , Ptr <git_blob> -> IO (Ptr CChar)
#ccall git_blob_rawsize , Ptr <git_blob> -> IO (CInt)
#ccall git_blob_create_fromfile , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_blob_create_frombuffer , Ptr <git_oid> -> Ptr <git_repository> -> Ptr CChar -> CSize -> IO (CInt)
#cinline git_blob_lookup , Ptr (Ptr <git_blob>) -> Ptr <git_repository> -> Ptr <git_oid> -> IO (CInt)
#cinline git_blob_lookup_prefix , Ptr (Ptr <git_blob>) -> Ptr <git_repository> -> Ptr <git_oid> -> CUInt -> IO (CInt)
#cinline git_blob_close , Ptr <git_blob> -> IO ()
