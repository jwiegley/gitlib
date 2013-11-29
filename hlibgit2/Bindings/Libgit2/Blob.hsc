{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Blob where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
#cinline git_blob_lookup , Ptr (Ptr <git_blob>) -> Ptr <git_repository> -> Ptr <git_oid> -> IO (CInt)
#cinline git_blob_lookup_prefix , Ptr (Ptr <git_blob>) -> Ptr <git_repository> -> Ptr <git_oid> -> CSize -> IO (CInt)
#cinline git_blob_free , Ptr <git_blob> -> IO ()
#cinline git_blob_id , Ptr <git_blob> -> IO (Ptr <git_oid>)
#ccall git_blob_rawcontent , Ptr <git_blob> -> IO (Ptr ())
#ccall git_blob_rawsize , Ptr <git_blob> -> IO (CLong)
#ccall git_blob_create_fromworkdir , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_blob_create_fromdisk , Ptr <git_oid> -> Ptr <git_repository> -> CString -> IO (CInt)
{- typedef int (* git_blob_chunk_cb)(char * content,
                                  size_t max_length,
                                  void * payload); -}
#callback git_blob_chunk_cb , CString -> CSize -> Ptr () -> IO CInt
#ccall git_blob_create_fromchunks , Ptr <git_oid> -> Ptr <git_repository> -> CString -> <git_blob_chunk_cb> -> Ptr () -> IO (CInt)
#ccall git_blob_create_frombuffer , Ptr <git_oid> -> Ptr <git_repository> -> Ptr () -> CSize -> IO (CInt)
#ccall git_blob_is_binary , Ptr <git_blob> -> IO (CInt)
