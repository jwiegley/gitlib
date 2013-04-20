{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/refdb_backend.h>
module Bindings.Libgit2.RefdbBackend where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Refs
import Bindings.Libgit2.Oid
{- struct git_refdb_backend {
    unsigned int version;
    int (* exists)(int * exists,
                   struct git_refdb_backend * backend,
                   const char * ref_name);
    int (* lookup)(git_reference * * out,
                   struct git_refdb_backend * backend,
                   const char * ref_name);
    int (* foreach)(struct git_refdb_backend * backend,
                    unsigned int list_flags,
                    git_reference_foreach_cb callback,
                    void * payload);
    int (* foreach_glob)(struct git_refdb_backend * backend,
                         const char * glob,
                         unsigned int list_flags,
                         git_reference_foreach_cb callback,
                         void * payload);
    int (* write)(struct git_refdb_backend * backend,
                  const git_reference * ref);
    int (* delete)(struct git_refdb_backend * backend,
                   const git_reference * ref);
    int (* compress)(struct git_refdb_backend * backend);
    void (* free)(struct git_refdb_backend * backend);
}; -}
#callback git_refdb_backend_exists_callback , Ptr CInt -> Ptr <git_refdb_backend> -> CString -> IO CInt
#callback git_refdb_backend_lookup_callback , Ptr (Ptr <git_reference>) -> Ptr <git_refdb_backend> -> CString -> IO CInt
#callback git_refdb_backend_foreach_callback , Ptr <git_refdb_backend> -> CUInt -> <git_reference_foreach_cb> -> Ptr () -> IO CInt
#callback git_refdb_backend_foreach_glob_callback , Ptr <git_refdb_backend> -> CString -> CUInt -> <git_reference_foreach_cb> -> Ptr () -> IO CInt
#callback git_refdb_backend_write_callback , Ptr <git_refdb_backend> -> Ptr <git_reference> -> IO CInt
#callback git_refdb_backend_delete_callback , Ptr <git_refdb_backend> -> Ptr <git_reference> -> IO CInt
#callback git_refdb_backend_compress_callback , Ptr <git_refdb_backend> -> IO CInt
#callback git_refdb_backend_free_callback , Ptr <git_refdb_backend> -> IO ()
#starttype git_refdb_backend
#field version , CUInt
#field exists , <git_refdb_backend_exists_callback>
#field lookup , <git_refdb_backend_lookup_callback>
#field foreach , <git_refdb_backend_foreach_callback>
#field foreach_glob , <git_refdb_backend_foreach_glob_callback>
#field write , <git_refdb_backend_write_callback>
#field delete , <git_refdb_backend_delete_callback>
#field compress , <git_refdb_backend_compress_callback>
#field free , <git_refdb_backend_free_callback>
#stoptype
#ccall git_refdb_backend_fs , Ptr (Ptr <git_refdb_backend>) -> Ptr <git_repository> -> Ptr <git_refdb> -> IO (CInt)
