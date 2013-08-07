{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.OdbBackend where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Indexer
{- struct git_odb_stream; -}
{-  #opaque_t git_odb_stream -}
{- struct git_odb_writepack; -}
{-  #opaque_t git_odb_writepack -}
{- typedef int (* git_odb_foreach_cb)(const git_oid * id,
                                   void * payload); -}
#callback git_odb_foreach_cb , Ptr (<git_oid>) -> Ptr () -> IO CInt
{- struct git_odb_backend {
    unsigned int version;
    git_odb * odb;
    int (* read)(void * *,
                 size_t *,
                 git_otype *,
                 struct git_odb_backend *,
                 const git_oid *);
    int (* read_prefix)(git_oid *,
                        void * *,
                        size_t *,
                        git_otype *,
                        struct git_odb_backend *,
                        const git_oid *,
                        size_t);
    int (* read_header)(size_t *,
                        git_otype *,
                        struct git_odb_backend *,
                        const git_oid *);
    int (* write)(git_oid *,
                  struct git_odb_backend *,
                  const void *,
                  size_t,
                  git_otype);
    int (* writestream)(struct git_odb_stream * *,
                        struct git_odb_backend *,
                        size_t,
                        git_otype);
    int (* readstream)(struct git_odb_stream * *,
                       struct git_odb_backend *,
                       const git_oid *);
    int (* exists)(struct git_odb_backend *, const git_oid *, bool);
    int (* refresh)(struct git_odb_backend *);
    int (* foreach)(struct git_odb_backend *,
                    git_odb_foreach_cb cb,
                    void * payload);
    int (* writepack)(struct git_odb_writepack * *,
                      struct git_odb_backend *,
                      git_transfer_progress_callback progress_cb,
                      void * progress_payload);
    void (* free)(struct git_odb_backend *);
}; -}
#callback git_odb_backend_read_callback , Ptr (Ptr ()) -> Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_read_prefix_callback , Ptr <git_oid> -> Ptr (Ptr ()) -> Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> CSize -> IO CInt
#callback git_odb_backend_read_header_callback , Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_write_callback , Ptr <git_oid> -> Ptr <git_odb_backend> -> Ptr () -> CSize -> <git_otype> -> IO CInt
#callback git_odb_backend_writestream_callback , Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> CSize -> <git_otype> -> IO CInt
#callback git_odb_backend_readstream_callback , Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_exists_callback , Ptr <git_odb_backend> -> Ptr <git_oid> -> CInt -> IO CInt
#callback git_odb_backend_refresh_callback , Ptr <git_odb_backend> -> IO CInt
#callback git_odb_backend_foreach_callback , Ptr <git_odb_backend> -> <git_odb_foreach_cb> -> Ptr () -> IO CInt
#callback git_odb_backend_writepack_callback , Ptr (Ptr <git_odb_writepack>) -> Ptr <git_odb_backend> -> <git_transfer_progress_callback> -> Ptr () -> IO CInt
#callback git_odb_backend_free_callback , Ptr <git_odb_backend> -> IO ()
#starttype git_odb_backend
#field version , CUInt
#field odb , Ptr <git_odb>
#field read , <git_odb_backend_read_callback>
#field read_prefix , <git_odb_backend_read_prefix_callback>
#field read_header , <git_odb_backend_read_header_callback>
#field write , <git_odb_backend_write_callback>
#field writestream , <git_odb_backend_writestream_callback>
#field readstream , <git_odb_backend_readstream_callback>
#field exists , <git_odb_backend_exists_callback>
#field refresh , <git_odb_backend_refresh_callback>
#field foreach , <git_odb_backend_foreach_callback>
#field writepack , <git_odb_backend_writepack_callback>
#field free , <git_odb_backend_free_callback>
#stoptype
{- enum {
    GIT_STREAM_RDONLY = 1 << 1,
    GIT_STREAM_WRONLY = 1 << 2,
    GIT_STREAM_RW = GIT_STREAM_RDONLY | GIT_STREAM_WRONLY
}; -}
#num GIT_STREAM_RDONLY
#num GIT_STREAM_WRONLY
#num GIT_STREAM_RW
{- struct git_odb_stream {
    struct git_odb_backend * backend;
    unsigned int mode;
    int (* read)(struct git_odb_stream * stream,
                 char * buffer,
                 size_t len);
    int (* write)(struct git_odb_stream * stream,
                  const char * buffer,
                  size_t len);
    int (* finalize_write)(git_oid * oid_p,
                           struct git_odb_stream * stream);
    void (* free)(struct git_odb_stream * stream);
}; -}
#callback git_odb_stream_read_callback , Ptr <git_odb_stream> -> CString -> CSize -> IO CInt
#callback git_odb_stream_write_callback , Ptr <git_odb_stream> -> CString -> CSize -> IO CInt
#callback git_odb_stream_finalize_write_callback , Ptr <git_oid> -> Ptr <git_odb_stream> -> IO CInt
#callback git_odb_stream_free_callback , Ptr <git_odb_stream> -> IO ()
#starttype git_odb_stream
#field backend , Ptr <git_odb_backend>
#field mode , CUInt
#field read , <git_odb_stream_read_callback>
#field write , <git_odb_stream_write_callback>
#field finalize_write , <git_odb_stream_finalize_write_callback>
#field free , <git_odb_stream_free_callback>
#stoptype
{- struct git_odb_writepack {
    struct git_odb_backend * backend;
    int (* add)(struct git_odb_writepack * writepack,
                const void * data,
                size_t size,
                git_transfer_progress * stats);
    int (* commit)(struct git_odb_writepack * writepack,
                   git_transfer_progress * stats);
    void (* free)(struct git_odb_writepack * writepack);
}; -}
#callback git_odb_writepack_add_callback , Ptr <git_odb_writepack> -> Ptr () -> CSize -> Ptr <git_transfer_progress> -> IO CInt
#callback git_odb_writepack_commit_callback , Ptr <git_odb_writepack> -> Ptr <git_transfer_progress> -> IO CInt
#callback git_odb_writepack_free_callback , Ptr <git_odb_writepack> -> IO ()
#starttype git_odb_writepack
#field backend , Ptr <git_odb_backend>
#field add , <git_odb_writepack_add_callback>
#field commit , <git_odb_writepack_commit_callback>
#field free , <git_odb_writepack_free_callback>
#stoptype
#ccall git_odb_backend_malloc , Ptr <git_odb_backend> -> CSize -> IO (Ptr ())
#ccall git_odb_backend_pack , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
#ccall git_odb_backend_loose , Ptr (Ptr <git_odb_backend>) -> CString -> CInt -> CInt -> IO (CInt)
#ccall git_odb_backend_one_pack , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
