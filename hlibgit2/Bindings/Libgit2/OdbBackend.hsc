#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.OdbBackend where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- struct git_odb_backend {
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
                        unsigned int);
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
    int (* exists)(struct git_odb_backend *, const git_oid *);
    void (* free)(struct git_odb_backend *);
}; -}
#callback git_odb_backend_read_callback , Ptr (Ptr ()) -> Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_read_prefix_callback , Ptr <git_oid> -> Ptr (Ptr ()) -> Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> CUInt -> IO CInt
#callback git_odb_backend_read_header_callback , Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_write_callback , Ptr <git_oid> -> Ptr <git_odb_backend> -> Ptr () -> CSize -> <git_otype> -> IO CInt
#callback git_odb_backend_writestream_callback , Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> CSize -> <git_otype> -> IO CInt
#callback git_odb_backend_readstream_callback , Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_exists_callback , Ptr <git_odb_backend> -> Ptr <git_oid> -> IO CInt
#callback git_odb_backend_free_callback , Ptr <git_odb_backend> -> IO ()
#starttype git_odb_backend
#field odb , Ptr <git_odb>
#field read , <git_odb_backend_read_callback>
#field read_prefix , <git_odb_backend_read_prefix_callback>
#field read_header , <git_odb_backend_read_header_callback>
#field write , <git_odb_backend_write_callback>
#field writestream , <git_odb_backend_writestream_callback>
#field readstream , <git_odb_backend_readstream_callback>
#field exists , <git_odb_backend_exists_callback>
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
    int mode;
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
#field mode , CInt
#field read , <git_odb_stream_read_callback>
#field write , <git_odb_stream_write_callback>
#field finalize_write , <git_odb_stream_finalize_write_callback>
#field free , <git_odb_stream_free_callback>
#stoptype
#ccall git_odb_backend_pack , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
#ccall git_odb_backend_loose , Ptr (Ptr <git_odb_backend>) -> CString -> CInt -> CInt -> IO (CInt)
