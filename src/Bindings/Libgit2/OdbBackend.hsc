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
#starttype git_odb_backend
#field odb , Ptr <git_odb>
#field read , FunPtr (Ptr (Ptr ()) -> Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> CInt)
#field read_prefix , FunPtr (Ptr <git_oid> -> Ptr (Ptr ()) -> Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> CUInt -> CInt)
#field read_header , FunPtr (Ptr CSize -> Ptr <git_otype> -> Ptr <git_odb_backend> -> Ptr <git_oid> -> CInt)
#field write , FunPtr (Ptr <git_oid> -> Ptr <git_odb_backend> -> Ptr () -> CSize -> <git_otype> -> CInt)
#field writestream , FunPtr (Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> CSize -> <git_otype> -> CInt)
#field readstream , FunPtr (Ptr (Ptr <git_odb_stream>) -> Ptr <git_odb_backend> -> Ptr <git_oid> -> CInt)
#field exists , FunPtr (Ptr <git_odb_backend> -> Ptr <git_oid> -> CInt)
#field free , FunPtr (Ptr <git_odb_backend>)
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
#starttype git_odb_stream
#field backend , Ptr <git_odb_backend>
#field mode , CInt
#field read , FunPtr (Ptr <git_odb_stream> -> CString -> CSize -> CInt)
#field write , FunPtr (Ptr <git_odb_stream> -> CString -> CSize -> CInt)
#field finalize_write , FunPtr (Ptr <git_oid> -> Ptr <git_odb_stream> -> CInt)
#field free , FunPtr (Ptr <git_odb_stream>)
#stoptype
#ccall git_odb_backend_pack , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
#ccall git_odb_backend_loose , Ptr (Ptr <git_odb_backend>) -> CString -> CInt -> CInt -> IO (CInt)
