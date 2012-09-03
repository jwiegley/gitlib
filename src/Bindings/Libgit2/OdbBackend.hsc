
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.OdbBackend where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef enum {
	GIT_STREAM_RDONLY = (1 << 1),
	GIT_STREAM_WRONLY = (1 << 2),
	GIT_STREAM_RW = (GIT_STREAM_RDONLY | GIT_STREAM_WRONLY),
} git_odb_streammode; -}
#num    GIT_STREAM_RDONLY
#num    GIT_STREAM_WRONLY
#num    GIT_STREAM_RW
#ccall git_odb_backend_pack , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
#ccall git_odb_backend_loose , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
