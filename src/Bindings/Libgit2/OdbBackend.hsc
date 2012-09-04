#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.OdbBackend where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_odb_backend_pack , Ptr (Ptr <git_odb_backend>) -> CString -> IO (CInt)
#ccall git_odb_backend_loose , Ptr (Ptr <git_odb_backend>) -> CString -> CInt -> CInt -> IO (CInt)
