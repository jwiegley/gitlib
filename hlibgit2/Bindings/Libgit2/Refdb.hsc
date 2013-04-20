{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/refdb.h>
module Bindings.Libgit2.Refdb where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Refs
import Bindings.Libgit2.RefdbBackend
#ccall git_reference__alloc , Ptr <git_refdb> -> CString -> Ptr <git_oid> -> CString -> IO (Ptr <git_reference>)
#ccall git_refdb_new , Ptr (Ptr <git_refdb>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_refdb_open , Ptr (Ptr <git_refdb>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_refdb_compress , Ptr <git_refdb> -> IO (CInt)
#ccall git_refdb_free , Ptr <git_refdb> -> IO ()
#ccall git_refdb_set_backend , Ptr <git_refdb> -> Ptr <git_refdb_backend> -> IO (CInt)
