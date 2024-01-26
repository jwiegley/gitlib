{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/refdb.h>
module Bindings.Libgit2.Refdb where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Refs
#ccall git_refdb_new , Ptr (Ptr <struct git_refdb>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_refdb_open , Ptr (Ptr <struct git_refdb>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_refdb_compress , Ptr <struct git_refdb> -> IO CInt
#ccall git_refdb_free , Ptr <struct git_refdb> -> IO ()
