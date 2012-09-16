#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Merge where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_merge_base , Ptr <git_oid> -> Ptr <git_repository> -> Ptr <git_oid> -> Ptr <git_oid> -> IO (CInt)
