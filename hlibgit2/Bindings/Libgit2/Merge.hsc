{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Merge where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_merge_base , Ptr <git_oid> -> Ptr <git_repository> -> Ptr <git_oid> -> Ptr <git_oid> -> IO (CInt)
#ccall git_merge_base_many , Ptr <git_oid> -> Ptr <git_repository> -> Ptr (<git_oid>) -> CSize -> IO (CInt)
