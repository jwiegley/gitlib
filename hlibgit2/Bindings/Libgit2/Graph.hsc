{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/graph.h>
module Bindings.Libgit2.Graph where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_graph_ahead_behind , Ptr CSize -> Ptr CSize -> Ptr <git_repository> -> Ptr <git_oid> -> Ptr <git_oid> -> IO (CInt)
