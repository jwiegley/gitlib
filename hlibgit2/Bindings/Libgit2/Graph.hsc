{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/graph.h>
module Bindings.Libgit2.Graph where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_graph_ahead_behind , Ptr CSize -> Ptr CSize -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_graph_descendant_of , Ptr <struct git_repository> -> Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_graph_reachable_from_any , Ptr <struct git_repository> -> Ptr <struct git_oid> -> Ptr (<struct git_oid>) -> CSize -> IO CInt
