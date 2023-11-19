{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/global.h>
module Bindings.Libgit2.Global where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
#ccall git_libgit2_init , IO CInt
#ccall git_libgit2_shutdown , IO CInt
