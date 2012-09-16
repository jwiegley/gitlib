#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Threads where
#strict_import

import Bindings.Libgit2.Common
#ccall git_threads_init , IO ()
#ccall git_threads_shutdown , IO ()
