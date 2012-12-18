#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Windows where
#strict_import

import Bindings.Libgit2.Common
#ifdef GIT_WIN32
#ccall gitwin_set_codepage , CUInt -> IO ()
#ccall gitwin_get_codepage , IO (CUInt)
#ccall gitwin_set_utf8 , IO ()
#endif