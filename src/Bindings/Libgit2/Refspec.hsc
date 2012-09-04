#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Refspec where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_refspec_src , Ptr <git_refspec> -> IO (CString)
#ccall git_refspec_dst , Ptr <git_refspec> -> IO (CString)
#ccall git_refspec_src_matches , Ptr <git_refspec> -> CString -> IO (CInt)
#ccall git_refspec_transform , CString -> CLong -> Ptr <git_refspec> -> CString -> IO (CInt)
