{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/strarray.h>
module Bindings.Libgit2.Strarray where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
{- typedef struct git_strarray {
            char * * strings; size_t count;
        } git_strarray; -}
#starttype git_strarray
#field strings , Ptr (CString)
#field count , CSize
#stoptype
#ccall git_strarray_free , Ptr <git_strarray> -> IO ()
#ccall git_strarray_copy , Ptr <git_strarray> -> Ptr <git_strarray> -> IO (CInt)
