{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/oidarray.h>
module Bindings.Libgit2.Oidarray where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
{- typedef struct git_oidarray {
            git_oid * ids; size_t count;
        } git_oidarray; -}
#starttype struct git_oidarray
#field ids , Ptr <struct git_oid>
#field count , CSize
#stoptype
#ccall git_oidarray_dispose , Ptr <struct git_oidarray> -> IO ()
