{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/buffer.h>
module Bindings.Libgit2.Buffer where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
{- typedef struct {
            char * ptr; size_t reserved; size_t size;
        } git_buf; -}
#starttype git_buf
#field ptr , CString
#field reserved , CSize
#field size , CSize
#stoptype
#ccall git_buf_dispose , Ptr <git_buf> -> IO ()
