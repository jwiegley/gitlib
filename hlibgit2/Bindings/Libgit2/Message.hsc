{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/message.h>
module Bindings.Libgit2.Message where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
#ccall git_message_prettify , CString -> CSize -> CString -> CInt -> IO (CInt)
