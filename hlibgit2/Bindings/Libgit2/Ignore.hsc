{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/ignore.h>
module Bindings.Libgit2.Ignore where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_ignore_add_rule , Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_ignore_clear_internal_rules , Ptr <struct git_repository> -> IO CInt
#ccall git_ignore_path_is_ignored , Ptr CInt -> Ptr <struct git_repository> -> CString -> IO CInt
