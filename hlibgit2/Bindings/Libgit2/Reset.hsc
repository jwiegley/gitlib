{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/reset.h>
module Bindings.Libgit2.Reset where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
{- typedef enum {
            GIT_RESET_SOFT = 1, GIT_RESET_MIXED = 2, GIT_RESET_HARD = 3
        } git_reset_t; -}
#integral_t git_reset_t
#num GIT_RESET_SOFT
#num GIT_RESET_MIXED
#num GIT_RESET_HARD
#ccall git_reset , Ptr <git_repository> -> Ptr <git_object> -> <git_reset_t> -> IO (CInt)
#ccall git_reset_default , Ptr <git_repository> -> Ptr <git_object> -> Ptr <git_strarray> -> IO (CInt)
