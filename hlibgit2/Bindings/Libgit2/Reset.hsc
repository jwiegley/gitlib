{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/reset.h>
module Bindings.Libgit2.Reset where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Checkout
{- typedef enum {
            GIT_RESET_SOFT = 1, GIT_RESET_MIXED = 2, GIT_RESET_HARD = 3
        } git_reset_t; -}
#integral_t git_reset_t
#num GIT_RESET_SOFT
#num GIT_RESET_MIXED
#num GIT_RESET_HARD
#ccall git_reset , Ptr <struct git_repository> -> Ptr <struct git_object> -> <git_reset_t> -> Ptr <struct git_checkout_options> -> IO CInt
#ccall git_reset_from_annotated , Ptr <struct git_repository> -> Ptr <struct git_annotated_commit> -> <git_reset_t> -> Ptr <struct git_checkout_options> -> IO CInt
#ccall git_reset_default , Ptr <struct git_repository> -> Ptr <struct git_object> -> Ptr <struct git_strarray> -> IO CInt
