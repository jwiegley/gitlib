{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/revert.h>
module Bindings.Libgit2.Revert where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Merge
import Bindings.Libgit2.Checkout
{- typedef struct {
            unsigned int version;
            unsigned int mainline;
            git_merge_options merge_opts;
            git_checkout_options checkout_opts;
        } git_revert_options; -}
#starttype git_revert_options
#field version , CUInt
#field mainline , CUInt
#field merge_opts , <git_merge_options>
#field checkout_opts , <struct git_checkout_options>
#stoptype
#ccall git_revert_options_init , Ptr <git_revert_options> -> CUInt -> IO CInt
#ccall git_revert_commit , Ptr (Ptr <struct git_index>) -> Ptr <struct git_repository> -> Ptr <struct git_commit> -> Ptr <struct git_commit> -> CUInt -> Ptr <git_merge_options> -> IO CInt
#ccall git_revert , Ptr <struct git_repository> -> Ptr <struct git_commit> -> Ptr <git_revert_options> -> IO CInt
