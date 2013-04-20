{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/stash.h>
module Bindings.Libgit2.Stash where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef enum {
            GIT_STASH_DEFAULT = 0,
            GIT_STASH_KEEP_INDEX = 1 << 0,
            GIT_STASH_INCLUDE_UNTRACKED = 1 << 1,
            GIT_STASH_INCLUDE_IGNORED = 1 << 2
        } git_stash_flags; -}
#integral_t git_stash_flags
#num GIT_STASH_DEFAULT
#num GIT_STASH_KEEP_INDEX
#num GIT_STASH_INCLUDE_UNTRACKED
#num GIT_STASH_INCLUDE_IGNORED
#ccall git_stash_save , Ptr <git_oid> -> Ptr <git_repository> -> Ptr <git_signature> -> CString -> CUInt -> IO (CInt)
{- typedef int (* git_stash_cb)(size_t index,
                             const char * message,
                             const git_oid * stash_id,
                             void * payload); -}
#callback git_stash_cb , CSize -> CString -> Ptr (<git_oid>) -> Ptr () -> IO CInt
#ccall git_stash_foreach , Ptr <git_repository> -> <git_stash_cb> -> Ptr () -> IO (CInt)
#ccall git_stash_drop , Ptr <git_repository> -> CSize -> IO (CInt)
