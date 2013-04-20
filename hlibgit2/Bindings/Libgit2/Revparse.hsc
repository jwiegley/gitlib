{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/revparse.h>
module Bindings.Libgit2.Revparse where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_revparse_single , Ptr (Ptr <git_object>) -> Ptr <git_repository> -> CString -> IO (CInt)
{- typedef enum {
            GIT_REVPARSE_SINGLE = 1 << 0,
            GIT_REVPARSE_RANGE = 1 << 1,
            GIT_REVPARSE_MERGE_BASE = 1 << 2
        } git_revparse_mode_t; -}
#integral_t git_revparse_mode_t
#num GIT_REVPARSE_SINGLE
#num GIT_REVPARSE_RANGE
#num GIT_REVPARSE_MERGE_BASE
{- typedef struct {
            git_object * from; git_object * to; unsigned int flags;
        } git_revspec; -}
#starttype git_revspec
#field from , Ptr <git_object>
#field to , Ptr <git_object>
#field flags , CUInt
#stoptype
#ccall git_revparse , Ptr <git_revspec> -> Ptr <git_repository> -> CString -> IO (CInt)
