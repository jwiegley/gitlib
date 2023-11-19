{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/revparse.h>
module Bindings.Libgit2.Revparse where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_revparse_single , Ptr (Ptr <struct git_object>) -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_revparse_ext , Ptr (Ptr <struct git_object>) -> Ptr (Ptr <struct git_reference>) -> Ptr <struct git_repository> -> CString -> IO CInt
{- typedef enum {
            GIT_REVSPEC_SINGLE = 1 << 0,
            GIT_REVSPEC_RANGE = 1 << 1,
            GIT_REVSPEC_MERGE_BASE = 1 << 2
        } git_revspec_t; -}
#integral_t git_revspec_t
#num GIT_REVSPEC_SINGLE
#num GIT_REVSPEC_RANGE
#num GIT_REVSPEC_MERGE_BASE
{- typedef struct {
            git_object * from; git_object * to; unsigned int flags;
        } git_revspec; -}
#starttype git_revspec
#field from , Ptr <struct git_object>
#field to , Ptr <struct git_object>
#field flags , CUInt
#stoptype
#ccall git_revparse , Ptr <git_revspec> -> Ptr <struct git_repository> -> CString -> IO CInt
