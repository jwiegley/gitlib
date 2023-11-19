{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/apply.h>
module Bindings.Libgit2.Apply where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Diff
#callback git_apply_delta_cb , Ptr <git_diff_delta> -> Ptr () -> IO CInt
#callback git_apply_hunk_cb , Ptr <git_diff_hunk> -> Ptr () -> IO CInt
{- typedef enum {
            GIT_APPLY_CHECK = 1 << 0
        } git_apply_flags_t; -}
#integral_t git_apply_flags_t
#num GIT_APPLY_CHECK
{- typedef struct {
            unsigned int version;
            git_apply_delta_cb delta_cb;
            git_apply_hunk_cb hunk_cb;
            void * payload;
            unsigned int flags;
        } git_apply_options; -}
#starttype git_apply_options
#field version , CUInt
#field delta_cb , <git_apply_delta_cb>
#field hunk_cb , <git_apply_hunk_cb>
#field payload , Ptr ()
#field flags , CUInt
#stoptype
#ccall git_apply_options_init , Ptr <git_apply_options> -> CUInt -> IO CInt
#ccall git_apply_to_tree , Ptr (Ptr <struct git_index>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> Ptr <struct git_diff> -> Ptr <git_apply_options> -> IO CInt
{- typedef enum {
            GIT_APPLY_LOCATION_WORKDIR = 0,
            GIT_APPLY_LOCATION_INDEX = 1,
            GIT_APPLY_LOCATION_BOTH = 2
        } git_apply_location_t; -}
#integral_t git_apply_location_t
#num GIT_APPLY_LOCATION_WORKDIR
#num GIT_APPLY_LOCATION_INDEX
#num GIT_APPLY_LOCATION_BOTH
#ccall git_apply , Ptr <struct git_repository> -> Ptr <struct git_diff> -> <git_apply_location_t> -> Ptr <git_apply_options> -> IO CInt
