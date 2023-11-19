{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/describe.h>
module Bindings.Libgit2.Describe where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Buffer
{- typedef enum {
            GIT_DESCRIBE_DEFAULT, GIT_DESCRIBE_TAGS, GIT_DESCRIBE_ALL
        } git_describe_strategy_t; -}
#integral_t git_describe_strategy_t
#num GIT_DESCRIBE_DEFAULT
#num GIT_DESCRIBE_TAGS
#num GIT_DESCRIBE_ALL
{- typedef struct git_describe_options {
            unsigned int version;
            unsigned int max_candidates_tags;
            unsigned int describe_strategy;
            const char * pattern;
            int only_follow_first_parent;
            int show_commit_oid_as_fallback;
        } git_describe_options; -}
#starttype struct git_describe_options
#field version , CUInt
#field max_candidates_tags , CUInt
#field describe_strategy , CUInt
#field pattern , CString
#field only_follow_first_parent , CInt
#field show_commit_oid_as_fallback , CInt
#stoptype
#ccall git_describe_options_init , Ptr <struct git_describe_options> -> CUInt -> IO CInt
{- typedef struct {
            unsigned int version;
            unsigned int abbreviated_size;
            int always_use_long_format;
            const char * dirty_suffix;
        } git_describe_format_options; -}
#starttype git_describe_format_options
#field version , CUInt
#field abbreviated_size , CUInt
#field always_use_long_format , CInt
#field dirty_suffix , CString
#stoptype
#ccall git_describe_format_options_init , Ptr <git_describe_format_options> -> CUInt -> IO CInt
{- typedef struct git_describe_result git_describe_result; -}
#opaque_t struct git_describe_result
#ccall git_describe_commit , Ptr (Ptr <struct git_describe_result>) -> Ptr <struct git_object> -> Ptr <struct git_describe_options> -> IO CInt
#ccall git_describe_workdir , Ptr (Ptr <struct git_describe_result>) -> Ptr <struct git_repository> -> Ptr <struct git_describe_options> -> IO CInt
#ccall git_describe_format , Ptr <git_buf> -> Ptr <struct git_describe_result> -> Ptr <git_describe_format_options> -> IO CInt
#ccall git_describe_result_free , Ptr <struct git_describe_result> -> IO ()
