{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/pathspec.h>
module Bindings.Libgit2.Pathspec where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Diff
{- typedef struct git_pathspec git_pathspec; -}
#opaque_t struct git_pathspec
{- typedef struct git_pathspec_match_list git_pathspec_match_list; -}
#opaque_t struct git_pathspec_match_list
{- typedef enum {
            GIT_PATHSPEC_DEFAULT = 0,
            GIT_PATHSPEC_IGNORE_CASE = 1u << 0,
            GIT_PATHSPEC_USE_CASE = 1u << 1,
            GIT_PATHSPEC_NO_GLOB = 1u << 2,
            GIT_PATHSPEC_NO_MATCH_ERROR = 1u << 3,
            GIT_PATHSPEC_FIND_FAILURES = 1u << 4,
            GIT_PATHSPEC_FAILURES_ONLY = 1u << 5
        } git_pathspec_flag_t; -}
#integral_t git_pathspec_flag_t
#num GIT_PATHSPEC_DEFAULT
#num GIT_PATHSPEC_IGNORE_CASE
#num GIT_PATHSPEC_USE_CASE
#num GIT_PATHSPEC_NO_GLOB
#num GIT_PATHSPEC_NO_MATCH_ERROR
#num GIT_PATHSPEC_FIND_FAILURES
#num GIT_PATHSPEC_FAILURES_ONLY
#ccall git_pathspec_new , Ptr (Ptr <struct git_pathspec>) -> Ptr <struct git_strarray> -> IO CInt
#ccall git_pathspec_free , Ptr <struct git_pathspec> -> IO ()
#ccall git_pathspec_matches_path , Ptr <struct git_pathspec> -> CUInt -> CString -> IO CInt
#ccall git_pathspec_match_workdir , Ptr (Ptr <struct git_pathspec_match_list>) -> Ptr <struct git_repository> -> CUInt -> Ptr <struct git_pathspec> -> IO CInt
#ccall git_pathspec_match_index , Ptr (Ptr <struct git_pathspec_match_list>) -> Ptr <struct git_index> -> CUInt -> Ptr <struct git_pathspec> -> IO CInt
#ccall git_pathspec_match_tree , Ptr (Ptr <struct git_pathspec_match_list>) -> Ptr <struct git_tree> -> CUInt -> Ptr <struct git_pathspec> -> IO CInt
#ccall git_pathspec_match_diff , Ptr (Ptr <struct git_pathspec_match_list>) -> Ptr <struct git_diff> -> CUInt -> Ptr <struct git_pathspec> -> IO CInt
#ccall git_pathspec_match_list_free , Ptr <struct git_pathspec_match_list> -> IO ()
#ccall git_pathspec_match_list_entrycount , Ptr <struct git_pathspec_match_list> -> IO CSize
#ccall git_pathspec_match_list_entry , Ptr <struct git_pathspec_match_list> -> CSize -> IO CString
#ccall git_pathspec_match_list_diff_entry , Ptr <struct git_pathspec_match_list> -> CSize -> IO (Ptr <git_diff_delta>)
#ccall git_pathspec_match_list_failed_entrycount , Ptr <struct git_pathspec_match_list> -> IO CSize
#ccall git_pathspec_match_list_failed_entry , Ptr <struct git_pathspec_match_list> -> CSize -> IO CString
