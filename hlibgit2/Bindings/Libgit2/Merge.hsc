{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/merge.h>
module Bindings.Libgit2.Merge where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Oidarray
import Bindings.Libgit2.Checkout
import Bindings.Libgit2.Index
import Bindings.Libgit2.AnnotatedCommit
import Bindings.Libgit2.Diff
{- typedef struct {
            unsigned int version;
            const char * ptr;
            size_t size;
            const char * path;
            unsigned int mode;
        } git_merge_file_input; -}
#starttype git_merge_file_input
#field version , CUInt
#field ptr , CString
#field size , CSize
#field path , CString
#field mode , CUInt
#stoptype
#ccall git_merge_file_input_init , Ptr <git_merge_file_input> -> CUInt -> IO CInt
{- typedef enum {
            GIT_MERGE_FIND_RENAMES = 1 << 0,
            GIT_MERGE_FAIL_ON_CONFLICT = 1 << 1,
            GIT_MERGE_SKIP_REUC = 1 << 2,
            GIT_MERGE_NO_RECURSIVE = 1 << 3,
            GIT_MERGE_VIRTUAL_BASE = 1 << 4
        } git_merge_flag_t; -}
#integral_t git_merge_flag_t
#num GIT_MERGE_FIND_RENAMES
#num GIT_MERGE_FAIL_ON_CONFLICT
#num GIT_MERGE_SKIP_REUC
#num GIT_MERGE_NO_RECURSIVE
#num GIT_MERGE_VIRTUAL_BASE
{- typedef enum {
            GIT_MERGE_FILE_FAVOR_NORMAL = 0,
            GIT_MERGE_FILE_FAVOR_OURS = 1,
            GIT_MERGE_FILE_FAVOR_THEIRS = 2,
            GIT_MERGE_FILE_FAVOR_UNION = 3
        } git_merge_file_favor_t; -}
#integral_t git_merge_file_favor_t
#num GIT_MERGE_FILE_FAVOR_NORMAL
#num GIT_MERGE_FILE_FAVOR_OURS
#num GIT_MERGE_FILE_FAVOR_THEIRS
#num GIT_MERGE_FILE_FAVOR_UNION
{- typedef enum {
            GIT_MERGE_FILE_DEFAULT = 0,
            GIT_MERGE_FILE_STYLE_MERGE = 1 << 0,
            GIT_MERGE_FILE_STYLE_DIFF3 = 1 << 1,
            GIT_MERGE_FILE_SIMPLIFY_ALNUM = 1 << 2,
            GIT_MERGE_FILE_IGNORE_WHITESPACE = 1 << 3,
            GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE = 1 << 4,
            GIT_MERGE_FILE_IGNORE_WHITESPACE_EOL = 1 << 5,
            GIT_MERGE_FILE_DIFF_PATIENCE = 1 << 6,
            GIT_MERGE_FILE_DIFF_MINIMAL = 1 << 7,
            GIT_MERGE_FILE_STYLE_ZDIFF3 = 1 << 8,
            GIT_MERGE_FILE_ACCEPT_CONFLICTS = 1 << 9
        } git_merge_file_flag_t; -}
#integral_t git_merge_file_flag_t
#num GIT_MERGE_FILE_DEFAULT
#num GIT_MERGE_FILE_STYLE_MERGE
#num GIT_MERGE_FILE_STYLE_DIFF3
#num GIT_MERGE_FILE_SIMPLIFY_ALNUM
#num GIT_MERGE_FILE_IGNORE_WHITESPACE
#num GIT_MERGE_FILE_IGNORE_WHITESPACE_CHANGE
#num GIT_MERGE_FILE_IGNORE_WHITESPACE_EOL
#num GIT_MERGE_FILE_DIFF_PATIENCE
#num GIT_MERGE_FILE_DIFF_MINIMAL
#num GIT_MERGE_FILE_STYLE_ZDIFF3
#num GIT_MERGE_FILE_ACCEPT_CONFLICTS
{- typedef struct {
            unsigned int version;
            const char * ancestor_label;
            const char * our_label;
            const char * their_label;
            git_merge_file_favor_t favor;
            uint32_t flags;
            unsigned short marker_size;
        } git_merge_file_options; -}
#starttype git_merge_file_options
#field version , CUInt
#field ancestor_label , CString
#field our_label , CString
#field their_label , CString
#field favor , <git_merge_file_favor_t>
#field flags , CUInt
#field marker_size , CUShort
#stoptype
#ccall git_merge_file_options_init , Ptr <git_merge_file_options> -> CUInt -> IO CInt
{- typedef struct {
            unsigned int automergeable;
            const char * path;
            unsigned int mode;
            const char * ptr;
            size_t len;
        } git_merge_file_result; -}
#starttype git_merge_file_result
#field automergeable , CUInt
#field path , CString
#field mode , CUInt
#field ptr , CString
#field len , CSize
#stoptype
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            unsigned int rename_threshold;
            unsigned int target_limit;
            git_diff_similarity_metric * metric;
            unsigned int recursion_limit;
            const char * default_driver;
            git_merge_file_favor_t file_favor;
            uint32_t file_flags;
        } git_merge_options; -}
#starttype git_merge_options
#field version , CUInt
#field flags , CUInt
#field rename_threshold , CUInt
#field target_limit , CUInt
#field metric , Ptr <git_diff_similarity_metric>
#field recursion_limit , CUInt
#field default_driver , CString
#field file_favor , <git_merge_file_favor_t>
#field file_flags , CUInt
#stoptype
#ccall git_merge_options_init , Ptr <git_merge_options> -> CUInt -> IO CInt
{- typedef enum {
            GIT_MERGE_ANALYSIS_NONE = 0,
            GIT_MERGE_ANALYSIS_NORMAL = 1 << 0,
            GIT_MERGE_ANALYSIS_UP_TO_DATE = 1 << 1,
            GIT_MERGE_ANALYSIS_FASTFORWARD = 1 << 2,
            GIT_MERGE_ANALYSIS_UNBORN = 1 << 3
        } git_merge_analysis_t; -}
#integral_t git_merge_analysis_t
#num GIT_MERGE_ANALYSIS_NONE
#num GIT_MERGE_ANALYSIS_NORMAL
#num GIT_MERGE_ANALYSIS_UP_TO_DATE
#num GIT_MERGE_ANALYSIS_FASTFORWARD
#num GIT_MERGE_ANALYSIS_UNBORN
{- typedef enum {
            GIT_MERGE_PREFERENCE_NONE = 0,
            GIT_MERGE_PREFERENCE_NO_FASTFORWARD = 1 << 0,
            GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY = 1 << 1
        } git_merge_preference_t; -}
#integral_t git_merge_preference_t
#num GIT_MERGE_PREFERENCE_NONE
#num GIT_MERGE_PREFERENCE_NO_FASTFORWARD
#num GIT_MERGE_PREFERENCE_FASTFORWARD_ONLY
#ccall git_merge_analysis , Ptr <git_merge_analysis_t> -> Ptr <git_merge_preference_t> -> Ptr <struct git_repository> -> Ptr (Ptr <struct git_annotated_commit>) -> CSize -> IO CInt
#ccall git_merge_analysis_for_ref , Ptr <git_merge_analysis_t> -> Ptr <git_merge_preference_t> -> Ptr <struct git_repository> -> Ptr <struct git_reference> -> Ptr (Ptr <struct git_annotated_commit>) -> CSize -> IO CInt
#ccall git_merge_base , Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_merge_bases , Ptr <struct git_oidarray> -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_merge_base_many , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CSize -> Ptr (<struct git_oid>) -> IO CInt
#ccall git_merge_bases_many , Ptr <struct git_oidarray> -> Ptr <struct git_repository> -> CSize -> Ptr (<struct git_oid>) -> IO CInt
#ccall git_merge_base_octopus , Ptr <struct git_oid> -> Ptr <struct git_repository> -> CSize -> Ptr (<struct git_oid>) -> IO CInt
#ccall git_merge_file , Ptr <git_merge_file_result> -> Ptr <git_merge_file_input> -> Ptr <git_merge_file_input> -> Ptr <git_merge_file_input> -> Ptr <git_merge_file_options> -> IO CInt
#ccall git_merge_file_from_index , Ptr <git_merge_file_result> -> Ptr <struct git_repository> -> Ptr <struct git_index_entry> -> Ptr <struct git_index_entry> -> Ptr <struct git_index_entry> -> Ptr <git_merge_file_options> -> IO CInt
#ccall git_merge_file_result_free , Ptr <git_merge_file_result> -> IO ()
#ccall git_merge_trees , Ptr (Ptr <struct git_index>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> Ptr <struct git_tree> -> Ptr <struct git_tree> -> Ptr <git_merge_options> -> IO CInt
#ccall git_merge_commits , Ptr (Ptr <struct git_index>) -> Ptr <struct git_repository> -> Ptr <struct git_commit> -> Ptr <struct git_commit> -> Ptr <git_merge_options> -> IO CInt
#ccall git_merge , Ptr <struct git_repository> -> Ptr (Ptr <struct git_annotated_commit>) -> CSize -> Ptr <git_merge_options> -> Ptr <struct git_checkout_options> -> IO CInt
