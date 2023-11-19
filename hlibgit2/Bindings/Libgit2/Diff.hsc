{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/diff.h>
module Bindings.Libgit2.Diff where
import Foreign.Ptr
import Bindings.Libgit2.Buffer
import Bindings.Libgit2.Strarray
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Tree
import Bindings.Libgit2.Refs
{- typedef enum {
            GIT_DIFF_NORMAL = 0,
            GIT_DIFF_REVERSE = 1u << 0,
            GIT_DIFF_INCLUDE_IGNORED = 1u << 1,
            GIT_DIFF_RECURSE_IGNORED_DIRS = 1u << 2,
            GIT_DIFF_INCLUDE_UNTRACKED = 1u << 3,
            GIT_DIFF_RECURSE_UNTRACKED_DIRS = 1u << 4,
            GIT_DIFF_INCLUDE_UNMODIFIED = 1u << 5,
            GIT_DIFF_INCLUDE_TYPECHANGE = 1u << 6,
            GIT_DIFF_INCLUDE_TYPECHANGE_TREES = 1u << 7,
            GIT_DIFF_IGNORE_FILEMODE = 1u << 8,
            GIT_DIFF_IGNORE_SUBMODULES = 1u << 9,
            GIT_DIFF_IGNORE_CASE = 1u << 10,
            GIT_DIFF_INCLUDE_CASECHANGE = 1u << 11,
            GIT_DIFF_DISABLE_PATHSPEC_MATCH = 1u << 12,
            GIT_DIFF_SKIP_BINARY_CHECK = 1u << 13,
            GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS = 1u << 14,
            GIT_DIFF_UPDATE_INDEX = 1u << 15,
            GIT_DIFF_INCLUDE_UNREADABLE = 1u << 16,
            GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED = 1u << 17,
            GIT_DIFF_INDENT_HEURISTIC = 1u << 18,
            GIT_DIFF_IGNORE_BLANK_LINES = 1u << 19,
            GIT_DIFF_FORCE_TEXT = 1u << 20,
            GIT_DIFF_FORCE_BINARY = 1u << 21,
            GIT_DIFF_IGNORE_WHITESPACE = 1u << 22,
            GIT_DIFF_IGNORE_WHITESPACE_CHANGE = 1u << 23,
            GIT_DIFF_IGNORE_WHITESPACE_EOL = 1u << 24,
            GIT_DIFF_SHOW_UNTRACKED_CONTENT = 1u << 25,
            GIT_DIFF_SHOW_UNMODIFIED = 1u << 26,
            GIT_DIFF_PATIENCE = 1u << 28,
            GIT_DIFF_MINIMAL = 1u << 29,
            GIT_DIFF_SHOW_BINARY = 1u << 30
        } git_diff_option_t; -}
#integral_t git_diff_option_t
#num GIT_DIFF_NORMAL
#num GIT_DIFF_REVERSE
#num GIT_DIFF_INCLUDE_IGNORED
#num GIT_DIFF_RECURSE_IGNORED_DIRS
#num GIT_DIFF_INCLUDE_UNTRACKED
#num GIT_DIFF_RECURSE_UNTRACKED_DIRS
#num GIT_DIFF_INCLUDE_UNMODIFIED
#num GIT_DIFF_INCLUDE_TYPECHANGE
#num GIT_DIFF_INCLUDE_TYPECHANGE_TREES
#num GIT_DIFF_IGNORE_FILEMODE
#num GIT_DIFF_IGNORE_SUBMODULES
#num GIT_DIFF_IGNORE_CASE
#num GIT_DIFF_INCLUDE_CASECHANGE
#num GIT_DIFF_DISABLE_PATHSPEC_MATCH
#num GIT_DIFF_SKIP_BINARY_CHECK
#num GIT_DIFF_ENABLE_FAST_UNTRACKED_DIRS
#num GIT_DIFF_UPDATE_INDEX
#num GIT_DIFF_INCLUDE_UNREADABLE
#num GIT_DIFF_INCLUDE_UNREADABLE_AS_UNTRACKED
#num GIT_DIFF_INDENT_HEURISTIC
#num GIT_DIFF_IGNORE_BLANK_LINES
#num GIT_DIFF_FORCE_TEXT
#num GIT_DIFF_FORCE_BINARY
#num GIT_DIFF_IGNORE_WHITESPACE
#num GIT_DIFF_IGNORE_WHITESPACE_CHANGE
#num GIT_DIFF_IGNORE_WHITESPACE_EOL
#num GIT_DIFF_SHOW_UNTRACKED_CONTENT
#num GIT_DIFF_SHOW_UNMODIFIED
#num GIT_DIFF_PATIENCE
#num GIT_DIFF_MINIMAL
#num GIT_DIFF_SHOW_BINARY
{- typedef struct git_diff git_diff; -}
#opaque_t struct git_diff
{- typedef enum {
            GIT_DIFF_FLAG_BINARY = 1u << 0,
            GIT_DIFF_FLAG_NOT_BINARY = 1u << 1,
            GIT_DIFF_FLAG_VALID_ID = 1u << 2,
            GIT_DIFF_FLAG_EXISTS = 1u << 3,
            GIT_DIFF_FLAG_VALID_SIZE = 1u << 4
        } git_diff_flag_t; -}
#integral_t git_diff_flag_t
#num GIT_DIFF_FLAG_BINARY
#num GIT_DIFF_FLAG_NOT_BINARY
#num GIT_DIFF_FLAG_VALID_ID
#num GIT_DIFF_FLAG_EXISTS
#num GIT_DIFF_FLAG_VALID_SIZE
{- typedef enum {
            GIT_DELTA_UNMODIFIED = 0,
            GIT_DELTA_ADDED = 1,
            GIT_DELTA_DELETED = 2,
            GIT_DELTA_MODIFIED = 3,
            GIT_DELTA_RENAMED = 4,
            GIT_DELTA_COPIED = 5,
            GIT_DELTA_IGNORED = 6,
            GIT_DELTA_UNTRACKED = 7,
            GIT_DELTA_TYPECHANGE = 8,
            GIT_DELTA_UNREADABLE = 9,
            GIT_DELTA_CONFLICTED = 10
        } git_delta_t; -}
#integral_t git_delta_t
#num GIT_DELTA_UNMODIFIED
#num GIT_DELTA_ADDED
#num GIT_DELTA_DELETED
#num GIT_DELTA_MODIFIED
#num GIT_DELTA_RENAMED
#num GIT_DELTA_COPIED
#num GIT_DELTA_IGNORED
#num GIT_DELTA_UNTRACKED
#num GIT_DELTA_TYPECHANGE
#num GIT_DELTA_UNREADABLE
#num GIT_DELTA_CONFLICTED
{- typedef struct {
            git_oid id;
            const char * path;
            git_object_size_t size;
            uint32_t flags;
            uint16_t mode;
            uint16_t id_abbrev;
        } git_diff_file; -}
#starttype git_diff_file
#field id , <struct git_oid>
#field path , CString
#field size , CULong
#field flags , CUInt
#field mode , CUInt
#field id_abbrev , CUInt
#stoptype
{- typedef struct {
            git_delta_t status;
            uint32_t flags;
            uint16_t similarity;
            uint16_t nfiles;
            git_diff_file old_file;
            git_diff_file new_file;
        } git_diff_delta; -}
#starttype git_diff_delta
#field status , <git_delta_t>
#field flags , CUInt
#field similarity , CUInt
#field nfiles , CUInt
#field old_file , <git_diff_file>
#field new_file , <git_diff_file>
#stoptype
#callback git_diff_notify_cb , Ptr <struct git_diff> -> Ptr <git_diff_delta> -> CString -> Ptr () -> IO CInt
#callback git_diff_progress_cb , Ptr <struct git_diff> -> CString -> CString -> Ptr () -> IO CInt
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            git_submodule_ignore_t ignore_submodules;
            git_strarray pathspec;
            git_diff_notify_cb notify_cb;
            git_diff_progress_cb progress_cb;
            void * payload;
            uint32_t context_lines;
            uint32_t interhunk_lines;
            git_oid_t oid_type;
            uint16_t id_abbrev;
            git_off_t max_size;
            const char * old_prefix;
            const char * new_prefix;
        } git_diff_options; -}
#starttype git_diff_options
#field version , CUInt
#field flags , CUInt
#field ignore_submodules , <git_submodule_ignore_t>
#field pathspec , <struct git_strarray>
#field notify_cb , <git_diff_notify_cb>
#field progress_cb , <git_diff_progress_cb>
#field payload , Ptr ()
#field context_lines , CUInt
#field interhunk_lines , CUInt
#field oid_type , <git_oid_t>
#field id_abbrev , CUInt
#field max_size , CLong
#field old_prefix , CString
#field new_prefix , CString
#stoptype
#ccall git_diff_options_init , Ptr <git_diff_options> -> CUInt -> IO CInt
#callback git_diff_file_cb , Ptr <git_diff_delta> -> CFloat -> Ptr () -> IO CInt
{- typedef enum {
            GIT_DIFF_BINARY_NONE,
            GIT_DIFF_BINARY_LITERAL,
            GIT_DIFF_BINARY_DELTA
        } git_diff_binary_t; -}
#integral_t git_diff_binary_t
#num GIT_DIFF_BINARY_NONE
#num GIT_DIFF_BINARY_LITERAL
#num GIT_DIFF_BINARY_DELTA
{- typedef struct {
            git_diff_binary_t type;
            const char * data;
            size_t datalen;
            size_t inflatedlen;
        } git_diff_binary_file; -}
#starttype git_diff_binary_file
#field type , <git_diff_binary_t>
#field data , CString
#field datalen , CSize
#field inflatedlen , CSize
#stoptype
{- typedef struct {
            unsigned int contains_data;
            git_diff_binary_file old_file;
            git_diff_binary_file new_file;
        } git_diff_binary; -}
#starttype git_diff_binary
#field contains_data , CUInt
#field old_file , <git_diff_binary_file>
#field new_file , <git_diff_binary_file>
#stoptype
#callback git_diff_binary_cb , Ptr <git_diff_delta> -> Ptr <git_diff_binary> -> Ptr () -> IO CInt
{- typedef struct {
            int old_start;
            int old_lines;
            int new_start;
            int new_lines;
            size_t header_len;
            char header[128];
        } git_diff_hunk; -}
#starttype git_diff_hunk
#field old_start , CInt
#field old_lines , CInt
#field new_start , CInt
#field new_lines , CInt
#field header_len , CSize
#array_field header , CChar
#stoptype
#callback git_diff_hunk_cb , Ptr <git_diff_delta> -> Ptr <git_diff_hunk> -> Ptr () -> IO CInt
{- typedef enum {
            GIT_DIFF_LINE_CONTEXT = ' ',
            GIT_DIFF_LINE_ADDITION = '+',
            GIT_DIFF_LINE_DELETION = '-',
            GIT_DIFF_LINE_CONTEXT_EOFNL = '=',
            GIT_DIFF_LINE_ADD_EOFNL = '>',
            GIT_DIFF_LINE_DEL_EOFNL = '<',
            GIT_DIFF_LINE_FILE_HDR = 'F',
            GIT_DIFF_LINE_HUNK_HDR = 'H',
            GIT_DIFF_LINE_BINARY = 'B'
        } git_diff_line_t; -}
#integral_t git_diff_line_t
#num GIT_DIFF_LINE_CONTEXT
#num GIT_DIFF_LINE_ADDITION
#num GIT_DIFF_LINE_DELETION
#num GIT_DIFF_LINE_CONTEXT_EOFNL
#num GIT_DIFF_LINE_ADD_EOFNL
#num GIT_DIFF_LINE_DEL_EOFNL
#num GIT_DIFF_LINE_FILE_HDR
#num GIT_DIFF_LINE_HUNK_HDR
#num GIT_DIFF_LINE_BINARY
{- typedef struct {
            char origin;
            int old_lineno;
            int new_lineno;
            int num_lines;
            size_t content_len;
            git_off_t content_offset;
            const char * content;
        } git_diff_line; -}
#starttype git_diff_line
#field origin , CChar
#field old_lineno , CInt
#field new_lineno , CInt
#field num_lines , CInt
#field content_len , CSize
#field content_offset , CLong
#field content , CString
#stoptype
#callback git_diff_line_cb , Ptr <git_diff_delta> -> Ptr <git_diff_hunk> -> Ptr <git_diff_line> -> Ptr () -> IO CInt
{- typedef enum {
            GIT_DIFF_FIND_BY_CONFIG = 0,
            GIT_DIFF_FIND_RENAMES = 1u << 0,
            GIT_DIFF_FIND_RENAMES_FROM_REWRITES = 1u << 1,
            GIT_DIFF_FIND_COPIES = 1u << 2,
            GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED = 1u << 3,
            GIT_DIFF_FIND_REWRITES = 1u << 4,
            GIT_DIFF_BREAK_REWRITES = 1u << 5,
            GIT_DIFF_FIND_AND_BREAK_REWRITES = GIT_DIFF_FIND_REWRITES | GIT_DIFF_BREAK_REWRITES,
            GIT_DIFF_FIND_FOR_UNTRACKED = 1u << 6,
            GIT_DIFF_FIND_ALL = 0xff,
            GIT_DIFF_FIND_IGNORE_LEADING_WHITESPACE = 0,
            GIT_DIFF_FIND_IGNORE_WHITESPACE = 1u << 12,
            GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE = 1u << 13,
            GIT_DIFF_FIND_EXACT_MATCH_ONLY = 1u << 14,
            GIT_DIFF_BREAK_REWRITES_FOR_RENAMES_ONLY = 1u << 15,
            GIT_DIFF_FIND_REMOVE_UNMODIFIED = 1u << 16
        } git_diff_find_t; -}
#integral_t git_diff_find_t
#num GIT_DIFF_FIND_BY_CONFIG
#num GIT_DIFF_FIND_RENAMES
#num GIT_DIFF_FIND_RENAMES_FROM_REWRITES
#num GIT_DIFF_FIND_COPIES
#num GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED
#num GIT_DIFF_FIND_REWRITES
#num GIT_DIFF_BREAK_REWRITES
#num GIT_DIFF_FIND_AND_BREAK_REWRITES
#num GIT_DIFF_FIND_FOR_UNTRACKED
#num GIT_DIFF_FIND_ALL
#num GIT_DIFF_FIND_IGNORE_LEADING_WHITESPACE
#num GIT_DIFF_FIND_IGNORE_WHITESPACE
#num GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE
#num GIT_DIFF_FIND_EXACT_MATCH_ONLY
#num GIT_DIFF_BREAK_REWRITES_FOR_RENAMES_ONLY
#num GIT_DIFF_FIND_REMOVE_UNMODIFIED
{- typedef struct {
            int (* file_signature)(void * * out,
                                   const git_diff_file * file,
                                   const char * fullpath,
                                   void * payload);
            int (* buffer_signature)(void * * out,
                                     const git_diff_file * file,
                                     const char * buf,
                                     size_t buflen,
                                     void * payload);
            void (* free_signature)(void * sig, void * payload);
            int (* similarity)(int * score,
                               void * siga,
                               void * sigb,
                               void * payload);
            void * payload;
        } git_diff_similarity_metric; -}
#starttype git_diff_similarity_metric
#field file_signature , FunPtr (Ptr (Ptr ()) -> Ptr <git_diff_file> -> CString -> Ptr () -> CInt)
#field buffer_signature , FunPtr (Ptr (Ptr ()) -> Ptr <git_diff_file> -> CString -> CSize -> Ptr () -> CInt)
#field free_signature , FunPtr (Ptr () -> Ptr () -> IO ())
#field similarity , FunPtr (Ptr CInt -> Ptr () -> Ptr () -> Ptr () -> CInt)
#field payload , Ptr ()
#stoptype
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            uint16_t rename_threshold;
            uint16_t rename_from_rewrite_threshold;
            uint16_t copy_threshold;
            uint16_t break_rewrite_threshold;
            size_t rename_limit;
            git_diff_similarity_metric * metric;
        } git_diff_find_options; -}
#starttype git_diff_find_options
#field version , CUInt
#field flags , CUInt
#field rename_threshold , CUInt
#field rename_from_rewrite_threshold , CUInt
#field copy_threshold , CUInt
#field break_rewrite_threshold , CUInt
#field rename_limit , CSize
#field metric , Ptr <git_diff_similarity_metric>
#stoptype
#ccall git_diff_find_options_init , Ptr <git_diff_find_options> -> CUInt -> IO CInt
#ccall git_diff_free , Ptr <struct git_diff> -> IO ()
#ccall git_diff_tree_to_tree , Ptr (Ptr <struct git_diff>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> Ptr <struct git_tree> -> Ptr <git_diff_options> -> IO CInt
#ccall git_diff_tree_to_index , Ptr (Ptr <struct git_diff>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> Ptr <struct git_index> -> Ptr <git_diff_options> -> IO CInt
#ccall git_diff_index_to_workdir , Ptr (Ptr <struct git_diff>) -> Ptr <struct git_repository> -> Ptr <struct git_index> -> Ptr <git_diff_options> -> IO CInt
#ccall git_diff_tree_to_workdir , Ptr (Ptr <struct git_diff>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> Ptr <git_diff_options> -> IO CInt
#ccall git_diff_tree_to_workdir_with_index , Ptr (Ptr <struct git_diff>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> Ptr <git_diff_options> -> IO CInt
#ccall git_diff_index_to_index , Ptr (Ptr <struct git_diff>) -> Ptr <struct git_repository> -> Ptr <struct git_index> -> Ptr <struct git_index> -> Ptr <git_diff_options> -> IO CInt
#ccall git_diff_merge , Ptr <struct git_diff> -> Ptr <struct git_diff> -> IO CInt
#ccall git_diff_find_similar , Ptr <struct git_diff> -> Ptr <git_diff_find_options> -> IO CInt
#ccall git_diff_num_deltas , Ptr <struct git_diff> -> IO CSize
#ccall git_diff_num_deltas_of_type , Ptr <struct git_diff> -> <git_delta_t> -> IO CSize
#ccall git_diff_get_delta , Ptr <struct git_diff> -> CSize -> IO (Ptr <git_diff_delta>)
#ccall git_diff_is_sorted_icase , Ptr <struct git_diff> -> IO CInt
#ccall git_diff_foreach , Ptr <struct git_diff> -> <git_diff_file_cb> -> <git_diff_binary_cb> -> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO CInt
#ccall git_diff_status_char , <git_delta_t> -> IO CChar
{- typedef enum {
            GIT_DIFF_FORMAT_PATCH = 1u,
            GIT_DIFF_FORMAT_PATCH_HEADER = 2u,
            GIT_DIFF_FORMAT_RAW = 3u,
            GIT_DIFF_FORMAT_NAME_ONLY = 4u,
            GIT_DIFF_FORMAT_NAME_STATUS = 5u,
            GIT_DIFF_FORMAT_PATCH_ID = 6u
        } git_diff_format_t; -}
#integral_t git_diff_format_t
#num GIT_DIFF_FORMAT_PATCH
#num GIT_DIFF_FORMAT_PATCH_HEADER
#num GIT_DIFF_FORMAT_RAW
#num GIT_DIFF_FORMAT_NAME_ONLY
#num GIT_DIFF_FORMAT_NAME_STATUS
#num GIT_DIFF_FORMAT_PATCH_ID
#ccall git_diff_print , Ptr <struct git_diff> -> <git_diff_format_t> -> <git_diff_line_cb> -> Ptr () -> IO CInt
#ccall git_diff_to_buf , Ptr <git_buf> -> Ptr <struct git_diff> -> <git_diff_format_t> -> IO CInt
#ccall git_diff_blobs , Ptr <struct git_blob> -> CString -> Ptr <struct git_blob> -> CString -> Ptr <git_diff_options> -> <git_diff_file_cb> -> <git_diff_binary_cb> -> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO CInt
#ccall git_diff_blob_to_buffer , Ptr <struct git_blob> -> CString -> CString -> CSize -> CString -> Ptr <git_diff_options> -> <git_diff_file_cb> -> <git_diff_binary_cb> -> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO CInt
#ccall git_diff_buffers , Ptr () -> CSize -> CString -> Ptr () -> CSize -> CString -> Ptr <git_diff_options> -> <git_diff_file_cb> -> <git_diff_binary_cb> -> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO CInt
{- typedef struct {
            unsigned int version; git_oid_t oid_type;
        } git_diff_parse_options; -}
#starttype git_diff_parse_options
#field version , CUInt
#field oid_type , <git_oid_t>
#stoptype
#ccall git_diff_from_buffer , Ptr (Ptr <struct git_diff>) -> CString -> CSize -> IO CInt
{- typedef struct git_diff_stats git_diff_stats; -}
#opaque_t struct git_diff_stats
{- typedef enum {
            GIT_DIFF_STATS_NONE = 0,
            GIT_DIFF_STATS_FULL = 1u << 0,
            GIT_DIFF_STATS_SHORT = 1u << 1,
            GIT_DIFF_STATS_NUMBER = 1u << 2,
            GIT_DIFF_STATS_INCLUDE_SUMMARY = 1u << 3
        } git_diff_stats_format_t; -}
#integral_t git_diff_stats_format_t
#num GIT_DIFF_STATS_NONE
#num GIT_DIFF_STATS_FULL
#num GIT_DIFF_STATS_SHORT
#num GIT_DIFF_STATS_NUMBER
#num GIT_DIFF_STATS_INCLUDE_SUMMARY
#ccall git_diff_get_stats , Ptr (Ptr <struct git_diff_stats>) -> Ptr <struct git_diff> -> IO CInt
#ccall git_diff_stats_files_changed , Ptr <struct git_diff_stats> -> IO CSize
#ccall git_diff_stats_insertions , Ptr <struct git_diff_stats> -> IO CSize
#ccall git_diff_stats_deletions , Ptr <struct git_diff_stats> -> IO CSize
#ccall git_diff_stats_to_buf , Ptr <git_buf> -> Ptr <struct git_diff_stats> -> <git_diff_stats_format_t> -> CSize -> IO CInt
#ccall git_diff_stats_free , Ptr <struct git_diff_stats> -> IO ()
{- typedef struct git_diff_patchid_options {
            unsigned int version;
        } git_diff_patchid_options; -}
#starttype struct git_diff_patchid_options
#field version , CUInt
#stoptype
#ccall git_diff_patchid_options_init , Ptr <struct git_diff_patchid_options> -> CUInt -> IO CInt
#ccall git_diff_patchid , Ptr <struct git_oid> -> Ptr <struct git_diff> -> Ptr <struct git_diff_patchid_options> -> IO CInt
