#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Diff where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Tree
import Bindings.Libgit2.Refs
{- enum {
    GIT_DIFF_NORMAL = 0,
    GIT_DIFF_REVERSE = 1 << 0,
    GIT_DIFF_FORCE_TEXT = 1 << 1,
    GIT_DIFF_IGNORE_WHITESPACE = 1 << 2,
    GIT_DIFF_IGNORE_WHITESPACE_CHANGE = 1 << 3,
    GIT_DIFF_IGNORE_WHITESPACE_EOL = 1 << 4,
    GIT_DIFF_IGNORE_SUBMODULES = 1 << 5,
    GIT_DIFF_PATIENCE = 1 << 6,
    GIT_DIFF_INCLUDE_IGNORED = 1 << 7,
    GIT_DIFF_INCLUDE_UNTRACKED = 1 << 8,
    GIT_DIFF_INCLUDE_UNMODIFIED = 1 << 9,
    GIT_DIFF_RECURSE_UNTRACKED_DIRS = 1 << 10
}; -}
#num GIT_DIFF_NORMAL
#num GIT_DIFF_REVERSE
#num GIT_DIFF_FORCE_TEXT
#num GIT_DIFF_IGNORE_WHITESPACE
#num GIT_DIFF_IGNORE_WHITESPACE_CHANGE
#num GIT_DIFF_IGNORE_WHITESPACE_EOL
#num GIT_DIFF_IGNORE_SUBMODULES
#num GIT_DIFF_PATIENCE
#num GIT_DIFF_INCLUDE_IGNORED
#num GIT_DIFF_INCLUDE_UNTRACKED
#num GIT_DIFF_INCLUDE_UNMODIFIED
#num GIT_DIFF_RECURSE_UNTRACKED_DIRS
{- typedef struct {
            uint32_t flags;
            uint16_t context_lines;
            uint16_t interhunk_lines;
            char * old_prefix;
            char * new_prefix;
            git_strarray pathspec;
        } git_diff_options; -}
#starttype git_diff_options
#field flags , CUInt
#field context_lines , CUShort
#field interhunk_lines , CUShort
#field old_prefix , CString
#field new_prefix , CString
#field pathspec , <git_strarray>
#stoptype
{- typedef struct git_diff_list git_diff_list; -}
#opaque_t git_diff_list
{- enum {
    GIT_DIFF_FILE_VALID_OID = 1 << 0,
    GIT_DIFF_FILE_FREE_PATH = 1 << 1,
    GIT_DIFF_FILE_BINARY = 1 << 2,
    GIT_DIFF_FILE_NOT_BINARY = 1 << 3,
    GIT_DIFF_FILE_FREE_DATA = 1 << 4,
    GIT_DIFF_FILE_UNMAP_DATA = 1 << 5
}; -}
#num GIT_DIFF_FILE_VALID_OID
#num GIT_DIFF_FILE_FREE_PATH
#num GIT_DIFF_FILE_BINARY
#num GIT_DIFF_FILE_NOT_BINARY
#num GIT_DIFF_FILE_FREE_DATA
#num GIT_DIFF_FILE_UNMAP_DATA
{- typedef enum {
            GIT_DELTA_UNMODIFIED = 0,
            GIT_DELTA_ADDED = 1,
            GIT_DELTA_DELETED = 2,
            GIT_DELTA_MODIFIED = 3,
            GIT_DELTA_RENAMED = 4,
            GIT_DELTA_COPIED = 5,
            GIT_DELTA_IGNORED = 6,
            GIT_DELTA_UNTRACKED = 7
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
{- typedef struct {
            git_oid oid;
            char * path;
            uint16_t mode;
            git_off_t size;
            unsigned int flags;
        } git_diff_file; -}
#starttype git_diff_file
#field oid , <git_oid>
#field path , CString
#field mode , CUShort
#field size , CLong
#field flags , CUInt
#stoptype
{- typedef struct {
            git_diff_file old_file;
            git_diff_file new_file;
            git_delta_t status;
            unsigned int similarity;
            int binary;
        } git_diff_delta; -}
#starttype git_diff_delta
#field old_file , <git_diff_file>
#field new_file , <git_diff_file>
#field status , <git_delta_t>
#field similarity , CUInt
#field binary , CInt
#stoptype
{- typedef int (* git_diff_file_fn)(void * cb_data,
                                 git_diff_delta * delta,
                                 float progress); -}
#synonym_t git_diff_file_fn , CInt
{- typedef struct {
            int old_start; int old_lines; int new_start; int new_lines;
        } git_diff_range; -}
#starttype git_diff_range
#field old_start , CInt
#field old_lines , CInt
#field new_start , CInt
#field new_lines , CInt
#stoptype
{- typedef int (* git_diff_hunk_fn)(void * cb_data,
                                 git_diff_delta * delta,
                                 git_diff_range * range,
                                 const char * header,
                                 size_t header_len); -}
#synonym_t git_diff_hunk_fn , CInt
{- enum {
    GIT_DIFF_LINE_CONTEXT = ' ',
    GIT_DIFF_LINE_ADDITION = '+',
    GIT_DIFF_LINE_DELETION = '-',
    GIT_DIFF_LINE_ADD_EOFNL = '\n',
    GIT_DIFF_LINE_DEL_EOFNL = '\0',
    GIT_DIFF_LINE_FILE_HDR = 'F',
    GIT_DIFF_LINE_HUNK_HDR = 'H',
    GIT_DIFF_LINE_BINARY = 'B'
}; -}
#num GIT_DIFF_LINE_CONTEXT
#num GIT_DIFF_LINE_ADDITION
#num GIT_DIFF_LINE_DELETION
#num GIT_DIFF_LINE_ADD_EOFNL
#num GIT_DIFF_LINE_DEL_EOFNL
#num GIT_DIFF_LINE_FILE_HDR
#num GIT_DIFF_LINE_HUNK_HDR
#num GIT_DIFF_LINE_BINARY
{- typedef int (* git_diff_data_fn)(void * cb_data,
                                 git_diff_delta * delta,
                                 git_diff_range * range,
                                 char line_origin,
                                 const char * content,
                                 size_t content_len); -}
#synonym_t git_diff_data_fn , CInt
#ccall git_diff_list_free , Ptr <git_diff_list> -> IO ()
#ccall git_diff_tree_to_tree , Ptr <git_repository> -> Ptr <git_diff_options> -> Ptr <git_tree> -> Ptr <git_tree> -> Ptr (Ptr <git_diff_list>) -> IO (CInt)
#ccall git_diff_index_to_tree , Ptr <git_repository> -> Ptr <git_diff_options> -> Ptr <git_tree> -> Ptr (Ptr <git_diff_list>) -> IO (CInt)
#ccall git_diff_workdir_to_index , Ptr <git_repository> -> Ptr <git_diff_options> -> Ptr (Ptr <git_diff_list>) -> IO (CInt)
#ccall git_diff_workdir_to_tree , Ptr <git_repository> -> Ptr <git_diff_options> -> Ptr <git_tree> -> Ptr (Ptr <git_diff_list>) -> IO (CInt)
#ccall git_diff_merge , Ptr <git_diff_list> -> Ptr <git_diff_list> -> IO (CInt)
#ccall git_diff_foreach , Ptr <git_diff_list> -> Ptr () -> CInt -> CInt -> CInt -> IO (CInt)
#ccall git_diff_print_compact , Ptr <git_diff_list> -> Ptr () -> CInt -> IO (CInt)
#ccall git_diff_print_patch , Ptr <git_diff_list> -> Ptr () -> CInt -> IO (CInt)
#ccall git_diff_blobs , Ptr <git_blob> -> Ptr <git_blob> -> Ptr <git_diff_options> -> Ptr () -> CInt -> CInt -> CInt -> IO (CInt)
