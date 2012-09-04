#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Diff where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Tree
import Bindings.Libgit2.Refs
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
{- typedef int (* git_diff_data_fn)(void * cb_data,
                                 git_diff_delta * delta,
                                 git_diff_range * range,
                                 char line_origin,
                                 const char * content,
                                 size_t content_len); -}
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
