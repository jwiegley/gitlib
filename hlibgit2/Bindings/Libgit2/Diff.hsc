{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Diff where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Strarray
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Tree
import Bindings.Libgit2.Refs
{- typedef enum {
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
            GIT_DIFF_RECURSE_UNTRACKED_DIRS = 1 << 10,
            GIT_DIFF_DISABLE_PATHSPEC_MATCH = 1 << 11,
            GIT_DIFF_DELTAS_ARE_ICASE = 1 << 12,
            GIT_DIFF_INCLUDE_UNTRACKED_CONTENT = 1 << 13,
            GIT_DIFF_SKIP_BINARY_CHECK = 1 << 14,
            GIT_DIFF_INCLUDE_TYPECHANGE = 1 << 15,
            GIT_DIFF_INCLUDE_TYPECHANGE_TREES = 1 << 16,
            GIT_DIFF_IGNORE_FILEMODE = 1 << 17,
            GIT_DIFF_RECURSE_IGNORED_DIRS = 1 << 18
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
#opaque_t git_diff
{- typedef enum {
            GIT_DIFF_FLAG_BINARY = 1 << 0,
            GIT_DIFF_FLAG_NOT_BINARY = 1 << 1,
            GIT_DIFF_FLAG_VALID_OID = 1 << 2
        } git_diff_flag_t; -}
#integral_t git_diff_flag_t
#num GIT_DIFF_FLAG_BINARY
#num GIT_DIFF_FLAG_NOT_BINARY
#num GIT_DIFF_FLAG_VALID_ID
#num GIT_DIFF_FLAG_EXISTS
{- typedef enum {
            GIT_DELTA_UNMODIFIED = 0,
            GIT_DELTA_ADDED = 1,
            GIT_DELTA_DELETED = 2,
            GIT_DELTA_MODIFIED = 3,
            GIT_DELTA_RENAMED = 4,
            GIT_DELTA_COPIED = 5,
            GIT_DELTA_IGNORED = 6,
            GIT_DELTA_UNTRACKED = 7,
            GIT_DELTA_TYPECHANGE = 8
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
{- typedef struct {
            git_oid oid;
            const char * path;
            git_off_t size;
            uint32_t flags;
            uint16_t mode;
        } git_diff_file; -}
#starttype git_diff_file
#field id , <git_oid>
#field path , CString
#field size , CLong
#field flags , CUInt
#field mode , CUShort
#stoptype
{- typedef struct {
            git_diff_file old_file;
            git_diff_file new_file;
            git_delta_t status;
            uint32_t similarity;
            uint32_t flags;
        } git_diff_delta; -}
#starttype git_diff_delta
#field old_file , <git_diff_file>
#field new_file , <git_diff_file>
#field status , <git_delta_t>
#field similarity , CUInt
#field flags , CUInt
#stoptype
{- typedef int (* git_diff_notify_cb)(const git_diff_list * diff_so_far,
                                   const git_diff_delta * delta_to_add,
                                   const char * matched_pathspec,
                                   void * payload); -}
#callback git_diff_notify_cb , Ptr (<git_diff_list>) -> Ptr (<git_diff_delta>) -> CString -> Ptr () -> IO CInt
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            uint16_t context_lines;
            uint16_t interhunk_lines;
            const char * old_prefix;
            const char * new_prefix;
            git_strarray pathspec;
            git_off_t max_size;
            git_diff_notify_cb notify_cb;
            void * notify_payload;
        } git_diff_options; -}
#starttype git_diff_options
#field version , CUInt
#field flags , CUInt
#field context_lines , CUShort
#field interhunk_lines , CUShort
#field old_prefix , CString
#field new_prefix , CString
#field pathspec , <git_strarray>
#field max_size , CLong
#field notify_cb , <git_diff_notify_cb>
#field notify_payload , Ptr ()
#stoptype
{- typedef int (* git_diff_file_cb)(const git_diff_delta * delta,
                                 float progress,
                                 void * payload); -}
#callback git_diff_file_cb , Ptr (<git_diff_delta>) -> CFloat -> Ptr () -> IO CInt
{- typedef struct {
            int old_start; int old_lines; int new_start; int new_lines;
        } git_diff_range; -}
#starttype git_diff_hunk
#field old_start , CInt
#field old_lines , CInt
#field new_start , CInt
#field new_lines , CInt
#field header_len , CSize
#array_field header , CChar
#stoptype
{- typedef int (* git_diff_hunk_cb)(const git_diff_delta * delta,
                                 const git_diff_range * range,
                                 const char * header,
                                 size_t header_len,
                                 void * payload); -}
#callback git_diff_hunk_cb , Ptr (<git_diff_delta>) -> Ptr (<git_diff_hunk>) -> Ptr () -> IO CInt
{- typedef enum {
            GIT_DIFF_LINE_CONTEXT = ' ',
            GIT_DIFF_LINE_ADDITION = '+',
            GIT_DIFF_LINE_DELETION = '-',
            GIT_DIFF_LINE_ADD_EOFNL = '\n',
            GIT_DIFF_LINE_DEL_EOFNL = '\0',
            GIT_DIFF_LINE_FILE_HDR = 'F',
            GIT_DIFF_LINE_HUNK_HDR = 'H',
            GIT_DIFF_LINE_BINARY = 'B'
        } git_diff_line_t; -}
#integral_t git_diff_line_t
#num GIT_DIFF_LINE_CONTEXT
#num GIT_DIFF_LINE_ADDITION
#num GIT_DIFF_LINE_DELETION
#num GIT_DIFF_LINE_ADD_EOFNL
#num GIT_DIFF_LINE_DEL_EOFNL
#num GIT_DIFF_LINE_FILE_HDR
#num GIT_DIFF_LINE_HUNK_HDR
#num GIT_DIFF_LINE_BINARY
{- typedef struct git_diff_patch git_diff_patch; -}
#opaque_t git_diff_patch
{- typedef enum {
            GIT_DIFF_FIND_RENAMES = 1 << 0,
            GIT_DIFF_FIND_RENAMES_FROM_REWRITES = 1 << 1,
            GIT_DIFF_FIND_COPIES = 1 << 2,
            GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED = 1 << 3,
            GIT_DIFF_FIND_AND_BREAK_REWRITES = 1 << 4,
            GIT_DIFF_FIND_ALL = 0x1f,
            GIT_DIFF_FIND_IGNORE_LEADING_WHITESPACE = 0,
            GIT_DIFF_FIND_IGNORE_WHITESPACE = 1 << 6,
            GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE = 1 << 7
        } git_diff_find_t; -}
#integral_t git_diff_find_t
#num GIT_DIFF_FIND_RENAMES
#num GIT_DIFF_FIND_RENAMES_FROM_REWRITES
#num GIT_DIFF_FIND_COPIES
#num GIT_DIFF_FIND_COPIES_FROM_UNMODIFIED
#num GIT_DIFF_FIND_AND_BREAK_REWRITES
#num GIT_DIFF_FIND_ALL
#num GIT_DIFF_FIND_IGNORE_LEADING_WHITESPACE
#num GIT_DIFF_FIND_IGNORE_WHITESPACE
#num GIT_DIFF_FIND_DONT_IGNORE_WHITESPACE
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
#callback git_diff_similarity_metric_file_signature_callback , Ptr (Ptr ()) -> Ptr <git_diff_file> -> CString -> Ptr () -> IO CInt
#callback git_diff_similarity_metric_buffer_signature_callback , Ptr (Ptr ()) -> Ptr <git_diff_file> -> CString -> CSize -> Ptr () -> IO CInt
#callback git_diff_similarity_metric_free_signature_callback , Ptr () -> Ptr () -> IO ()
#callback git_diff_similarity_metric_similarity_callback , Ptr CInt -> Ptr () -> Ptr () -> Ptr () -> IO CInt
#starttype git_diff_similarity_metric
#field file_signature , <git_diff_similarity_metric_file_signature_callback>
#field buffer_signature , <git_diff_similarity_metric_buffer_signature_callback>
#field free_signature , <git_diff_similarity_metric_free_signature_callback>
#field similarity , <git_diff_similarity_metric_similarity_callback>
#field payload , Ptr ()
#stoptype
{- typedef struct {
            unsigned int version;
            unsigned int flags;
            unsigned int rename_threshold;
            unsigned int rename_from_rewrite_threshold;
            unsigned int copy_threshold;
            unsigned int break_rewrite_threshold;
            unsigned int target_limit;
            git_diff_similarity_metric * metric;
        } git_diff_find_options; -}
#starttype git_diff_find_options
#field version , CUInt
#field flags , CUInt
#field rename_threshold , CUInt
#field rename_from_rewrite_threshold , CUInt
#field copy_threshold , CUInt
#field break_rewrite_threshold , CUInt
#field rename_limit , CUInt
#field metric , Ptr <git_diff_similarity_metric>
#stoptype
#ccall git_diff_list_free , Ptr <git_diff_list> -> IO ()
#ccall git_diff_tree_to_tree , Ptr (Ptr <git_diff_list>) -> Ptr <git_repository> -> Ptr <git_tree> -> Ptr <git_tree> -> Ptr <git_diff_options> -> IO (CInt)
#ccall git_diff_tree_to_index , Ptr (Ptr <git_diff_list>) -> Ptr <git_repository> -> Ptr <git_tree> -> Ptr <git_index> -> Ptr <git_diff_options> -> IO (CInt)
#ccall git_diff_index_to_workdir , Ptr (Ptr <git_diff_list>) -> Ptr <git_repository> -> Ptr <git_index> -> Ptr <git_diff_options> -> IO (CInt)
#ccall git_diff_tree_to_workdir , Ptr (Ptr <git_diff_list>) -> Ptr <git_repository> -> Ptr <git_tree> -> Ptr <git_diff_options> -> IO (CInt)
#ccall git_diff_merge , Ptr <git_diff_list> -> Ptr <git_diff_list> -> IO (CInt)
#ccall git_diff_find_similar , Ptr <git_diff_list> -> Ptr <git_diff_find_options> -> IO (CInt)
#ccall git_diff_status_char , <git_delta_t> -> IO (CChar)
#ccall git_diff_num_deltas , Ptr <git_diff> -> IO (CSize)
#ccall git_diff_num_deltas_of_type , Ptr <git_diff> -> <git_delta_t> -> IO (CSize)
#ccall git_diff_get_delta , Ptr <git_diff> -> CSize -> IO (Ptr <git_diff_delta>)
#ccall git_diff_is_sorted_icase , Ptr <git_diff> -> IO (CInt)
#ccall git_diff_foreach , Ptr <git_diff> -> <git_diff_file_cb> -> <git_diff_binary_cb> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO (CInt)

#integral_t git_diff_format_t
#num GIT_DIFF_FORMAT_PATCH
#num GIT_DIFF_FORMAT_PATCH_HEADER
#num GIT_DIFF_FORMAT_RAW
#num GIT_DIFF_FORMAT_NAME_ONLY
#num GIT_DIFF_FORMAT_NAME_STATUS
  
#ccall git_diff_print , Ptr <git_diff> -> <git_diff_format_t> -> <git_diff_line_cb> -> Ptr () -> IO CInt

#ccall git_diff_blobs , Ptr <git_blob> -> CString -> Ptr <git_blob> -> CString -> Ptr <git_diff_options> -> <git_diff_file_cb> -> <git_diff_binary_cb> -> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO CInt
#ccall git_diff_blob_to_buffer , Ptr <git_blob> -> CString -> CString -> CSize -> CString -> Ptr <git_diff_options> -> <git_diff_file_cb> -> <git_diff_binary_cb> -> <git_diff_hunk_cb> -> <git_diff_line_cb> -> Ptr () -> IO CInt
