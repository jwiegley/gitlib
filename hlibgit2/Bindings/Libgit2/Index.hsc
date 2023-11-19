{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/index.h>
module Bindings.Libgit2.Index where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Strarray
{- typedef struct {
            int32_t seconds; uint32_t nanoseconds;
        } git_index_time; -}
#starttype git_index_time
#field seconds , CInt
#field nanoseconds , CUInt
#stoptype
{- typedef struct git_index_entry {
            git_index_time ctime;
            git_index_time mtime;
            uint32_t dev;
            uint32_t ino;
            uint32_t mode;
            uint32_t uid;
            uint32_t gid;
            uint32_t file_size;
            git_oid id;
            uint16_t flags;
            uint16_t flags_extended;
            const char * path;
        } git_index_entry; -}
#starttype struct git_index_entry
#field ctime , <git_index_time>
#field mtime , <git_index_time>
#field dev , CUInt
#field ino , CUInt
#field mode , CUInt
#field uid , CUInt
#field gid , CUInt
#field file_size , CUInt
#field id , <struct git_oid>
#field flags , CUInt
#field flags_extended , CUInt
#field path , CString
#stoptype
{- typedef enum {
            GIT_INDEX_ENTRY_EXTENDED = 0x4000, GIT_INDEX_ENTRY_VALID = 0x8000
        } git_index_entry_flag_t; -}
#integral_t git_index_entry_flag_t
#num GIT_INDEX_ENTRY_EXTENDED
#num GIT_INDEX_ENTRY_VALID
{- typedef enum {
            GIT_INDEX_ENTRY_INTENT_TO_ADD = 1 << 13,
            GIT_INDEX_ENTRY_SKIP_WORKTREE = 1 << 14,
            GIT_INDEX_ENTRY_EXTENDED_FLAGS = GIT_INDEX_ENTRY_INTENT_TO_ADD | GIT_INDEX_ENTRY_SKIP_WORKTREE,
            GIT_INDEX_ENTRY_UPTODATE = 1 << 2
        } git_index_entry_extended_flag_t; -}
#integral_t git_index_entry_extended_flag_t
#num GIT_INDEX_ENTRY_INTENT_TO_ADD
#num GIT_INDEX_ENTRY_SKIP_WORKTREE
#num GIT_INDEX_ENTRY_EXTENDED_FLAGS
#num GIT_INDEX_ENTRY_UPTODATE
{- typedef enum {
            GIT_INDEX_CAPABILITY_IGNORE_CASE = 1,
            GIT_INDEX_CAPABILITY_NO_FILEMODE = 2,
            GIT_INDEX_CAPABILITY_NO_SYMLINKS = 4,
            GIT_INDEX_CAPABILITY_FROM_OWNER = -1
        } git_index_capability_t; -}
#integral_t git_index_capability_t
#num GIT_INDEX_CAPABILITY_IGNORE_CASE
#num GIT_INDEX_CAPABILITY_NO_FILEMODE
#num GIT_INDEX_CAPABILITY_NO_SYMLINKS
#num GIT_INDEX_CAPABILITY_FROM_OWNER
#callback git_index_matched_path_cb , CString -> CString -> Ptr () -> IO CInt
{- typedef enum {
            GIT_INDEX_ADD_DEFAULT = 0,
            GIT_INDEX_ADD_FORCE = 1u << 0,
            GIT_INDEX_ADD_DISABLE_PATHSPEC_MATCH = 1u << 1,
            GIT_INDEX_ADD_CHECK_PATHSPEC = 1u << 2
        } git_index_add_option_t; -}
#integral_t git_index_add_option_t
#num GIT_INDEX_ADD_DEFAULT
#num GIT_INDEX_ADD_FORCE
#num GIT_INDEX_ADD_DISABLE_PATHSPEC_MATCH
#num GIT_INDEX_ADD_CHECK_PATHSPEC
{- typedef enum {
            GIT_INDEX_STAGE_ANY = -1,
            GIT_INDEX_STAGE_NORMAL = 0,
            GIT_INDEX_STAGE_ANCESTOR = 1,
            GIT_INDEX_STAGE_OURS = 2,
            GIT_INDEX_STAGE_THEIRS = 3
        } git_index_stage_t; -}
#integral_t git_index_stage_t
#num GIT_INDEX_STAGE_ANY
#num GIT_INDEX_STAGE_NORMAL
#num GIT_INDEX_STAGE_ANCESTOR
#num GIT_INDEX_STAGE_OURS
#num GIT_INDEX_STAGE_THEIRS
#ccall git_index_open , Ptr (Ptr <struct git_index>) -> CString -> IO CInt
#ccall git_index_new , Ptr (Ptr <struct git_index>) -> IO CInt
#ccall git_index_free , Ptr <struct git_index> -> IO ()
#ccall git_index_owner , Ptr <struct git_index> -> IO (Ptr <struct git_repository>)
#ccall git_index_caps , Ptr <struct git_index> -> IO CInt
#ccall git_index_set_caps , Ptr <struct git_index> -> CInt -> IO CInt
#ccall git_index_version , Ptr <struct git_index> -> IO CUInt
#ccall git_index_set_version , Ptr <struct git_index> -> CUInt -> IO CInt
#ccall git_index_read , Ptr <struct git_index> -> CInt -> IO CInt
#ccall git_index_write , Ptr <struct git_index> -> IO CInt
#ccall git_index_path , Ptr <struct git_index> -> IO CString
#ccall git_index_checksum , Ptr <struct git_index> -> IO (Ptr <struct git_oid>)
#ccall git_index_read_tree , Ptr <struct git_index> -> Ptr <struct git_tree> -> IO CInt
#ccall git_index_write_tree , Ptr <struct git_oid> -> Ptr <struct git_index> -> IO CInt
#ccall git_index_write_tree_to , Ptr <struct git_oid> -> Ptr <struct git_index> -> Ptr <struct git_repository> -> IO CInt
#ccall git_index_entrycount , Ptr <struct git_index> -> IO CSize
#ccall git_index_clear , Ptr <struct git_index> -> IO CInt
#ccall git_index_get_byindex , Ptr <struct git_index> -> CSize -> IO (Ptr <struct git_index_entry>)
#ccall git_index_get_bypath , Ptr <struct git_index> -> CString -> CInt -> IO (Ptr <struct git_index_entry>)
#ccall git_index_remove , Ptr <struct git_index> -> CString -> CInt -> IO CInt
#ccall git_index_remove_directory , Ptr <struct git_index> -> CString -> CInt -> IO CInt
#ccall git_index_add , Ptr <struct git_index> -> Ptr <struct git_index_entry> -> IO CInt
#ccall git_index_entry_stage , Ptr <struct git_index_entry> -> IO CInt
#ccall git_index_entry_is_conflict , Ptr <struct git_index_entry> -> IO CInt
#ccall git_index_iterator_new , Ptr (Ptr <struct git_index_iterator>) -> Ptr <struct git_index> -> IO CInt
#ccall git_index_iterator_next , Ptr (Ptr <struct git_index_entry>) -> Ptr <struct git_index_iterator> -> IO CInt
#ccall git_index_iterator_free , Ptr <struct git_index_iterator> -> IO ()
#ccall git_index_add_bypath , Ptr <struct git_index> -> CString -> IO CInt
#ccall git_index_add_from_buffer , Ptr <struct git_index> -> Ptr <struct git_index_entry> -> Ptr () -> CSize -> IO CInt
#ccall git_index_remove_bypath , Ptr <struct git_index> -> CString -> IO CInt
#ccall git_index_add_all , Ptr <struct git_index> -> Ptr <struct git_strarray> -> CUInt -> <git_index_matched_path_cb> -> Ptr () -> IO CInt
#ccall git_index_remove_all , Ptr <struct git_index> -> Ptr <struct git_strarray> -> <git_index_matched_path_cb> -> Ptr () -> IO CInt
#ccall git_index_update_all , Ptr <struct git_index> -> Ptr <struct git_strarray> -> <git_index_matched_path_cb> -> Ptr () -> IO CInt
#ccall git_index_find , Ptr CSize -> Ptr <struct git_index> -> CString -> IO CInt
#ccall git_index_find_prefix , Ptr CSize -> Ptr <struct git_index> -> CString -> IO CInt
#ccall git_index_conflict_add , Ptr <struct git_index> -> Ptr <struct git_index_entry> -> Ptr <struct git_index_entry> -> Ptr <struct git_index_entry> -> IO CInt
#ccall git_index_conflict_get , Ptr (Ptr <struct git_index_entry>) -> Ptr (Ptr <struct git_index_entry>) -> Ptr (Ptr <struct git_index_entry>) -> Ptr <struct git_index> -> CString -> IO CInt
#ccall git_index_conflict_remove , Ptr <struct git_index> -> CString -> IO CInt
#ccall git_index_conflict_cleanup , Ptr <struct git_index> -> IO CInt
#ccall git_index_has_conflicts , Ptr <struct git_index> -> IO CInt
#ccall git_index_conflict_iterator_new , Ptr (Ptr <struct git_index_conflict_iterator>) -> Ptr <struct git_index> -> IO CInt
#ccall git_index_conflict_next , Ptr (Ptr <struct git_index_entry>) -> Ptr (Ptr <struct git_index_entry>) -> Ptr (Ptr <struct git_index_entry>) -> Ptr <struct git_index_conflict_iterator> -> IO CInt
#ccall git_index_conflict_iterator_free , Ptr <struct git_index_conflict_iterator> -> IO ()
