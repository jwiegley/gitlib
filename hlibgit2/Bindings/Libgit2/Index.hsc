{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Index where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef struct {
            git_time_t seconds; unsigned int nanoseconds;
        } git_index_time; -}
#starttype git_index_time
#field seconds , CLong
#field nanoseconds , CUInt
#stoptype
{- typedef struct git_index_entry {
            git_index_time ctime;
            git_index_time mtime;
            unsigned int dev;
            unsigned int ino;
            unsigned int mode;
            unsigned int uid;
            unsigned int gid;
            git_off_t file_size;
            git_oid oid;
            unsigned short flags;
            unsigned short flags_extended;
            char * path;
        } git_index_entry; -}
#starttype git_index_entry
#field ctime , <git_index_time>
#field mtime , <git_index_time>
#field dev , CUInt
#field ino , CUInt
#field mode , CUInt
#field uid , CUInt
#field gid , CUInt
#field file_size , CLong
#field oid , <git_oid>
#field flags , CUShort
#field flags_extended , CUShort
#field path , CString
#stoptype
{- typedef struct git_index_reuc_entry {
            unsigned int mode[3]; git_oid oid[3]; char * path;
        } git_index_reuc_entry; -}
#starttype git_index_reuc_entry
#array_field mode , CUInt
#array_field oid , <git_oid>
#field path , CString
#stoptype
{- enum {
    GIT_INDEXCAP_IGNORE_CASE = 1,
    GIT_INDEXCAP_NO_FILEMODE = 2,
    GIT_INDEXCAP_NO_SYMLINKS = 4,
    GIT_INDEXCAP_FROM_OWNER = ~0u
}; -}
#num GIT_INDEXCAP_IGNORE_CASE
#num GIT_INDEXCAP_NO_FILEMODE
#num GIT_INDEXCAP_NO_SYMLINKS
#num GIT_INDEXCAP_FROM_OWNER
#ccall git_index_open , Ptr (Ptr <git_index>) -> CString -> IO (CInt)
#ccall git_index_new , Ptr (Ptr <git_index>) -> IO (CInt)
#ccall git_index_free , Ptr <git_index> -> IO ()
#ccall git_index_owner , Ptr <git_index> -> IO (Ptr <git_repository>)
#ccall git_index_caps , Ptr <git_index> -> IO (CUInt)
#ccall git_index_set_caps , Ptr <git_index> -> CUInt -> IO (CInt)
#ccall git_index_read , Ptr <git_index> -> IO (CInt)
#ccall git_index_write , Ptr <git_index> -> IO (CInt)
#ccall git_index_read_tree , Ptr <git_index> -> Ptr <git_tree> -> IO (CInt)
#ccall git_index_write_tree , Ptr <git_oid> -> Ptr <git_index> -> IO (CInt)
#ccall git_index_write_tree_to , Ptr <git_oid> -> Ptr <git_index> -> Ptr <git_repository> -> IO (CInt)
#ccall git_index_entrycount , Ptr <git_index> -> IO (CSize)
#ccall git_index_clear , Ptr <git_index> -> IO ()
#ccall git_index_get_byindex , Ptr <git_index> -> CSize -> IO (Ptr <git_index_entry>)
#ccall git_index_get_bypath , Ptr <git_index> -> CString -> CInt -> IO (Ptr <git_index_entry>)
#ccall git_index_remove , Ptr <git_index> -> CString -> CInt -> IO (CInt)
#ccall git_index_remove_directory , Ptr <git_index> -> CString -> CInt -> IO (CInt)
#ccall git_index_add , Ptr <git_index> -> Ptr <git_index_entry> -> IO (CInt)
#ccall git_index_entry_stage , Ptr <git_index_entry> -> IO (CInt)
#ccall git_index_add_bypath , Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_remove_bypath , Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_find , Ptr CSize -> Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_conflict_add , Ptr <git_index> -> Ptr <git_index_entry> -> Ptr <git_index_entry> -> Ptr <git_index_entry> -> IO (CInt)
#ccall git_index_conflict_get , Ptr (Ptr <git_index_entry>) -> Ptr (Ptr <git_index_entry>) -> Ptr (Ptr <git_index_entry>) -> Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_conflict_remove , Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_conflict_cleanup , Ptr <git_index> -> IO ()
#ccall git_index_has_conflicts , Ptr <git_index> -> IO (CInt)
#ccall git_index_reuc_entrycount , Ptr <git_index> -> IO (CUInt)
#ccall git_index_reuc_find , Ptr CSize -> Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_reuc_get_bypath , Ptr <git_index> -> CString -> IO (Ptr <git_index_reuc_entry>)
#ccall git_index_reuc_get_byindex , Ptr <git_index> -> CSize -> IO (Ptr <git_index_reuc_entry>)
#ccall git_index_reuc_add , Ptr <git_index> -> CString -> CInt -> Ptr <git_oid> -> CInt -> Ptr <git_oid> -> CInt -> Ptr <git_oid> -> IO (CInt)
#ccall git_index_reuc_remove , Ptr <git_index> -> CSize -> IO (CInt)
#ccall git_index_reuc_clear , Ptr <git_index> -> IO ()
