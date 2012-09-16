#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Index where
#strict_import

import Bindings.Libgit2.Common
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
{- typedef struct git_index_entry_unmerged {
            unsigned int mode[3]; git_oid oid[3]; char * path;
        } git_index_entry_unmerged; -}
#starttype git_index_entry_unmerged
#array_field mode , CUInt
#array_field oid , <git_oid>
#field path , CString
#stoptype
#ccall git_index_open , Ptr (Ptr <git_index>) -> CString -> IO (CInt)
#ccall git_index_clear , Ptr <git_index> -> IO ()
#ccall git_index_free , Ptr <git_index> -> IO ()
#ccall git_index_read , Ptr <git_index> -> IO (CInt)
#ccall git_index_write , Ptr <git_index> -> IO (CInt)
#ccall git_index_find , Ptr <git_index> -> CString -> IO (CInt)
#ccall git_index_uniq , Ptr <git_index> -> IO ()
#ccall git_index_add , Ptr <git_index> -> CString -> CInt -> IO (CInt)
#ccall git_index_add2 , Ptr <git_index> -> Ptr <git_index_entry> -> IO (CInt)
#ccall git_index_append , Ptr <git_index> -> CString -> CInt -> IO (CInt)
#ccall git_index_append2 , Ptr <git_index> -> Ptr <git_index_entry> -> IO (CInt)
#ccall git_index_remove , Ptr <git_index> -> CInt -> IO (CInt)
#ccall git_index_get , Ptr <git_index> -> CUInt -> IO (Ptr <git_index_entry>)
#ccall git_index_entrycount , Ptr <git_index> -> IO (CUInt)
#ccall git_index_entrycount_unmerged , Ptr <git_index> -> IO (CUInt)
#ccall git_index_get_unmerged_bypath , Ptr <git_index> -> CString -> IO (Ptr <git_index_entry_unmerged>)
#ccall git_index_get_unmerged_byindex , Ptr <git_index> -> CUInt -> IO (Ptr <git_index_entry_unmerged>)
#ccall git_index_entry_stage , Ptr <git_index_entry> -> IO (CInt)
#ccall git_index_read_tree , Ptr <git_index> -> Ptr <git_tree> -> IO (CInt)
