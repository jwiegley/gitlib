{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
#include <git2/tree.h>
module Bindings.Libgit2.Tree where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
#cinline git_tree_lookup , Ptr (Ptr <git_tree>) -> Ptr <git_repository> -> Ptr <git_oid> -> IO (CInt)
#cinline git_tree_lookup_prefix , Ptr (Ptr <git_tree>) -> Ptr <git_repository> -> Ptr <git_oid> -> CSize -> IO (CInt)
#cinline git_tree_free , Ptr <git_tree> -> IO ()
#ccall git_tree_id , Ptr <git_tree> -> IO (Ptr <git_oid>)
#ccall git_tree_owner , Ptr <git_tree> -> IO (Ptr <git_repository>)
#ccall git_tree_entrycount , Ptr <git_tree> -> IO (CSize)
#ccall git_tree_entry_byname , Ptr <git_tree> -> CString -> IO (Ptr <git_tree_entry>)
#ccall git_tree_entry_byindex , Ptr <git_tree> -> CSize -> IO (Ptr <git_tree_entry>)
#ccall git_tree_entry_byoid , Ptr <git_tree> -> Ptr <git_oid> -> IO (Ptr <git_tree_entry>)
#ccall git_tree_entry_bypath , Ptr (Ptr <git_tree_entry>) -> Ptr <git_tree> -> CString -> IO (CInt)
#ccall git_tree_entry_dup , Ptr <git_tree_entry> -> IO (Ptr <git_tree_entry>)
#ccall git_tree_entry_free , Ptr <git_tree_entry> -> IO ()
#ccall git_tree_entry_name , Ptr <git_tree_entry> -> IO (CString)
#ccall git_tree_entry_id , Ptr <git_tree_entry> -> IO (Ptr <git_oid>)
#ccall git_tree_entry_type , Ptr <git_tree_entry> -> IO (<git_otype>)
#ccall git_tree_entry_filemode , Ptr <git_tree_entry> -> IO (<git_filemode_t>)
#ccall git_tree_entry_cmp , Ptr <git_tree_entry> -> Ptr <git_tree_entry> -> IO (CInt)
#ccall git_tree_entry_to_object , Ptr (Ptr <git_object>) -> Ptr <git_repository> -> Ptr <git_tree_entry> -> IO (CInt)
#ccall git_treebuilder_create , Ptr (Ptr <git_treebuilder>) -> Ptr <git_tree> -> IO (CInt)
#ccall git_treebuilder_clear , Ptr <git_treebuilder> -> IO ()
#ccall git_treebuilder_entrycount , Ptr <git_treebuilder> -> IO (CUInt)
#ccall git_treebuilder_free , Ptr <git_treebuilder> -> IO ()
#ccall git_treebuilder_get , Ptr <git_treebuilder> -> CString -> IO (Ptr <git_tree_entry>)
#ccall git_treebuilder_insert , Ptr (Ptr <git_tree_entry>) -> Ptr <git_treebuilder> -> CString -> Ptr <git_oid> -> <git_filemode_t> -> IO (CInt)
#ccall git_treebuilder_remove , Ptr <git_treebuilder> -> CString -> IO (CInt)
{- typedef int (* git_treebuilder_filter_cb)(const git_tree_entry * entry,
                                          void * payload); -}
#callback git_treebuilder_filter_cb , Ptr (<git_tree_entry>) -> Ptr () -> IO CInt
#ccall git_treebuilder_filter , Ptr <git_treebuilder> -> <git_treebuilder_filter_cb> -> Ptr () -> IO ()
#ccall git_treebuilder_write , Ptr <git_oid> -> Ptr <git_repository> -> Ptr <git_treebuilder> -> IO (CInt)
{- typedef int (* git_treewalk_cb)(const char * root,
                                const git_tree_entry * entry,
                                void * payload); -}
#callback git_treewalk_cb , CString -> Ptr (<git_tree_entry>) -> Ptr () -> IO CInt
{- typedef enum {
            GIT_TREEWALK_PRE = 0, GIT_TREEWALK_POST = 1
        } git_treewalk_mode; -}
#integral_t git_treewalk_mode
#num GIT_TREEWALK_PRE
#num GIT_TREEWALK_POST
#ccall git_tree_walk , Ptr <git_tree> -> <git_treewalk_mode> -> <git_treewalk_cb> -> Ptr () -> IO (CInt)
