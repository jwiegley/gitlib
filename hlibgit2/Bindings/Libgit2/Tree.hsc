{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/tree.h>
module Bindings.Libgit2.Tree where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Object
#ccall git_tree_lookup , Ptr (Ptr <struct git_tree>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> IO CInt
#ccall git_tree_lookup_prefix , Ptr (Ptr <struct git_tree>) -> Ptr <struct git_repository> -> Ptr <struct git_oid> -> CSize -> IO CInt
#ccall git_tree_free , Ptr <struct git_tree> -> IO ()
#ccall git_tree_id , Ptr <struct git_tree> -> IO (Ptr <struct git_oid>)
#ccall git_tree_owner , Ptr <struct git_tree> -> IO (Ptr <struct git_repository>)
#ccall git_tree_entrycount , Ptr <struct git_tree> -> IO CSize
#ccall git_tree_entry_byname , Ptr <struct git_tree> -> CString -> IO (Ptr <struct git_tree_entry>)
#ccall git_tree_entry_byindex , Ptr <struct git_tree> -> CSize -> IO (Ptr <struct git_tree_entry>)
#ccall git_tree_entry_byid , Ptr <struct git_tree> -> Ptr <struct git_oid> -> IO (Ptr <struct git_tree_entry>)
#ccall git_tree_entry_bypath , Ptr (Ptr <struct git_tree_entry>) -> Ptr <struct git_tree> -> CString -> IO CInt
#ccall git_tree_entry_dup , Ptr (Ptr <struct git_tree_entry>) -> Ptr <struct git_tree_entry> -> IO CInt
#ccall git_tree_entry_free , Ptr <struct git_tree_entry> -> IO ()
#ccall git_tree_entry_name , Ptr <struct git_tree_entry> -> IO CString
#ccall git_tree_entry_id , Ptr <struct git_tree_entry> -> IO (Ptr <struct git_oid>)
#ccall git_tree_entry_type , Ptr <struct git_tree_entry> -> IO <git_object_t>
#ccall git_tree_entry_filemode , Ptr <struct git_tree_entry> -> IO <git_filemode_t>
#ccall git_tree_entry_filemode_raw , Ptr <struct git_tree_entry> -> IO <git_filemode_t>
#ccall git_tree_entry_cmp , Ptr <struct git_tree_entry> -> Ptr <struct git_tree_entry> -> IO CInt
#ccall git_tree_entry_to_object , Ptr (Ptr <struct git_object>) -> Ptr <struct git_repository> -> Ptr <struct git_tree_entry> -> IO CInt
#ccall git_treebuilder_new , Ptr (Ptr <struct git_treebuilder>) -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> IO CInt
#ccall git_treebuilder_clear , Ptr <struct git_treebuilder> -> IO CInt
#ccall git_treebuilder_entrycount , Ptr <struct git_treebuilder> -> IO CSize
#ccall git_treebuilder_free , Ptr <struct git_treebuilder> -> IO ()
#ccall git_treebuilder_get , Ptr <struct git_treebuilder> -> CString -> IO (Ptr <struct git_tree_entry>)
#ccall git_treebuilder_insert , Ptr (Ptr <struct git_tree_entry>) -> Ptr <struct git_treebuilder> -> CString -> Ptr <struct git_oid> -> <git_filemode_t> -> IO CInt
#ccall git_treebuilder_remove , Ptr <struct git_treebuilder> -> CString -> IO CInt
#callback git_treebuilder_filter_cb , Ptr <struct git_tree_entry> -> Ptr () -> IO CInt
#ccall git_treebuilder_filter , Ptr <struct git_treebuilder> -> <git_treebuilder_filter_cb> -> Ptr () -> IO CInt
#ccall git_treebuilder_write , Ptr <struct git_oid> -> Ptr <struct git_treebuilder> -> IO CInt
#callback git_treewalk_cb , CString -> Ptr <struct git_tree_entry> -> Ptr () -> IO CInt
{- typedef enum {
            GIT_TREEWALK_PRE = 0, GIT_TREEWALK_POST = 1
        } git_treewalk_mode; -}
#integral_t git_treewalk_mode
#num GIT_TREEWALK_PRE
#num GIT_TREEWALK_POST
#ccall git_tree_walk , Ptr <struct git_tree> -> <git_treewalk_mode> -> <git_treewalk_cb> -> Ptr () -> IO CInt
#ccall git_tree_dup , Ptr (Ptr <struct git_tree>) -> Ptr <struct git_tree> -> IO CInt
{- typedef enum {
            GIT_TREE_UPDATE_UPSERT, GIT_TREE_UPDATE_REMOVE
        } git_tree_update_t; -}
#integral_t git_tree_update_t
#num GIT_TREE_UPDATE_UPSERT
#num GIT_TREE_UPDATE_REMOVE
{- typedef struct {
            git_tree_update_t action;
            git_oid id;
            git_filemode_t filemode;
            const char * path;
        } git_tree_update; -}
#starttype git_tree_update
#field action , <git_tree_update_t>
#field id , <struct git_oid>
#field filemode , <git_filemode_t>
#field path , CString
#stoptype
#ccall git_tree_create_updated , Ptr <struct git_oid> -> Ptr <struct git_repository> -> Ptr <struct git_tree> -> CSize -> Ptr <git_tree_update> -> IO CInt
