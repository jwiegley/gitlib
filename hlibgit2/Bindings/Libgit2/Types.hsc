{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/types.h>
module Bindings.Libgit2.Types where
import Foreign.Ptr
--import Bindings.Libgit2.Oid
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Buffer
{- typedef int64_t git_off_t; -}
#synonym_t git_off_t , CLong
{- typedef int64_t git_time_t; -}
#synonym_t git_time_t , CLong
{- typedef uint64_t git_object_size_t; -}
#synonym_t git_object_size_t , CULong
{- typedef enum {
            GIT_OBJECT_ANY = -2,
            GIT_OBJECT_INVALID = -1,
            GIT_OBJECT_COMMIT = 1,
            GIT_OBJECT_TREE = 2,
            GIT_OBJECT_BLOB = 3,
            GIT_OBJECT_TAG = 4,
            GIT_OBJECT_OFS_DELTA = 6,
            GIT_OBJECT_REF_DELTA = 7
        } git_object_t; -}
#integral_t git_object_t
#num GIT_OBJECT_ANY
#num GIT_OBJECT_INVALID
#num GIT_OBJECT_COMMIT
#num GIT_OBJECT_TREE
#num GIT_OBJECT_BLOB
#num GIT_OBJECT_TAG
#num GIT_OBJECT_OFS_DELTA
#num GIT_OBJECT_REF_DELTA
{- typedef struct git_odb git_odb; -}
#opaque_t struct git_odb
{- typedef struct git_odb_backend git_odb_backend; -}
#opaque_t struct git_odb_backend
{- typedef struct git_odb_object git_odb_object; -}
#opaque_t struct git_odb_object
{- typedef struct git_midx_writer git_midx_writer; -}
#opaque_t struct git_midx_writer
{- typedef struct git_refdb git_refdb; -}
#opaque_t struct git_refdb
{- typedef struct git_refdb_backend git_refdb_backend; -}
#opaque_t struct git_refdb_backend
{- typedef struct git_commit_graph git_commit_graph; -}
#opaque_t struct git_commit_graph
{- typedef struct git_commit_graph_writer git_commit_graph_writer; -}
#opaque_t struct git_commit_graph_writer
{- typedef struct git_repository git_repository; -}
#opaque_t struct git_repository
{- typedef struct git_worktree git_worktree; -}
#opaque_t struct git_worktree
{- typedef struct git_object git_object; -}
#opaque_t struct git_object
{- typedef struct git_revwalk git_revwalk; -}
#opaque_t struct git_revwalk
{- typedef struct git_tag git_tag; -}
#opaque_t struct git_tag
{- typedef struct git_blob git_blob; -}
#opaque_t struct git_blob
{- typedef struct git_commit git_commit; -}
#opaque_t struct git_commit
{- typedef struct git_tree_entry git_tree_entry; -}
#opaque_t struct git_tree_entry
{- typedef struct git_tree git_tree; -}
#opaque_t struct git_tree
{- typedef struct git_treebuilder git_treebuilder; -}
#opaque_t struct git_treebuilder
{- typedef struct git_index git_index; -}
#opaque_t struct git_index
{- typedef struct git_index_iterator git_index_iterator; -}
#opaque_t struct git_index_iterator
{- typedef struct git_index_conflict_iterator git_index_conflict_iterator; -}
#opaque_t struct git_index_conflict_iterator
{- typedef struct git_config git_config; -}
#opaque_t struct git_config
{- typedef struct git_config_backend git_config_backend; -}
#opaque_t struct git_config_backend
{- typedef struct git_reflog_entry git_reflog_entry; -}
#opaque_t struct git_reflog_entry
{- typedef struct git_reflog git_reflog; -}
#opaque_t struct git_reflog
{- typedef struct git_note git_note; -}
#opaque_t struct git_note
{- typedef struct git_packbuilder git_packbuilder; -}
#opaque_t struct git_packbuilder
{- typedef struct git_time {
            git_time_t time; int offset; char sign;
        } git_time; -}
#starttype struct git_time
#field time , CLong
#field offset , CInt
#field sign , CChar
#stoptype
{- typedef struct git_signature {
            char * name; char * email; git_time when;
        } git_signature; -}
#starttype struct git_signature
#field name , CString
#field email , CString
#field when , <struct git_time>
#stoptype
{- typedef struct git_reference git_reference; -}
#opaque_t struct git_reference
{- typedef struct git_reference_iterator git_reference_iterator; -}
#opaque_t struct git_reference_iterator
{- typedef struct git_transaction git_transaction; -}
#opaque_t struct git_transaction
{- typedef struct git_annotated_commit git_annotated_commit; -}
#opaque_t struct git_annotated_commit
{- typedef struct git_status_list git_status_list; -}
#opaque_t struct git_status_list
{- typedef struct git_rebase git_rebase; -}
#opaque_t struct git_rebase
{- typedef enum {
            GIT_REFERENCE_INVALID = 0,
            GIT_REFERENCE_DIRECT = 1,
            GIT_REFERENCE_SYMBOLIC = 2,
            GIT_REFERENCE_ALL = GIT_REFERENCE_DIRECT | GIT_REFERENCE_SYMBOLIC
        } git_reference_t; -}
#integral_t git_reference_t
#num GIT_REFERENCE_INVALID
#num GIT_REFERENCE_DIRECT
#num GIT_REFERENCE_SYMBOLIC
#num GIT_REFERENCE_ALL
{- typedef enum {
            GIT_BRANCH_LOCAL = 1,
            GIT_BRANCH_REMOTE = 2,
            GIT_BRANCH_ALL = GIT_BRANCH_LOCAL | GIT_BRANCH_REMOTE
        } git_branch_t; -}
#integral_t git_branch_t
#num GIT_BRANCH_LOCAL
#num GIT_BRANCH_REMOTE
#num GIT_BRANCH_ALL
{- typedef enum {
            GIT_FILEMODE_UNREADABLE = 00,
            GIT_FILEMODE_TREE = 040000,
            GIT_FILEMODE_BLOB = 0100644,
            GIT_FILEMODE_BLOB_EXECUTABLE = 0100755,
            GIT_FILEMODE_LINK = 0120000,
            GIT_FILEMODE_COMMIT = 0160000
        } git_filemode_t; -}
#integral_t git_filemode_t
#num GIT_FILEMODE_UNREADABLE
#num GIT_FILEMODE_TREE
#num GIT_FILEMODE_BLOB
#num GIT_FILEMODE_BLOB_EXECUTABLE
#num GIT_FILEMODE_LINK
#num GIT_FILEMODE_COMMIT
{- typedef struct git_refspec git_refspec; -}
#opaque_t struct git_refspec
{- typedef struct git_remote git_remote; -}
#opaque_t struct git_remote
{- typedef struct git_transport git_transport; -}
#opaque_t struct git_transport
{- typedef struct git_push git_push; -}
#opaque_t struct git_push
{- typedef struct git_submodule git_submodule; -}
#opaque_t struct git_submodule
{- typedef enum {
            GIT_SUBMODULE_UPDATE_CHECKOUT = 1,
            GIT_SUBMODULE_UPDATE_REBASE = 2,
            GIT_SUBMODULE_UPDATE_MERGE = 3,
            GIT_SUBMODULE_UPDATE_NONE = 4,
            GIT_SUBMODULE_UPDATE_DEFAULT = 0
        } git_submodule_update_t; -}
#integral_t git_submodule_update_t
#num GIT_SUBMODULE_UPDATE_CHECKOUT
#num GIT_SUBMODULE_UPDATE_REBASE
#num GIT_SUBMODULE_UPDATE_MERGE
#num GIT_SUBMODULE_UPDATE_NONE
#num GIT_SUBMODULE_UPDATE_DEFAULT
{- typedef enum {
            GIT_SUBMODULE_IGNORE_UNSPECIFIED = -1,
            GIT_SUBMODULE_IGNORE_NONE = 1,
            GIT_SUBMODULE_IGNORE_UNTRACKED = 2,
            GIT_SUBMODULE_IGNORE_DIRTY = 3,
            GIT_SUBMODULE_IGNORE_ALL = 4
        } git_submodule_ignore_t; -}
#integral_t git_submodule_ignore_t
#num GIT_SUBMODULE_IGNORE_UNSPECIFIED
#num GIT_SUBMODULE_IGNORE_NONE
#num GIT_SUBMODULE_IGNORE_UNTRACKED
#num GIT_SUBMODULE_IGNORE_DIRTY
#num GIT_SUBMODULE_IGNORE_ALL
{- typedef enum {
            GIT_SUBMODULE_RECURSE_NO = 0,
            GIT_SUBMODULE_RECURSE_YES = 1,
            GIT_SUBMODULE_RECURSE_ONDEMAND = 2
        } git_submodule_recurse_t; -}
#integral_t git_submodule_recurse_t
#num GIT_SUBMODULE_RECURSE_NO
#num GIT_SUBMODULE_RECURSE_YES
#num GIT_SUBMODULE_RECURSE_ONDEMAND
{- typedef struct git_writestream git_writestream; -}
#opaque_t struct git_writestream
{- struct git_writestream {
    int (* write)(git_writestream * stream,
                  const char * buffer,
                  size_t len);
    int (* close)(git_writestream * stream);
    void (* free)(git_writestream * stream);
}; -}
{- typedef struct git_mailmap git_mailmap; -}
#opaque_t struct git_mailmap
