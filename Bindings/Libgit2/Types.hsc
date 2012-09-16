#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Types where
#strict_import

import Bindings.Libgit2.Common
{- typedef int64_t git_off_t; -}
#synonym_t git_off_t , CLong
{- typedef int64_t git_time_t; -}
#synonym_t git_time_t , CLong
{- typedef enum {
            GIT_OBJ_ANY = -2,
            GIT_OBJ_BAD = -1,
            GIT_OBJ__EXT1 = 0,
            GIT_OBJ_COMMIT = 1,
            GIT_OBJ_TREE = 2,
            GIT_OBJ_BLOB = 3,
            GIT_OBJ_TAG = 4,
            GIT_OBJ__EXT2 = 5,
            GIT_OBJ_OFS_DELTA = 6,
            GIT_OBJ_REF_DELTA = 7
        } git_otype; -}
#integral_t git_otype
#num GIT_OBJ_ANY
#num GIT_OBJ_BAD
#num GIT_OBJ__EXT1
#num GIT_OBJ_COMMIT
#num GIT_OBJ_TREE
#num GIT_OBJ_BLOB
#num GIT_OBJ_TAG
#num GIT_OBJ__EXT2
#num GIT_OBJ_OFS_DELTA
#num GIT_OBJ_REF_DELTA
{- typedef struct git_odb git_odb; -}
#opaque_t git_odb
-- {- typedef struct git_odb_backend git_odb_backend; -}
-- #opaque_t git_odb_backend
{- typedef struct git_odb_object git_odb_object; -}
#opaque_t git_odb_object
-- {- typedef struct git_odb_stream git_odb_stream; -}
-- #opaque_t git_odb_stream
{- typedef struct git_repository git_repository; -}
#opaque_t git_repository
{- typedef struct git_object git_object; -}
#opaque_t git_object
{- typedef struct git_revwalk git_revwalk; -}
#opaque_t git_revwalk
{- typedef struct git_tag git_tag; -}
#opaque_t git_tag
{- typedef struct git_blob git_blob; -}
#opaque_t git_blob
{- typedef struct git_commit git_commit; -}
#opaque_t git_commit
{- typedef struct git_tree_entry git_tree_entry; -}
#opaque_t git_tree_entry
{- typedef struct git_tree git_tree; -}
#opaque_t git_tree
{- typedef struct git_treebuilder git_treebuilder; -}
#opaque_t git_treebuilder
{- typedef struct git_index git_index; -}
#opaque_t git_index
{- typedef struct git_config git_config; -}
#opaque_t git_config
-- {- typedef struct git_config_file git_config_file; -}
-- #opaque_t git_config_file
{- typedef struct git_reflog_entry git_reflog_entry; -}
#opaque_t git_reflog_entry
{- typedef struct git_reflog git_reflog; -}
#opaque_t git_reflog
{- typedef struct git_note git_note; -}
#opaque_t git_note
{- typedef struct git_time {
            git_time_t time; int offset;
        } git_time; -}
#starttype git_time
#field time , CLong
#field offset , CInt
#stoptype
{- typedef struct git_signature {
            char * name; char * email; git_time when;
        } git_signature; -}
#starttype git_signature
#field name , CString
#field email , CString
#field when , <git_time>
#stoptype
{- typedef struct git_reference git_reference; -}
#opaque_t git_reference
{- typedef enum {
            GIT_REF_INVALID = 0,
            GIT_REF_OID = 1,
            GIT_REF_SYMBOLIC = 2,
            GIT_REF_PACKED = 4,
            GIT_REF_HAS_PEEL = 8,
            GIT_REF_LISTALL = GIT_REF_OID | GIT_REF_SYMBOLIC | GIT_REF_PACKED
        } git_ref_t; -}
#integral_t git_ref_t
#num GIT_REF_INVALID
#num GIT_REF_OID
#num GIT_REF_SYMBOLIC
#num GIT_REF_PACKED
#num GIT_REF_HAS_PEEL
#num GIT_REF_LISTALL
{- typedef enum {
            GIT_BRANCH_LOCAL = 1, GIT_BRANCH_REMOTE = 2
        } git_branch_t; -}
#integral_t git_branch_t
#num GIT_BRANCH_LOCAL
#num GIT_BRANCH_REMOTE
{- typedef struct git_refspec git_refspec; -}
#opaque_t git_refspec
{- typedef struct git_remote git_remote; -}
#opaque_t git_remote
-- {- typedef struct git_remote_head git_remote_head; -}
-- #opaque_t git_remote_head
