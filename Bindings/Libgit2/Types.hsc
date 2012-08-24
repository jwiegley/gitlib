
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Types where
#strict_import


#integral_t git_off_t
{- typedef enum {
	GIT_OBJ_ANY = -2,		/**< Object can be any of the following */
	GIT_OBJ_BAD = -1,       /**< Object is invalid. */
	GIT_OBJ__EXT1 = 0,      /**< Reserved for future use. */
	GIT_OBJ_COMMIT = 1,     /**< A commit object. */
	GIT_OBJ_TREE = 2,       /**< A tree (directory listing) object. */
	GIT_OBJ_BLOB = 3,       /**< A file revision object. */
	GIT_OBJ_TAG = 4,        /**< An annotated tag object. */
	GIT_OBJ__EXT2 = 5,      /**< Reserved for future use. */
	GIT_OBJ_OFS_DELTA = 6,  /**< A delta, base is given by an offset. */
	GIT_OBJ_REF_DELTA = 7,  /**< A delta, base is given by object id. */
} git_otype; -}
#integral_t git_otype
#num    GIT_OBJ_ANY
#num    GIT_OBJ_BAD
#num    GIT_OBJ__EXT1
#num    GIT_OBJ_COMMIT
#num    GIT_OBJ_TREE
#num    GIT_OBJ_BLOB
#num    GIT_OBJ_TAG
#num    GIT_OBJ__EXT2
#num    GIT_OBJ_OFS_DELTA
#num    GIT_OBJ_REF_DELTA
{- typedef enum {
	GIT_REF_INVALID = 0, /** Invalid reference */
	GIT_REF_OID = 1, /** A reference which points at an object id */
	GIT_REF_SYMBOLIC = 2, /** A reference which points at another reference */
	GIT_REF_PACKED = 4,
	GIT_REF_HAS_PEEL = 8,
	GIT_REF_LISTALL = GIT_REF_OID|GIT_REF_SYMBOLIC|GIT_REF_PACKED,
} git_ref_t; -}
#integral_t git_ref_t
#num    GIT_REF_INVALID
#num    GIT_REF_OID
#num    GIT_REF_SYMBOLIC
#num    GIT_REF_PACKED
#num    GIT_REF_HAS_PEEL
#num    GIT_REF_LISTALL
#opaque_t git_odb
#opaque_t git_odb_backend
#opaque_t git_odb_object
#opaque_t git_odb_stream
#opaque_t git_repository
#opaque_t git_object
#opaque_t git_revwalk
#opaque_t git_tag
#opaque_t git_blob
#opaque_t git_commit
#opaque_t git_tree_entry
#opaque_t git_tree
#opaque_t git_treebuilder
#opaque_t git_index
#opaque_t git_config
#opaque_t git_reflog_entry
#opaque_t git_reflog
#opaque_t git_reference
#opaque_t git_refspec
#opaque_t git_remote
#opaque_t git_transport
#opaque_t git_remote_head
#opaque_t git_headarray
{- typedef struct git_time {
	git_time_t time; 
	int offset; 
} git_time; -}
#starttype git_time
#field    time , CTime
#field    offset , CInt
#stoptype
{- typedef struct git_signature {
	char *name; 
	char *email; 
	git_time when; 
} git_signature; -}
#starttype git_signature
#field    name , CString
#field    email , CString
#field    when , <git_time>
#stoptype
