
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Errors where
#strict_import

import Bindings.Libgit2.Common
{- typedef enum {
	GIT_OK = 0,
	GIT_ERROR = -1,
	GIT_ENOTFOUND = -3,
	GIT_EEXISTS = -4,
	GIT_EAMBIGUOUS = -5,
	GIT_EBUFS = -6,

	GIT_PASSTHROUGH = -30,
	GIT_REVWALKOVER = -31,
}; -}
#num GIT_OK
#num GIT_ERROR
#num GIT_ENOTFOUND
#num GIT_EEXISTS
#num GIT_EAMBIGUOUS
#num GIT_EBUFS

#num GIT_PASSTHROUGH
#num GIT_REVWALKOVER
{- typedef struct git_error {
	char *message;
	int klass;
} git_error; -}
{- typedef enum {
	GITERR_NOMEMORY,
	GITERR_OS,
	GITERR_INVALID,
	GITERR_REFERENCE,
	GITERR_ZLIB,
	GITERR_REPOSITORY,
	GITERR_CONFIG,
	GITERR_REGEX,
	GITERR_ODB,
	GITERR_INDEX,
	GITERR_OBJECT,
	GITERR_NET,
	GITERR_TAG,
	GITERR_TREE,
	GITERR_INDEXER,
} git_error_t; -}
#integral_t git_error_t
#num GITERR_NOMEMORY
#num GITERR_OS
#num GITERR_INVALID
#num GITERR_REFERENCE
#num GITERR_ZLIB
#num GITERR_REPOSITORY
#num GITERR_CONFIG
#num GITERR_REGEX
#num GITERR_ODB
#num GITERR_INDEX
#num GITERR_OBJECT
#num GITERR_NET
#num GITERR_TAG
#num GITERR_TREE
#num GITERR_INDEXER
#ccall git_lasterror , IO (CString)
#ccall git_strerror , CInt -> IO (CString)
#ccall git_clearerror , IO ()
