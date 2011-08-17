
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Errors where
#strict_import

import Bindings.Libgit2.Common
{- typedef enum {
	GIT_SUCCESS = 0,
	GIT_ERROR = -1,

	/** Input was not a properly formatted Git object id. */
	GIT_ENOTOID = -2,

	/** Input does not exist in the scope searched. */
	GIT_ENOTFOUND = -3,

	/** Not enough space available. */
	GIT_ENOMEM = -4,

	/** Consult the OS error information. */
	GIT_EOSERR = -5,

	/** The specified object is of invalid type */
	GIT_EOBJTYPE = -6,

	/** The specified repository is invalid */
	GIT_ENOTAREPO = -7,

	/** The object type is invalid or doesn't match */
	GIT_EINVALIDTYPE = -8,

	/** The object cannot be written because it's missing internal data */
	GIT_EMISSINGOBJDATA = -9,

	/** The packfile for the ODB is corrupted */
	GIT_EPACKCORRUPTED = -10,

	/** Failed to acquire or release a file lock */
	GIT_EFLOCKFAIL = -11,

	/** The Z library failed to inflate/deflate an object's data */
	GIT_EZLIB = -12,

	/** The queried object is currently busy */
	GIT_EBUSY = -13,

	/** The index file is not backed up by an existing repository */
	GIT_EBAREINDEX = -14,

	/** The name of the reference is not valid */
	GIT_EINVALIDREFNAME = -15,

	/** The specified reference has its data corrupted */
	GIT_EREFCORRUPTED  = -16,

	/** The specified symbolic reference is too deeply nested */
	GIT_ETOONESTEDSYMREF = -17,

	/** The pack-refs file is either corrupted or its format is not currently supported */
	GIT_EPACKEDREFSCORRUPTED = -18,

	/** The path is invalid */
	GIT_EINVALIDPATH = -19,

	/** The revision walker is empty; there are no more commits left to iterate */
	GIT_EREVWALKOVER = -20,

	/** The state of the reference is not valid */
	GIT_EINVALIDREFSTATE = -21,

	/** This feature has not been implemented yet */
	GIT_ENOTIMPLEMENTED = -22,

	/** A reference with this name already exists */
	GIT_EEXISTS = -23,

	/** The given integer literal is too large to be parsed */
	GIT_EOVERFLOW = -24,

	/** The given literal is not a valid number */
	GIT_ENOTNUM = -25,

	/** Streaming error */
	GIT_ESTREAM = -26,

	/** invalid arguments to function */
	GIT_EINVALIDARGS = -27,

	/** The specified object has its data corrupted */
	GIT_EOBJCORRUPTED = -28,

	/** The given short oid is ambiguous */
	GIT_EAMBIGUOUSOIDPREFIX = -29,

	/** Skip and passthrough the given ODB backend */
	GIT_EPASSTHROUGH = -30,

	/** The path pattern and string did not match */
	GIT_ENOMATCH = -31,

	/**  The buffer is too short to satisfy the request */
	GIT_ESHORTBUFFER = -32,
} git_error; -}
#integral_t git_error
#num    GIT_SUCCESS
#num    GIT_ERROR
#num    GIT_ENOTOID
#num    GIT_ENOTFOUND
#num    GIT_ENOMEM
#num    GIT_EOSERR
#num    GIT_EOBJTYPE
#num    GIT_ENOTAREPO
#num    GIT_EINVALIDTYPE
#num    GIT_EMISSINGOBJDATA
#num    GIT_EPACKCORRUPTED
#num    GIT_EFLOCKFAIL
#num    GIT_EZLIB
#num    GIT_EBUSY
#num    GIT_EBAREINDEX
#num    GIT_EINVALIDREFNAME
#num    GIT_ETOONESTEDSYMREF
#num    GIT_EPACKEDREFSCORRUPTED
#num    GIT_EINVALIDPATH
#num    GIT_EREVWALKOVER
#num    GIT_EINVALIDREFSTATE
#num    GIT_ENOTIMPLEMENTED
#num    GIT_EEXISTS
#num    GIT_EOVERFLOW
#num    GIT_ENOTNUM
#num    GIT_ESTREAM
#num    GIT_EINVALIDARGS
#num    GIT_EOBJCORRUPTED
#num    GIT_EAMBIGUOUSOIDPREFIX
#num    GIT_EPASSTHROUGH
#num    GIT_ENOMATCH
#num    GIT_ESHORTBUFFER
#ccall git_lasterror , IO (CString)
#ccall git_strerror , CInt -> IO (CString)
#ccall git_clearerror , IO ()
