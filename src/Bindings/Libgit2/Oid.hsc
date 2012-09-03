
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Oid where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#num    GIT_OID_RAWSZ
#num    GIT_OID_HEXSZ
#num    GIT_OID_MINPREFIXLEN
#opaque_t git_oid_shorten
{- typedef struct {
	
	unsigned char id[GIT_OID_RAWSZ];
} git_oid; -}
#starttype git_oid
#array_field    id , Word8
#stoptype
#ccall git_oid_fromstr , Ptr <git_oid> -> CString -> IO (CInt)
#ccall git_oid_fromstrn , Ptr <git_oid> -> CString -> CSize -> IO (CInt)
#ccall git_oid_fromraw , Ptr <git_oid> -> Ptr Word8 -> IO ()
#ccall git_oid_fmt , CString -> Ptr <git_oid> -> IO ()
#ccall git_oid_pathfmt , CString -> Ptr <git_oid> -> IO ()
#ccall git_oid_allocfmt , Ptr <git_oid> -> IO (CString)
#ccall git_oid_to_string , CString -> CSize -> Ptr <git_oid> -> IO (CString)
#ccall git_oid_cpy , Ptr <git_oid> -> Ptr <git_oid> -> IO ()
#ccall git_oid_cmp , Ptr <git_oid> -> Ptr <git_oid> -> IO (CInt)
#ccall git_oid_ncmp , Ptr <git_oid> -> Ptr <git_oid> -> CUInt -> IO (CInt)
