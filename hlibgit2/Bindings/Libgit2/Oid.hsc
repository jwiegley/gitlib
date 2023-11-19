{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/oid.h>
module Bindings.Libgit2.Oid where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Experimental
{- typedef enum {
            GIT_OID_SHA1 = 1
        } git_oid_t; -}
#integral_t git_oid_t
#num GIT_OID_SHA1
{- typedef struct git_oid {
            unsigned char id[20];
        } git_oid; -}
#starttype struct git_oid
#array_field id , CUChar
#stoptype
#ccall git_oid_fromstr , Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_oid_fromstrp , Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_oid_fromstrn , Ptr <struct git_oid> -> CString -> CSize -> IO CInt
#ccall git_oid_fromraw , Ptr <struct git_oid> -> Ptr CUChar -> IO CInt
#ccall git_oid_fmt , CString -> Ptr <struct git_oid> -> IO CInt
#ccall git_oid_nfmt , CString -> CSize -> Ptr <struct git_oid> -> IO CInt
#ccall git_oid_pathfmt , CString -> Ptr <struct git_oid> -> IO CInt
#ccall git_oid_tostr_s , Ptr <struct git_oid> -> IO CString
#ccall git_oid_tostr , CString -> CSize -> Ptr <struct git_oid> -> IO CString
#ccall git_oid_cpy , Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_oid_cmp , Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_oid_equal , Ptr <struct git_oid> -> Ptr <struct git_oid> -> IO CInt
#ccall git_oid_ncmp , Ptr <struct git_oid> -> Ptr <struct git_oid> -> CSize -> IO CInt
#ccall git_oid_streq , Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_oid_strcmp , Ptr <struct git_oid> -> CString -> IO CInt
#ccall git_oid_is_zero , Ptr <struct git_oid> -> IO CInt
{- typedef struct git_oid_shorten git_oid_shorten; -}
#opaque_t struct git_oid_shorten
#ccall git_oid_shorten_new , CSize -> IO (Ptr <struct git_oid_shorten>)
#ccall git_oid_shorten_add , Ptr <struct git_oid_shorten> -> CString -> IO CInt
#ccall git_oid_shorten_free , Ptr <struct git_oid_shorten> -> IO ()
