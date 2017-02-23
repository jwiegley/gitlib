

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Errors where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
{- enum {
    GIT_OK = 0,
    GIT_ERROR = -1,
    GIT_ENOTFOUND = -3,
    GIT_EEXISTS = -4,
    GIT_EAMBIGUOUS = -5,
    GIT_EBUFS = -6,
    GIT_EUSER = -7,
    GIT_EBAREREPO = -8,
    GIT_EORPHANEDHEAD = -9,
    GIT_EUNMERGED = -10,
    GIT_ENONFASTFORWARD = -11,
    GIT_EINVALIDSPEC = -12,
    GIT_EMERGECONFLICT = -13,
    GIT_PASSTHROUGH = -30,
    GIT_ITEROVER = -31
}; -}
#num GIT_OK
#num GIT_ERROR
#num GIT_ENOTFOUND
#num GIT_EEXISTS
#num GIT_EAMBIGUOUS
#num GIT_EBUFS
#num GIT_EUSER
#num GIT_EBAREREPO
#num GIT_EUNBORNBRANCH
#num GIT_EUNMERGED
#num GIT_ENONFASTFORWARD
#num GIT_EINVALIDSPEC
#num GIT_ECONFLICT
#num GIT_ELOCKED
#num GIT_EMODIFIED
#num GIT_EAUTH
#num GIT_ECERTIFICATE
#num GIT_EAPPLIED
#num GIT_EPEEL
#num GIT_EEOF
#num GIT_EINVALID
#num GIT_EUNCOMMITTED
#num GIT_EDIRECTORY
#num GIT_PASSTHROUGH
#num GIT_ITEROVER
{- typedef struct {
            char * message; int klass;
        } git_error; -}
#starttype git_error
#field message , CString
#field klass , CInt
#stoptype
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
            GITERR_SSL,
            GITERR_SUBMODULE,
            GITERR_THREAD,
            GITERR_STASH,
            GITERR_CHECKOUT,
            GITERR_FETCHHEAD,
            GITERR_MERGE
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
#num GITERR_SSL
#num GITERR_SUBMODULE
#num GITERR_THREAD
#num GITERR_STASH
#num GITERR_CHECKOUT
#num GITERR_FETCHHEAD
#num GITERR_MERGE
#ccall giterr_last , IO (Ptr <git_error>)
#ccall giterr_clear , IO ()
#ccall giterr_set_str , CInt -> CString -> IO ()
#ccall giterr_set_oom , IO ()
