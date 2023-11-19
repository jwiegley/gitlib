{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/errors.h>
module Bindings.Libgit2.Errors where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
{- typedef enum {
            GIT_OK = 0,
            GIT_ERROR = -1,
            GIT_ENOTFOUND = -3,
            GIT_EEXISTS = -4,
            GIT_EAMBIGUOUS = -5,
            GIT_EBUFS = -6,
            GIT_EUSER = -7,
            GIT_EBAREREPO = -8,
            GIT_EUNBORNBRANCH = -9,
            GIT_EUNMERGED = -10,
            GIT_ENONFASTFORWARD = -11,
            GIT_EINVALIDSPEC = -12,
            GIT_ECONFLICT = -13,
            GIT_ELOCKED = -14,
            GIT_EMODIFIED = -15,
            GIT_EAUTH = -16,
            GIT_ECERTIFICATE = -17,
            GIT_EAPPLIED = -18,
            GIT_EPEEL = -19,
            GIT_EEOF = -20,
            GIT_EINVALID = -21,
            GIT_EUNCOMMITTED = -22,
            GIT_EDIRECTORY = -23,
            GIT_EMERGECONFLICT = -24,
            GIT_PASSTHROUGH = -30,
            GIT_ITEROVER = -31,
            GIT_RETRY = -32,
            GIT_EMISMATCH = -33,
            GIT_EINDEXDIRTY = -34,
            GIT_EAPPLYFAIL = -35,
            GIT_EOWNER = -36,
            GIT_TIMEOUT = -37
        } git_error_code; -}
#integral_t git_error_code
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
#num GIT_EMERGECONFLICT
#num GIT_PASSTHROUGH
#num GIT_ITEROVER
#num GIT_RETRY
#num GIT_EMISMATCH
#num GIT_EINDEXDIRTY
#num GIT_EAPPLYFAIL
#num GIT_EOWNER
#num GIT_TIMEOUT
{- typedef struct {
            char * message; int klass;
        } git_error; -}
#starttype git_error
#field message , CString
#field klass , CInt
#stoptype
{- typedef enum {
            GIT_ERROR_NONE = 0,
            GIT_ERROR_NOMEMORY,
            GIT_ERROR_OS,
            GIT_ERROR_INVALID,
            GIT_ERROR_REFERENCE,
            GIT_ERROR_ZLIB,
            GIT_ERROR_REPOSITORY,
            GIT_ERROR_CONFIG,
            GIT_ERROR_REGEX,
            GIT_ERROR_ODB,
            GIT_ERROR_INDEX,
            GIT_ERROR_OBJECT,
            GIT_ERROR_NET,
            GIT_ERROR_TAG,
            GIT_ERROR_TREE,
            GIT_ERROR_INDEXER,
            GIT_ERROR_SSL,
            GIT_ERROR_SUBMODULE,
            GIT_ERROR_THREAD,
            GIT_ERROR_STASH,
            GIT_ERROR_CHECKOUT,
            GIT_ERROR_FETCHHEAD,
            GIT_ERROR_MERGE,
            GIT_ERROR_SSH,
            GIT_ERROR_FILTER,
            GIT_ERROR_REVERT,
            GIT_ERROR_CALLBACK,
            GIT_ERROR_CHERRYPICK,
            GIT_ERROR_DESCRIBE,
            GIT_ERROR_REBASE,
            GIT_ERROR_FILESYSTEM,
            GIT_ERROR_PATCH,
            GIT_ERROR_WORKTREE,
            GIT_ERROR_SHA,
            GIT_ERROR_HTTP,
            GIT_ERROR_INTERNAL,
            GIT_ERROR_GRAFTS
        } git_error_t; -}
#integral_t git_error_t
#num GIT_ERROR_NONE
#num GIT_ERROR_NOMEMORY
#num GIT_ERROR_OS
#num GIT_ERROR_INVALID
#num GIT_ERROR_REFERENCE
#num GIT_ERROR_ZLIB
#num GIT_ERROR_REPOSITORY
#num GIT_ERROR_CONFIG
#num GIT_ERROR_REGEX
#num GIT_ERROR_ODB
#num GIT_ERROR_INDEX
#num GIT_ERROR_OBJECT
#num GIT_ERROR_NET
#num GIT_ERROR_TAG
#num GIT_ERROR_TREE
#num GIT_ERROR_INDEXER
#num GIT_ERROR_SSL
#num GIT_ERROR_SUBMODULE
#num GIT_ERROR_THREAD
#num GIT_ERROR_STASH
#num GIT_ERROR_CHECKOUT
#num GIT_ERROR_FETCHHEAD
#num GIT_ERROR_MERGE
#num GIT_ERROR_SSH
#num GIT_ERROR_FILTER
#num GIT_ERROR_REVERT
#num GIT_ERROR_CALLBACK
#num GIT_ERROR_CHERRYPICK
#num GIT_ERROR_DESCRIBE
#num GIT_ERROR_REBASE
#num GIT_ERROR_FILESYSTEM
#num GIT_ERROR_PATCH
#num GIT_ERROR_WORKTREE
#num GIT_ERROR_SHA
#num GIT_ERROR_HTTP
#num GIT_ERROR_INTERNAL
#num GIT_ERROR_GRAFTS
#ccall git_error_last , IO (Ptr <git_error>)
