#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Errors where
#strict_import

import Bindings.Libgit2.Common
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
            GITERR_INDEXER
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
#ccall giterr_last , IO (Ptr <git_error>)
#ccall giterr_clear , IO ()
