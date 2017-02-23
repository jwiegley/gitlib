{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Common where
import Foreign.Ptr
#strict_import

#ccall git_libgit2_version , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
{- enum {
    GIT_CAP_THREADS = 1 << 0, GIT_CAP_HTTPS = 1 << 1
}; -}
#num GIT_FEATURE_THREADS
#num GIT_FEATURE_HTTPS
#ccall git_libgit2_features , IO (CInt)
{- enum {
    GIT_OPT_GET_MWINDOW_SIZE,
    GIT_OPT_SET_MWINDOW_SIZE,
    GIT_OPT_GET_MWINDOW_MAPPED_LIMIT,
    GIT_OPT_SET_MWINDOW_MAPPED_LIMIT,
    GIT_OPT_GET_SEARCH_PATH,
    GIT_OPT_SET_SEARCH_PATH,
    GIT_OPT_GET_ODB_CACHE_SIZE,
    GIT_OPT_SET_ODB_CACHE_SIZE
}; -}
#num GIT_OPT_GET_MWINDOW_SIZE
#num GIT_OPT_SET_MWINDOW_SIZE
#num GIT_OPT_GET_MWINDOW_MAPPED_LIMIT
#num GIT_OPT_SET_MWINDOW_MAPPED_LIMIT
#num GIT_OPT_GET_SEARCH_PATH
#num GIT_OPT_SET_SEARCH_PATH
#num GIT_OPT_SET_CACHE_OBJECT_LIMIT
#num GIT_OPT_SET_CACHE_MAX_SIZE
#num GIT_OPT_ENABLE_CACHING
#num GIT_OPT_GET_CACHED_MEMORY
#num GIT_OPT_GET_TEMPLATE_PATH
#num GIT_OPT_SET_TEMPLATE_PATH
#num GIT_OPT_SET_SSL_CERT_LOCATIONS
#ccall git_libgit2_opts , CInt -> IO (CInt)
