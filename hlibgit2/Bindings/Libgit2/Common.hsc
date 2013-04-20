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
#num GIT_CAP_THREADS
#num GIT_CAP_HTTPS
#ccall git_libgit2_capabilities , IO (CInt)
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
#num GIT_OPT_GET_ODB_CACHE_SIZE
#num GIT_OPT_SET_ODB_CACHE_SIZE
#ccall git_libgit2_opts , CInt -> IO (CInt)
