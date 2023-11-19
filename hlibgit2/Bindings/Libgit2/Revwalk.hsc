{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/revwalk.h>
module Bindings.Libgit2.Revwalk where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef enum {
            GIT_SORT_NONE = 0,
            GIT_SORT_TOPOLOGICAL = 1 << 0,
            GIT_SORT_TIME = 1 << 1,
            GIT_SORT_REVERSE = 1 << 2
        } git_sort_t; -}
#integral_t git_sort_t
#num GIT_SORT_NONE
#num GIT_SORT_TOPOLOGICAL
#num GIT_SORT_TIME
#num GIT_SORT_REVERSE
#ccall git_revwalk_new , Ptr (Ptr <struct git_revwalk>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_revwalk_reset , Ptr <struct git_revwalk> -> IO CInt
#ccall git_revwalk_push , Ptr <struct git_revwalk> -> Ptr <struct git_oid> -> IO CInt
#ccall git_revwalk_push_glob , Ptr <struct git_revwalk> -> CString -> IO CInt
#ccall git_revwalk_push_head , Ptr <struct git_revwalk> -> IO CInt
#ccall git_revwalk_hide , Ptr <struct git_revwalk> -> Ptr <struct git_oid> -> IO CInt
#ccall git_revwalk_hide_glob , Ptr <struct git_revwalk> -> CString -> IO CInt
#ccall git_revwalk_hide_head , Ptr <struct git_revwalk> -> IO CInt
#ccall git_revwalk_push_ref , Ptr <struct git_revwalk> -> CString -> IO CInt
#ccall git_revwalk_hide_ref , Ptr <struct git_revwalk> -> CString -> IO CInt
#ccall git_revwalk_next , Ptr <struct git_oid> -> Ptr <struct git_revwalk> -> IO CInt
#ccall git_revwalk_sorting , Ptr <struct git_revwalk> -> CUInt -> IO CInt
#ccall git_revwalk_push_range , Ptr <struct git_revwalk> -> CString -> IO CInt
#ccall git_revwalk_simplify_first_parent , Ptr <struct git_revwalk> -> IO CInt
#ccall git_revwalk_free , Ptr <struct git_revwalk> -> IO ()
#ccall git_revwalk_repository , Ptr <struct git_revwalk> -> IO (Ptr <struct git_repository>)
#callback git_revwalk_hide_cb , Ptr <struct git_oid> -> Ptr () -> IO CInt
#ccall git_revwalk_add_hide_cb , Ptr <struct git_revwalk> -> <git_revwalk_hide_cb> -> Ptr () -> IO CInt
