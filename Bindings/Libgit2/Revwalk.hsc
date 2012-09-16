#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Revwalk where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
#ccall git_revwalk_new , Ptr (Ptr <git_revwalk>) -> Ptr <git_repository> -> IO (CInt)
#ccall git_revwalk_reset , Ptr <git_revwalk> -> IO ()
#ccall git_revwalk_push , Ptr <git_revwalk> -> Ptr <git_oid> -> IO (CInt)
#ccall git_revwalk_push_glob , Ptr <git_revwalk> -> CString -> IO (CInt)
#ccall git_revwalk_push_head , Ptr <git_revwalk> -> IO (CInt)
#ccall git_revwalk_hide , Ptr <git_revwalk> -> Ptr <git_oid> -> IO (CInt)
#ccall git_revwalk_hide_glob , Ptr <git_revwalk> -> CString -> IO (CInt)
#ccall git_revwalk_hide_head , Ptr <git_revwalk> -> IO (CInt)
#ccall git_revwalk_push_ref , Ptr <git_revwalk> -> CString -> IO (CInt)
#ccall git_revwalk_hide_ref , Ptr <git_revwalk> -> CString -> IO (CInt)
#ccall git_revwalk_next , Ptr <git_oid> -> Ptr <git_revwalk> -> IO (CInt)
#ccall git_revwalk_sorting , Ptr <git_revwalk> -> CUInt -> IO ()
#ccall git_revwalk_free , Ptr <git_revwalk> -> IO ()
#ccall git_revwalk_repository , Ptr <git_revwalk> -> IO (Ptr <git_repository>)
