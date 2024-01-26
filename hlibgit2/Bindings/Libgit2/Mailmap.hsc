{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/mailmap.h>
module Bindings.Libgit2.Mailmap where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Buffer
#ccall git_mailmap_new , Ptr (Ptr <struct git_mailmap>) -> IO CInt
#ccall git_mailmap_free , Ptr <struct git_mailmap> -> IO ()
#ccall git_mailmap_add_entry , Ptr <struct git_mailmap> -> CString -> CString -> CString -> CString -> IO CInt
#ccall git_mailmap_from_buffer , Ptr (Ptr <struct git_mailmap>) -> CString -> CSize -> IO CInt
#ccall git_mailmap_from_repository , Ptr (Ptr <struct git_mailmap>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_mailmap_resolve , Ptr CString -> Ptr CString -> Ptr <struct git_mailmap> -> CString -> CString -> IO CInt
#ccall git_mailmap_resolve_signature , Ptr (Ptr <struct git_signature>) -> Ptr <struct git_mailmap> -> Ptr <struct git_signature> -> IO CInt
