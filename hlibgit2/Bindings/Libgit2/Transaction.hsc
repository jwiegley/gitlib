{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/transaction.h>
module Bindings.Libgit2.Transaction where
import Foreign.Ptr
import Bindings.Libgit2.Oid
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_transaction_new , Ptr (Ptr <struct git_transaction>) -> Ptr <struct git_repository> -> IO CInt
#ccall git_transaction_lock_ref , Ptr <struct git_transaction> -> CString -> IO CInt
#ccall git_transaction_set_target , Ptr <struct git_transaction> -> CString -> Ptr <struct git_oid> -> Ptr <struct git_signature> -> CString -> IO CInt
#ccall git_transaction_set_symbolic_target , Ptr <struct git_transaction> -> CString -> CString -> Ptr <struct git_signature> -> CString -> IO CInt
#ccall git_transaction_set_reflog , Ptr <struct git_transaction> -> CString -> Ptr <struct git_reflog> -> IO CInt
#ccall git_transaction_remove , Ptr <struct git_transaction> -> CString -> IO CInt
#ccall git_transaction_commit , Ptr <struct git_transaction> -> IO CInt
#ccall git_transaction_free , Ptr <struct git_transaction> -> IO ()
