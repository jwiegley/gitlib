module Bindings.Git2 where
#include <bindings.dsl.h>
#include <git2.h>
#strict_import

#opaque_t git_repository

#ccall git_repository_open , Ptr (Ptr <git_repository>) -> CString -> IO CInt 
#ccall git_repository_free , Ptr <git_repository> -> IO CInt