
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Common where
#strict_import

#num    GIT_PATH_LIST_SEPARATOR
#num    GIT_PATH_MAX
{- typedef struct {
	char **strings;
	size_t count;
} git_strarray; -}
#starttype git_strarray
#field    strings , Ptr CString
#field    count , CSize
#stoptype
#ccall git_strarray_free , Ptr <git_strarray> -> IO ()
#ccall git_libgit2_version , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()
