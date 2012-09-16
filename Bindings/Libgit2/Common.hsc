#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Common where
#strict_import

{- typedef struct {
            char * * strings; size_t count;
        } git_strarray; -}
#starttype git_strarray
#field strings , Ptr (CString)
#field count , CSize
#stoptype
#ccall git_strarray_free , Ptr <git_strarray> -> IO ()
#ccall git_strarray_copy , Ptr <git_strarray> -> Ptr <git_strarray> -> IO (CInt)
#ccall git_libgit2_version , Ptr CInt -> Ptr CInt -> Ptr CInt -> IO ()

{- BEGIN MANUAL ENTRY -}
#cinline GIT_PATH_LIST_SEPARATOR , CChar
#num GIT_PATH_MAX
{- END MANUAL ENTRY -}
