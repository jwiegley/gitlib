#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Status where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
#ccall git_status_foreach , Ptr <git_repository> -> Ptr () -> Ptr () -> IO (CInt)
{- typedef enum {
            GIT_STATUS_SHOW_INDEX_AND_WORKDIR = 0,
            GIT_STATUS_SHOW_INDEX_ONLY = 1,
            GIT_STATUS_SHOW_WORKDIR_ONLY = 2,
            GIT_STATUS_SHOW_INDEX_THEN_WORKDIR = 3
        } git_status_show_t; -}
#integral_t git_status_show_t
#num GIT_STATUS_SHOW_INDEX_AND_WORKDIR
#num GIT_STATUS_SHOW_INDEX_ONLY
#num GIT_STATUS_SHOW_WORKDIR_ONLY
#num GIT_STATUS_SHOW_INDEX_THEN_WORKDIR
{- typedef struct {
            git_status_show_t show; unsigned int flags; git_strarray pathspec;
        } git_status_options; -}
#starttype git_status_options
#field show , <git_status_show_t>
#field flags , CUInt
#field pathspec , <git_strarray>
#stoptype
#ccall git_status_foreach_ext , Ptr <git_repository> -> Ptr <git_status_options> -> Ptr () -> Ptr () -> IO (CInt)
#ccall git_status_file , Ptr CUInt -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_status_should_ignore , Ptr CInt -> Ptr <git_repository> -> CString -> IO (CInt)
