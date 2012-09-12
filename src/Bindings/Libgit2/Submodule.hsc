#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Submodule where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
{- typedef enum {
            GIT_SUBMODULE_UPDATE_CHECKOUT = 0,
            GIT_SUBMODULE_UPDATE_REBASE = 1,
            GIT_SUBMODULE_UPDATE_MERGE = 2
        } git_submodule_update_t; -}
#integral_t git_submodule_update_t
#num GIT_SUBMODULE_UPDATE_CHECKOUT
#num GIT_SUBMODULE_UPDATE_REBASE
#num GIT_SUBMODULE_UPDATE_MERGE
{- typedef enum {
            GIT_SUBMODULE_IGNORE_ALL = 0,
            GIT_SUBMODULE_IGNORE_DIRTY = 1,
            GIT_SUBMODULE_IGNORE_UNTRACKED = 2,
            GIT_SUBMODULE_IGNORE_NONE = 3
        } git_submodule_ignore_t; -}
#integral_t git_submodule_ignore_t
#num GIT_SUBMODULE_IGNORE_ALL
#num GIT_SUBMODULE_IGNORE_DIRTY
#num GIT_SUBMODULE_IGNORE_UNTRACKED
#num GIT_SUBMODULE_IGNORE_NONE
{- typedef struct {
            char * name;
            char * path;
            char * url;
            git_oid oid;
            git_submodule_update_t update;
            git_submodule_ignore_t ignore;
            int fetch_recurse;
            int refcount;
        } git_submodule; -}
#starttype git_submodule
#field name , CString
#field path , CString
#field url , CString
#field oid , <git_oid>
#field update , <git_submodule_update_t>
#field ignore , <git_submodule_ignore_t>
#field fetch_recurse , CInt
#field refcount , CInt
#stoptype
#ccall git_submodule_foreach , Ptr <git_repository> -> FunPtr (CString -> Ptr () -> CInt) -> Ptr () -> IO (CInt)
#ccall git_submodule_lookup , Ptr (Ptr <git_submodule>) -> Ptr <git_repository> -> CString -> IO (CInt)
