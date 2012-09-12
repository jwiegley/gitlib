#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Net where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
{- struct git_remote_head {
    int local : 1; git_oid oid; git_oid loid; char * name;
}; -}
#starttype git_remote_head
-- #field local , CInt
#field oid , <git_oid>
#field loid , <git_oid>
#field name , CString
#stoptype
{- typedef int (* git_headlist_cb)(git_remote_head *, void *); -}
#synonym_t git_headlist_cb , CInt
