#include <bindings.cmacros.h>
#include <git2.h>

BC_INLINE3(git_tree_lookup, git_tree**, git_repository*, const git_oid*, int)
BC_INLINE4(git_tree_lookup_prefix, git_tree**, git_repository*, const git_oid*, size_t, int)
BC_INLINE1VOID(git_tree_free, git_tree*)
