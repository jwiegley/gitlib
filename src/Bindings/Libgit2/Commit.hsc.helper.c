
#include <bindings.cmacros.h>
#include <git2.h>

BC_INLINE3(git_commit_lookup, git_commit **, git_repository *, const git_oid *,int)
BC_INLINE4(git_commit_lookup_prefix, git_commit **, git_repository *, const git_oid *, unsigned ,int)
BC_INLINE1VOID(git_commit_close, git_commit *)
