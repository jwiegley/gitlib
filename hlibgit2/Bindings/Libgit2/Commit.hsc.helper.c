#include <bindings.cmacros.h>
#include <git2.h>

BC_INLINE3(git_commit_lookup, git_commit**, git_repository*, const git_oid*, int)
BC_INLINE4(git_commit_lookup_prefix, git_commit**, git_repository*, const git_oid*, size_t, int)
BC_INLINE1VOID(git_commit_free, git_commit*)
BC_INLINE1(git_commit_id, const git_commit*, const git_oid*)
