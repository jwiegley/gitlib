#include <bindings.cmacros.h>
#include <git2.h>

BC_INLINE3(git_blob_lookup, git_blob**, git_repository*, const git_oid*, int)
BC_INLINE4(git_blob_lookup_prefix, git_blob**, git_repository*, const git_oid*, size_t, int)
BC_INLINE1VOID(git_blob_free, git_blob*)
BC_INLINE1(git_blob_id, const git_blob*, const git_oid*)
