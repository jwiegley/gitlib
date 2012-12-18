#include <bindings.cmacros.h>
#include <git2.h>

BC_INLINE3(git_tag_lookup, git_tag**, git_repository*, const git_oid*, int)
BC_INLINE4(git_tag_lookup_prefix, git_tag**, git_repository*, const git_oid*, unsigned int, int)
BC_INLINE1VOID(git_tag_free, git_tag*)
