{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Net where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
{- typedef enum {
            GIT_DIRECTION_FETCH = 0, GIT_DIRECTION_PUSH = 1
        } git_direction; -}
#integral_t git_direction
#num GIT_DIRECTION_FETCH
#num GIT_DIRECTION_PUSH
{- struct git_remote_head {
    int local; git_oid oid; git_oid loid; char * name;
}; -}
#starttype git_remote_head
#field local , CInt
#field oid , <git_oid>
#field loid , <git_oid>
#field name , CString
#stoptype
{- typedef int (* git_headlist_cb)(git_remote_head * rhead,
                                void * payload); -}
#callback git_headlist_cb , Ptr (<git_remote_head>) -> Ptr () -> IO CInt
