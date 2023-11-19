{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/trace.h>
module Bindings.Libgit2.Trace where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- typedef enum {
            GIT_TRACE_NONE = 0,
            GIT_TRACE_FATAL = 1,
            GIT_TRACE_ERROR = 2,
            GIT_TRACE_WARN = 3,
            GIT_TRACE_INFO = 4,
            GIT_TRACE_DEBUG = 5,
            GIT_TRACE_TRACE = 6
        } git_trace_level_t; -}
#integral_t git_trace_level_t
#num GIT_TRACE_NONE
#num GIT_TRACE_FATAL
#num GIT_TRACE_ERROR
#num GIT_TRACE_WARN
#num GIT_TRACE_INFO
#num GIT_TRACE_DEBUG
#num GIT_TRACE_TRACE
#callback git_trace_cb , <git_trace_level_t> -> CString -> IO ()
#ccall git_trace_set , <git_trace_level_t> -> <git_trace_cb> -> IO CInt
