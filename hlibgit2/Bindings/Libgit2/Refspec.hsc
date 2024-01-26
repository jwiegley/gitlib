{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/refspec.h>
module Bindings.Libgit2.Refspec where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Net
import Bindings.Libgit2.Buffer
#ccall git_refspec_parse , Ptr (Ptr <struct git_refspec>) -> CString -> CInt -> IO CInt
#ccall git_refspec_free , Ptr <struct git_refspec> -> IO ()
#ccall git_refspec_src , Ptr <struct git_refspec> -> IO CString
#ccall git_refspec_dst , Ptr <struct git_refspec> -> IO CString
#ccall git_refspec_string , Ptr <struct git_refspec> -> IO CString
#ccall git_refspec_force , Ptr <struct git_refspec> -> IO CInt
#ccall git_refspec_direction , Ptr <struct git_refspec> -> IO <git_direction>
#ccall git_refspec_src_matches , Ptr <struct git_refspec> -> CString -> IO CInt
#ccall git_refspec_dst_matches , Ptr <struct git_refspec> -> CString -> IO CInt
#ccall git_refspec_transform , Ptr <git_buf> -> Ptr <struct git_refspec> -> CString -> IO CInt
#ccall git_refspec_rtransform , Ptr <git_buf> -> Ptr <struct git_refspec> -> CString -> IO CInt
