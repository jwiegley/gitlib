#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Remote where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Repository
import Bindings.Libgit2.Refspec
import Bindings.Libgit2.Net
import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Types
#ccall git_remote_new , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> CString -> CString -> IO (CInt)
#ccall git_remote_load , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> IO (CInt)
#ccall git_remote_save , Ptr <git_remote> -> IO (CInt)
#ccall git_remote_name , Ptr <git_remote> -> IO (CString)
#ccall git_remote_url , Ptr <git_remote> -> IO (CString)
#ccall git_remote_set_fetchspec , Ptr <git_remote> -> CString -> IO (CInt)
#ccall git_remote_fetchspec , Ptr <git_remote> -> IO (Ptr <git_refspec>)
#ccall git_remote_set_pushspec , Ptr <git_remote> -> CString -> IO (CInt)
#ccall git_remote_pushspec , Ptr <git_remote> -> IO (Ptr <git_refspec>)
#ccall git_remote_connect , Ptr <git_remote> -> CInt -> IO (CInt)
#ccall git_remote_ls , Ptr <git_remote> -> CInt -> Ptr () -> IO (CInt)
#ccall git_remote_download , Ptr <git_remote> -> Ptr CLong -> Ptr <git_indexer_stats> -> IO (CInt)
#ccall git_remote_connected , Ptr <git_remote> -> IO (CInt)
#ccall git_remote_disconnect , Ptr <git_remote> -> IO ()
#ccall git_remote_free , Ptr <git_remote> -> IO ()
#ccall git_remote_update_tips , Ptr <git_remote> -> Ptr () -> IO (CInt)
#ccall git_remote_valid_url , CString -> IO (CInt)
#ccall git_remote_supported_url , CString -> IO (CInt)
#ccall git_remote_list , Ptr <git_strarray> -> Ptr <git_repository> -> IO (CInt)
#ccall git_remote_add , Ptr (Ptr <git_remote>) -> Ptr <git_repository> -> CString -> CString -> IO (CInt)
