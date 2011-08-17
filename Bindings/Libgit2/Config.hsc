
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Config where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types

#starttype struct git_config_file
#field cfg, Ptr <git_config>
#field open, FunPtr (Ptr <git_config_file> -> IO CInt)
#field get, FunPtr (Ptr <git_config_file> -> CString -> Ptr CString -> IO CInt)
#field set, FunPtr (Ptr <git_config_file> -> CString -> CString -> IO CInt)
#field foreach, FunPtr (Ptr <git_config_file> -> FunPtr (CString -> CString -> Ptr () -> CInt) -> Ptr () -> IO CInt)
#field free, FunPtr (Ptr <git_config_file> -> IO ())
#stoptype

#ccall git_config_find_global , CString -> IO (CInt)
#ccall git_config_open_global , Ptr (Ptr <git_config>) -> IO (CInt)
#ccall git_config_file__ondisk , Ptr (Ptr <git_config_file>) -> CString -> IO (CInt)
#ccall git_config_new , Ptr (Ptr <git_config>) -> IO (CInt)
#ccall git_config_add_file , Ptr <git_config> -> Ptr <git_config_file> -> CInt -> IO (CInt)
#ccall git_config_add_file_ondisk , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_open_ondisk , Ptr (Ptr <git_config>) -> CString -> IO (CInt)
#ccall git_config_free , Ptr <git_config> -> IO ()
#ccall git_config_get_int , Ptr <git_config> -> CString -> Ptr CInt -> IO (CInt)
#ccall git_config_get_long , Ptr <git_config> -> CString -> CLong -> IO (CInt)
#ccall git_config_get_bool , Ptr <git_config> -> CString -> Ptr CInt -> IO (CInt)
#ccall git_config_get_string , Ptr <git_config> -> CString -> Ptr CString -> IO (CInt)
#ccall git_config_set_int , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_set_long , Ptr <git_config> -> CString -> CLong -> IO (CInt)
#ccall git_config_set_bool , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_set_string , Ptr <git_config> -> CString -> CString -> IO (CInt)
#ccall git_config_delete , Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_foreach , Ptr <git_config> -> CInt -> IO (CInt)
