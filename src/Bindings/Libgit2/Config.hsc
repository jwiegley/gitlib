#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Config where
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- struct git_config_file {
    struct git_config * cfg;
    int (* open)(struct git_config_file *);
    int (* get)(struct git_config_file *,
                const char * key,
                const char * * value);
    int (* get_multivar)(struct git_config_file *,
                         const char * key,
                         const char * regexp,
                         int (* fn)(const char *, void *),
                         void * data);
    int (* set)(struct git_config_file *,
                const char * key,
                const char * value);
    int (* set_multivar)(git_config_file * cfg,
                         const char * name,
                         const char * regexp,
                         const char * value);
    int (* del)(struct git_config_file *, const char * key);
    int (* foreach)(struct git_config_file *,
                    int (* fn)(const char *, const char *, void *),
                    void * data);
    void (* free)(struct git_config_file *);
}; -}
#starttype git_config_file
#field cfg , Ptr <git_config>
#field open , FunPtr (Ptr <git_config_file> -> CInt)
#field get , FunPtr (Ptr <git_config_file> -> CString -> Ptr (CString) -> CInt)
#field get_multivar , FunPtr (Ptr <git_config_file> -> CString -> CString -> FunPtr (CString -> Ptr () -> CInt) -> Ptr () -> CInt)
#field set , FunPtr (Ptr <git_config_file> -> CString -> CString -> CInt)
#field set_multivar , FunPtr (Ptr <git_config_file> -> CString -> CString -> CString -> CInt)
#field del , FunPtr (Ptr <git_config_file> -> CString -> CInt)
#field foreach , FunPtr (Ptr <git_config_file> -> FunPtr (CString -> CString -> Ptr () -> CInt) -> Ptr () -> CInt)
#field free , FunPtr (Ptr <git_config_file>)
#stoptype
{- typedef enum {
            GIT_CVAR_FALSE = 0,
            GIT_CVAR_TRUE = 1,
            GIT_CVAR_INT32,
            GIT_CVAR_STRING
        } git_cvar_t; -}
#integral_t git_cvar_t
#num GIT_CVAR_FALSE
#num GIT_CVAR_TRUE
#num GIT_CVAR_INT32
#num GIT_CVAR_STRING
{- typedef struct {
            git_cvar_t cvar_type; const char * str_match; int map_value;
        } git_cvar_map; -}
#starttype git_cvar_map
#field cvar_type , <git_cvar_t>
#field str_match , CString
#field map_value , CInt
#stoptype
#ccall git_config_find_global , CString -> CSize -> IO (CInt)
#ccall git_config_find_system , CString -> CSize -> IO (CInt)
#ccall git_config_open_global , Ptr (Ptr <git_config>) -> IO (CInt)
#ccall git_config_file__ondisk , Ptr (Ptr <git_config_file>) -> CString -> IO (CInt)
#ccall git_config_new , Ptr (Ptr <git_config>) -> IO (CInt)
#ccall git_config_add_file , Ptr <git_config> -> Ptr <git_config_file> -> CInt -> IO (CInt)
#ccall git_config_add_file_ondisk , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_open_ondisk , Ptr (Ptr <git_config>) -> CString -> IO (CInt)
#ccall git_config_free , Ptr <git_config> -> IO ()
#ccall git_config_get_int32 , Ptr CInt -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_int64 , Ptr CLong -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_bool , Ptr CInt -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_string , Ptr (CString) -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_multivar , Ptr <git_config> -> CString -> CString -> FunPtr (CString -> Ptr () -> CInt) -> Ptr () -> IO (CInt)
#ccall git_config_set_int32 , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_set_int64 , Ptr <git_config> -> CString -> CLong -> IO (CInt)
#ccall git_config_set_bool , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_set_string , Ptr <git_config> -> CString -> CString -> IO (CInt)
#ccall git_config_set_multivar , Ptr <git_config> -> CString -> CString -> CString -> IO (CInt)
#ccall git_config_delete , Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_foreach , Ptr <git_config> -> FunPtr (CString -> CString -> Ptr () -> CInt) -> Ptr () -> IO (CInt)
#ccall git_config_get_mapped , Ptr CInt -> Ptr <git_config> -> CString -> Ptr <git_cvar_map> -> CSize -> IO (CInt)
