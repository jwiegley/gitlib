{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2.h>
module Bindings.Libgit2.Config where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
{- enum {
    GIT_CONFIG_LEVEL_SYSTEM = 1,
    GIT_CONFIG_LEVEL_XDG = 2,
    GIT_CONFIG_LEVEL_GLOBAL = 3,
    GIT_CONFIG_LEVEL_LOCAL = 4,
    GIT_CONFIG_HIGHEST_LEVEL = -1
}; -}
#num GIT_CONFIG_LEVEL_SYSTEM
#num GIT_CONFIG_LEVEL_XDG
#num GIT_CONFIG_LEVEL_GLOBAL
#num GIT_CONFIG_LEVEL_LOCAL
#num GIT_CONFIG_HIGHEST_LEVEL
{- typedef struct {
            const char * name; const char * value; unsigned int level;
        } git_config_entry; -}
#starttype git_config_entry
#field name , CString
#field value , CString
#field level , CUInt
#stoptype
{- typedef int (* git_config_foreach_cb)(const git_config_entry *,
                                      void *); -}
#callback git_config_foreach_cb , Ptr (<git_config_entry>) -> Ptr () -> IO CInt
{- struct git_config_backend {
    unsigned int version;
    struct git_config * cfg;
    int (* open)(struct git_config_backend *, unsigned int level);
    int (* get)(const struct git_config_backend *,
                const char * key,
                const git_config_entry * * entry);
    int (* get_multivar)(struct git_config_backend *,
                         const char * key,
                         const char * regexp,
                         git_config_foreach_cb callback,
                         void * payload);
    int (* set)(struct git_config_backend *,
                const char * key,
                const char * value);
    int (* set_multivar)(git_config_backend * cfg,
                         const char * name,
                         const char * regexp,
                         const char * value);
    int (* del)(struct git_config_backend *, const char * key);
    int (* foreach)(struct git_config_backend *,
                    const char *,
                    git_config_foreach_cb callback,
                    void * payload);
    int (* refresh)(struct git_config_backend *);
    void (* free)(struct git_config_backend *);
}; -}
#callback git_config_backend_open_callback , Ptr <git_config_backend> -> CUInt -> IO CInt
#callback git_config_backend_get_callback , Ptr <git_config_backend> -> CString -> Ptr (Ptr <git_config_entry>) -> IO CInt
#callback git_config_backend_get_multivar_callback , Ptr <git_config_backend> -> CString -> CString -> <git_config_foreach_cb> -> Ptr () -> IO CInt
#callback git_config_backend_set_callback , Ptr <git_config_backend> -> CString -> CString -> IO CInt
#callback git_config_backend_set_multivar_callback , Ptr <git_config_backend> -> CString -> CString -> CString -> IO CInt
#callback git_config_backend_del_callback , Ptr <git_config_backend> -> CString -> IO CInt
#callback git_config_backend_foreach_callback , Ptr <git_config_backend> -> CString -> <git_config_foreach_cb> -> Ptr () -> IO CInt
#callback git_config_backend_refresh_callback , Ptr <git_config_backend> -> IO CInt
#callback git_config_backend_free_callback , Ptr <git_config_backend> -> IO ()
#starttype git_config_backend
#field version , CUInt
#field cfg , Ptr <git_config>
#field open , <git_config_backend_open_callback>
#field get , <git_config_backend_get_callback>
#field get_multivar , <git_config_backend_get_multivar_callback>
#field set , <git_config_backend_set_callback>
#field set_multivar , <git_config_backend_set_multivar_callback>
#field del , <git_config_backend_del_callback>
#field foreach , <git_config_backend_foreach_callback>
#field refresh , <git_config_backend_refresh_callback>
#field free , <git_config_backend_free_callback>
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
#ccall git_config_find_xdg , CString -> CSize -> IO (CInt)
#ccall git_config_find_system , CString -> CSize -> IO (CInt)
#ccall git_config_open_default , Ptr (Ptr <git_config>) -> IO (CInt)
#ccall git_config_new , Ptr (Ptr <git_config>) -> IO (CInt)
#ccall git_config_add_backend , Ptr <git_config> -> Ptr <git_config_backend> -> CUInt -> CInt -> IO (CInt)
#ccall git_config_add_file_ondisk , Ptr <git_config> -> CString -> CUInt -> CInt -> IO (CInt)
#ccall git_config_open_ondisk , Ptr (Ptr <git_config>) -> CString -> IO (CInt)
#ccall git_config_open_level , Ptr (Ptr <git_config>) -> Ptr <git_config> -> CUInt -> IO (CInt)
#ccall git_config_refresh , Ptr <git_config> -> IO (CInt)
#ccall git_config_free , Ptr <git_config> -> IO ()
#ccall git_config_get_entry , Ptr (Ptr <git_config_entry>) -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_int32 , Ptr CInt -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_int64 , Ptr CLong -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_bool , Ptr CInt -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_string , Ptr (CString) -> Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_get_multivar , Ptr <git_config> -> CString -> CString -> <git_config_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_config_set_int32 , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_set_int64 , Ptr <git_config> -> CString -> CLong -> IO (CInt)
#ccall git_config_set_bool , Ptr <git_config> -> CString -> CInt -> IO (CInt)
#ccall git_config_set_string , Ptr <git_config> -> CString -> CString -> IO (CInt)
#ccall git_config_set_multivar , Ptr <git_config> -> CString -> CString -> CString -> IO (CInt)
#ccall git_config_delete_entry , Ptr <git_config> -> CString -> IO (CInt)
#ccall git_config_foreach , Ptr <git_config> -> <git_config_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_config_foreach_match , Ptr <git_config> -> CString -> <git_config_foreach_cb> -> Ptr () -> IO (CInt)
#ccall git_config_get_mapped , Ptr CInt -> Ptr <git_config> -> CString -> Ptr <git_cvar_map> -> CSize -> IO (CInt)
#ccall git_config_lookup_map_value , Ptr CInt -> Ptr <git_cvar_map> -> CSize -> CString -> IO (CInt)
#ccall git_config_parse_bool , Ptr CInt -> CString -> IO (CInt)
#ccall git_config_parse_int32 , Ptr CInt -> CString -> IO (CInt)
#ccall git_config_parse_int64 , Ptr CLong -> CString -> IO (CInt)
