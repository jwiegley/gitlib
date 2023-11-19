{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/config.h>
module Bindings.Libgit2.Config where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Buffer
{- typedef enum {
            GIT_CONFIG_LEVEL_PROGRAMDATA = 1,
            GIT_CONFIG_LEVEL_SYSTEM = 2,
            GIT_CONFIG_LEVEL_XDG = 3,
            GIT_CONFIG_LEVEL_GLOBAL = 4,
            GIT_CONFIG_LEVEL_LOCAL = 5,
            GIT_CONFIG_LEVEL_APP = 6,
            GIT_CONFIG_HIGHEST_LEVEL = -1
        } git_config_level_t; -}
#integral_t git_config_level_t
#num GIT_CONFIG_LEVEL_PROGRAMDATA
#num GIT_CONFIG_LEVEL_SYSTEM
#num GIT_CONFIG_LEVEL_XDG
#num GIT_CONFIG_LEVEL_GLOBAL
#num GIT_CONFIG_LEVEL_LOCAL
#num GIT_CONFIG_LEVEL_APP
#num GIT_CONFIG_HIGHEST_LEVEL
{- typedef struct git_config_entry {
            const char * name;
            const char * value;
            const char * backend_type;
            const char * origin_path;
            unsigned int include_depth;
            git_config_level_t level;
            void (* free)(struct git_config_entry * entry);
        } git_config_entry; -}
#starttype struct git_config_entry
#field name , CString
#field value , CString
#field backend_type , CString
#field origin_path , CString
#field include_depth , CUInt
#field level , <git_config_level_t>
#field free , FunPtr (Ptr <struct git_config_entry> -> IO ())
#stoptype
#ccall git_config_entry_free , Ptr <struct git_config_entry> -> IO ()
#callback git_config_foreach_cb , Ptr <struct git_config_entry> -> Ptr () -> IO CInt
{- typedef struct git_config_iterator git_config_iterator; -}
#opaque_t struct git_config_iterator
{- typedef enum {
            GIT_CONFIGMAP_FALSE = 0,
            GIT_CONFIGMAP_TRUE = 1,
            GIT_CONFIGMAP_INT32,
            GIT_CONFIGMAP_STRING
        } git_configmap_t; -}
#integral_t git_configmap_t
#num GIT_CONFIGMAP_FALSE
#num GIT_CONFIGMAP_TRUE
#num GIT_CONFIGMAP_INT32
#num GIT_CONFIGMAP_STRING
{- typedef struct {
            git_configmap_t type; const char * str_match; int map_value;
        } git_configmap; -}
#starttype git_configmap
#field type , <git_configmap_t>
#field str_match , CString
#field map_value , CInt
#stoptype
#ccall git_config_find_global , Ptr <git_buf> -> IO CInt
#ccall git_config_find_xdg , Ptr <git_buf> -> IO CInt
#ccall git_config_find_system , Ptr <git_buf> -> IO CInt
#ccall git_config_find_programdata , Ptr <git_buf> -> IO CInt
#ccall git_config_open_default , Ptr (Ptr <struct git_config>) -> IO CInt
#ccall git_config_new , Ptr (Ptr <struct git_config>) -> IO CInt
#ccall git_config_add_file_ondisk , Ptr <struct git_config> -> CString -> <git_config_level_t> -> Ptr <struct git_repository> -> CInt -> IO CInt
#ccall git_config_open_ondisk , Ptr (Ptr <struct git_config>) -> CString -> IO CInt
#ccall git_config_open_level , Ptr (Ptr <struct git_config>) -> Ptr <struct git_config> -> <git_config_level_t> -> IO CInt
#ccall git_config_open_global , Ptr (Ptr <struct git_config>) -> Ptr <struct git_config> -> IO CInt
#ccall git_config_snapshot , Ptr (Ptr <struct git_config>) -> Ptr <struct git_config> -> IO CInt
#ccall git_config_free , Ptr <struct git_config> -> IO ()
#ccall git_config_get_entry , Ptr (Ptr <struct git_config_entry>) -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_int32 , Ptr CInt -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_int64 , Ptr CLong -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_bool , Ptr CInt -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_path , Ptr <git_buf> -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_string , Ptr CString -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_string_buf , Ptr <git_buf> -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_get_multivar_foreach , Ptr <struct git_config> -> CString -> CString -> <git_config_foreach_cb> -> Ptr () -> IO CInt
#ccall git_config_multivar_iterator_new , Ptr (Ptr <struct git_config_iterator>) -> Ptr <struct git_config> -> CString -> CString -> IO CInt
#ccall git_config_next , Ptr (Ptr <struct git_config_entry>) -> Ptr <struct git_config_iterator> -> IO CInt
#ccall git_config_iterator_free , Ptr <struct git_config_iterator> -> IO ()
#ccall git_config_set_int32 , Ptr <struct git_config> -> CString -> CInt -> IO CInt
#ccall git_config_set_int64 , Ptr <struct git_config> -> CString -> CLong -> IO CInt
#ccall git_config_set_bool , Ptr <struct git_config> -> CString -> CInt -> IO CInt
#ccall git_config_set_string , Ptr <struct git_config> -> CString -> CString -> IO CInt
#ccall git_config_set_multivar , Ptr <struct git_config> -> CString -> CString -> CString -> IO CInt
#ccall git_config_delete_entry , Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_delete_multivar , Ptr <struct git_config> -> CString -> CString -> IO CInt
#ccall git_config_foreach , Ptr <struct git_config> -> <git_config_foreach_cb> -> Ptr () -> IO CInt
#ccall git_config_iterator_new , Ptr (Ptr <struct git_config_iterator>) -> Ptr <struct git_config> -> IO CInt
#ccall git_config_iterator_glob_new , Ptr (Ptr <struct git_config_iterator>) -> Ptr <struct git_config> -> CString -> IO CInt
#ccall git_config_foreach_match , Ptr <struct git_config> -> CString -> <git_config_foreach_cb> -> Ptr () -> IO CInt
#ccall git_config_get_mapped , Ptr CInt -> Ptr <struct git_config> -> CString -> Ptr <git_configmap> -> CSize -> IO CInt
#ccall git_config_lookup_map_value , Ptr CInt -> Ptr <git_configmap> -> CSize -> CString -> IO CInt
#ccall git_config_parse_bool , Ptr CInt -> CString -> IO CInt
#ccall git_config_parse_int32 , Ptr CInt -> CString -> IO CInt
#ccall git_config_parse_int64 , Ptr CLong -> CString -> IO CInt
#ccall git_config_parse_path , Ptr <git_buf> -> CString -> IO CInt
#ccall git_config_backend_foreach_match , Ptr <struct git_config_backend> -> CString -> <git_config_foreach_cb> -> Ptr () -> IO CInt
#ccall git_config_lock , Ptr (Ptr <struct git_transaction>) -> Ptr <struct git_config> -> IO CInt
