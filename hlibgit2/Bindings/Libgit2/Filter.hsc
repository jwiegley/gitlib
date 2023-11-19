{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/filter.h>
module Bindings.Libgit2.Filter where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Buffer
{- typedef enum {
            GIT_FILTER_TO_WORKTREE = 0,
            GIT_FILTER_SMUDGE = GIT_FILTER_TO_WORKTREE,
            GIT_FILTER_TO_ODB = 1,
            GIT_FILTER_CLEAN = GIT_FILTER_TO_ODB
        } git_filter_mode_t; -}
#integral_t git_filter_mode_t
#num GIT_FILTER_TO_WORKTREE
#num GIT_FILTER_SMUDGE
#num GIT_FILTER_TO_ODB
#num GIT_FILTER_CLEAN
{- typedef enum {
            GIT_FILTER_DEFAULT = 0u,
            GIT_FILTER_ALLOW_UNSAFE = 1u << 0,
            GIT_FILTER_NO_SYSTEM_ATTRIBUTES = 1u << 1,
            GIT_FILTER_ATTRIBUTES_FROM_HEAD = 1u << 2,
            GIT_FILTER_ATTRIBUTES_FROM_COMMIT = 1u << 3
        } git_filter_flag_t; -}
#integral_t git_filter_flag_t
#num GIT_FILTER_DEFAULT
#num GIT_FILTER_ALLOW_UNSAFE
#num GIT_FILTER_NO_SYSTEM_ATTRIBUTES
#num GIT_FILTER_ATTRIBUTES_FROM_HEAD
#num GIT_FILTER_ATTRIBUTES_FROM_COMMIT
{- typedef struct {
            unsigned int version;
            uint32_t flags;
            git_oid * commit_id;
            git_oid attr_commit_id;
        } git_filter_options; -}
#starttype git_filter_options
#field version , CUInt
#field flags , CUInt
#field commit_id , Ptr <struct git_oid>
#field attr_commit_id , <struct git_oid>
#stoptype
{- typedef struct git_filter git_filter; -}
#opaque_t struct git_filter
{- typedef struct git_filter_list git_filter_list; -}
#opaque_t struct git_filter_list
#ccall git_filter_list_load , Ptr (Ptr <struct git_filter_list>) -> Ptr <struct git_repository> -> Ptr <struct git_blob> -> CString -> <git_filter_mode_t> -> CUInt -> IO CInt
#ccall git_filter_list_load_ext , Ptr (Ptr <struct git_filter_list>) -> Ptr <struct git_repository> -> Ptr <struct git_blob> -> CString -> <git_filter_mode_t> -> Ptr <git_filter_options> -> IO CInt
#ccall git_filter_list_contains , Ptr <struct git_filter_list> -> CString -> IO CInt
#ccall git_filter_list_apply_to_buffer , Ptr <git_buf> -> Ptr <struct git_filter_list> -> CString -> CSize -> IO CInt
#ccall git_filter_list_apply_to_file , Ptr <git_buf> -> Ptr <struct git_filter_list> -> Ptr <struct git_repository> -> CString -> IO CInt
#ccall git_filter_list_apply_to_blob , Ptr <git_buf> -> Ptr <struct git_filter_list> -> Ptr <struct git_blob> -> IO CInt
#ccall git_filter_list_stream_buffer , Ptr <struct git_filter_list> -> CString -> CSize -> Ptr <struct git_writestream> -> IO CInt
#ccall git_filter_list_stream_file , Ptr <struct git_filter_list> -> Ptr <struct git_repository> -> CString -> Ptr <struct git_writestream> -> IO CInt
#ccall git_filter_list_stream_blob , Ptr <struct git_filter_list> -> Ptr <struct git_blob> -> Ptr <struct git_writestream> -> IO CInt
#ccall git_filter_list_free , Ptr <struct git_filter_list> -> IO ()
