{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/blame.h>
module Bindings.Libgit2.Blame where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Types
{- typedef enum {
            GIT_BLAME_NORMAL = 0,
            GIT_BLAME_TRACK_COPIES_SAME_FILE = 1 << 0,
            GIT_BLAME_TRACK_COPIES_SAME_COMMIT_MOVES = 1 << 1,
            GIT_BLAME_TRACK_COPIES_SAME_COMMIT_COPIES = 1 << 2,
            GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES = 1 << 3,
            GIT_BLAME_FIRST_PARENT = 1 << 4,
            GIT_BLAME_USE_MAILMAP = 1 << 5,
            GIT_BLAME_IGNORE_WHITESPACE = 1 << 6
        } git_blame_flag_t; -}
#integral_t git_blame_flag_t
#num GIT_BLAME_NORMAL
#num GIT_BLAME_TRACK_COPIES_SAME_FILE
#num GIT_BLAME_TRACK_COPIES_SAME_COMMIT_MOVES
#num GIT_BLAME_TRACK_COPIES_SAME_COMMIT_COPIES
#num GIT_BLAME_TRACK_COPIES_ANY_COMMIT_COPIES
#num GIT_BLAME_FIRST_PARENT
#num GIT_BLAME_USE_MAILMAP
#num GIT_BLAME_IGNORE_WHITESPACE
{- typedef struct git_blame_options {
            unsigned int version;
            uint32_t flags;
            uint16_t min_match_characters;
            git_oid newest_commit;
            git_oid oldest_commit;
            size_t min_line;
            size_t max_line;
        } git_blame_options; -}
#starttype struct git_blame_options
#field version , CUInt
#field flags , CUInt
#field min_match_characters , CUInt
#field newest_commit , <struct git_oid>
#field oldest_commit , <struct git_oid>
#field min_line , CSize
#field max_line , CSize
#stoptype
#ccall git_blame_options_init , Ptr <struct git_blame_options> -> CUInt -> IO CInt
{- typedef struct git_blame_hunk {
            size_t lines_in_hunk;
            git_oid final_commit_id;
            size_t final_start_line_number;
            git_signature * final_signature;
            git_oid orig_commit_id;
            const char * orig_path;
            size_t orig_start_line_number;
            git_signature * orig_signature;
            char boundary;
        } git_blame_hunk; -}
#starttype struct git_blame_hunk
#field lines_in_hunk , CSize
#field final_commit_id , <struct git_oid>
#field final_start_line_number , CSize
#field final_signature , Ptr <struct git_signature>
#field orig_commit_id , <struct git_oid>
#field orig_path , CString
#field orig_start_line_number , CSize
#field orig_signature , Ptr <struct git_signature>
#field boundary , CChar
#stoptype
{- typedef struct git_blame git_blame; -}
#opaque_t struct git_blame
#ccall git_blame_get_hunk_count , Ptr <struct git_blame> -> IO CUInt
#ccall git_blame_get_hunk_byindex , Ptr <struct git_blame> -> CUInt -> IO (Ptr <struct git_blame_hunk>)
#ccall git_blame_get_hunk_byline , Ptr <struct git_blame> -> CSize -> IO (Ptr <struct git_blame_hunk>)
#ccall git_blame_file , Ptr (Ptr <struct git_blame>) -> Ptr <struct git_repository> -> CString -> Ptr <struct git_blame_options> -> IO CInt
#ccall git_blame_buffer , Ptr (Ptr <struct git_blame>) -> Ptr <struct git_blame> -> CString -> CSize -> IO CInt
#ccall git_blame_free , Ptr <struct git_blame> -> IO ()
