{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/patch.h>
module Bindings.Libgit2.Patch where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Types
import Bindings.Libgit2.Oid
import Bindings.Libgit2.Diff
import Bindings.Libgit2.Buffer
{- typedef struct git_patch git_patch; -}
#opaque_t struct git_patch
#ccall git_patch_owner , Ptr <struct git_patch> -> IO (Ptr <struct git_repository>)
#ccall git_patch_from_diff , Ptr (Ptr <struct git_patch>) -> Ptr <struct git_diff> -> CSize -> IO CInt
#ccall git_patch_from_blobs , Ptr (Ptr <struct git_patch>) -> Ptr <struct git_blob> -> CString -> Ptr <struct git_blob> -> CString -> Ptr <git_diff_options> -> IO CInt
#ccall git_patch_from_blob_and_buffer , Ptr (Ptr <struct git_patch>) -> Ptr <struct git_blob> -> CString -> Ptr () -> CSize -> CString -> Ptr <git_diff_options> -> IO CInt
#ccall git_patch_from_buffers , Ptr (Ptr <struct git_patch>) -> Ptr () -> CSize -> CString -> Ptr () -> CSize -> CString -> Ptr <git_diff_options> -> IO CInt
#ccall git_patch_free , Ptr <struct git_patch> -> IO ()
#ccall git_patch_get_delta , Ptr <struct git_patch> -> IO (Ptr <git_diff_delta>)
#ccall git_patch_num_hunks , Ptr <struct git_patch> -> IO CSize
#ccall git_patch_line_stats , Ptr CSize -> Ptr CSize -> Ptr CSize -> Ptr <struct git_patch> -> IO CInt
#ccall git_patch_get_hunk , Ptr (Ptr <git_diff_hunk>) -> Ptr CSize -> Ptr <struct git_patch> -> CSize -> IO CInt
#ccall git_patch_num_lines_in_hunk , Ptr <struct git_patch> -> CSize -> IO CInt
#ccall git_patch_get_line_in_hunk , Ptr (Ptr <git_diff_line>) -> Ptr <struct git_patch> -> CSize -> CSize -> IO CInt
#ccall git_patch_size , Ptr <struct git_patch> -> CInt -> CInt -> CInt -> IO CSize
#ccall git_patch_print , Ptr <struct git_patch> -> <git_diff_line_cb> -> Ptr () -> IO CInt
#ccall git_patch_to_buf , Ptr <git_buf> -> Ptr <struct git_patch> -> IO CInt
