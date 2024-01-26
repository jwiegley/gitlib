{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/message.h>
module Bindings.Libgit2.Message where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Common
import Bindings.Libgit2.Buffer
#ccall git_message_prettify , Ptr <git_buf> -> CString -> CInt -> CChar -> IO CInt
{- typedef struct {
            const char * key; const char * value;
        } git_message_trailer; -}
#starttype git_message_trailer
#field key , CString
#field value , CString
#stoptype
{- typedef struct {
            git_message_trailer * trailers;
            size_t count;
            char * _trailer_block;
        } git_message_trailer_array; -}
#starttype git_message_trailer_array
#field trailers , Ptr <git_message_trailer>
#field count , CSize
#field _trailer_block , CString
#stoptype
#ccall git_message_trailers , Ptr <git_message_trailer_array> -> CString -> IO CInt
#ccall git_message_trailer_array_free , Ptr <git_message_trailer_array> -> IO ()
