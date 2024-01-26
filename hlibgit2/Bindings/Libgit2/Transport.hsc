{-# OPTIONS_GHC -fno-warn-unused-imports #-}
#include <bindings.dsl.h>
#include <git2/transport.h>
module Bindings.Libgit2.Transport where
import Foreign.Ptr
#strict_import

import Bindings.Libgit2.Indexer
import Bindings.Libgit2.Net
import Bindings.Libgit2.Types
import Bindings.Libgit2.Cert
import Bindings.Libgit2.Credential
#callback git_transport_message_cb , CString -> CInt -> Ptr () -> IO CInt
#callback git_transport_cb , Ptr (Ptr <struct git_transport>) -> Ptr <struct git_remote> -> Ptr () -> IO CInt
