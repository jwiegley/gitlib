module Data.Git.Foreign
       ( Hash, Ident, ObjPtr )
       where

import Bindings.Libgit2
import Foreign.ForeignPtr

type Hash     = C'git_oid
type Ident a  = Either (a -> IO Hash) Hash
type ObjPtr a = Maybe (ForeignPtr a)

-- Foreign.hs
