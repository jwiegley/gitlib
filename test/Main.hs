module Main where
import Bindings.Libgit2
import Foreign
import Foreign.C.String

main = do
  alloca $ \ptr -> do
    withCString "foobar.git/.git" $ \str -> do
      c'git_repository_open ptr str
      peek ptr >>= c'git_repository_free