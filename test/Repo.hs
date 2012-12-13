module Main where
import Bindings.Libgit2
import Foreign
import Foreign.C.String
import System.Exit
import Control.Monad(when)
import System.Process(system)

main = do
  alloca $ \ptr -> do
    system "git init smoke.git" 
    withCString "smoke.git/.git" $ \str -> do
      r <- c'git_repository_open ptr str
      when (r < 0) $ exitWith (ExitFailure 1) 
      peek ptr >>= c'git_repository_free