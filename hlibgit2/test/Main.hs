module Main where

import Bindings.Libgit2
import Control.Monad
import Foreign.C.String
import Foreign.Marshal.Alloc
import Foreign.Storable
import Prelude
import System.Exit
import System.Process (system)

main :: IO ()
main = withLibGitDo $ do
  putStrLn "Creating Git repository..."
  _ <- system "git init smoke.git"

  putStrLn "Accessing directly..."
  alloca $ \ptr -> do
    withCString "smoke.git/.git" $ \str -> do
      r <- c'git_repository_open ptr str
      when (r < 0) $ exitWith (ExitFailure 1)
      peek ptr >>= c'git_repository_free

-- Main.hs ends here
