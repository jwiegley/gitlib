module Main where

import           Bindings.Libgit2
import           Control.Monad(when)
import qualified Data.ByteString as B
import           Data.Git
import           Data.Text as T hiding (map)
import qualified Data.Text.Encoding as E
import           Filesystem.Path.CurrentOS
import           Foreign
import           Foreign.C.String
import           Prelude hiding (FilePath)
import           System.Exit
import           System.Process(system)

main = do
  putStrLn "Creating Git repository..."
  system "git init smoke.git"

  putStrLn "Accessing directly..."
  alloca $ \ptr -> do
    withCString "smoke.git/.git" $ \str -> do
      r <- c'git_repository_open ptr str
      when (r < 0) $ exitWith (ExitFailure 1)
      peek ptr >>= c'git_repository_free

  putStrLn "Accessing via higher-level types..."
  repo <- openRepository (fromText (T.pack "smoke.git/.git"))
  writeBlob $ createBlob repo (E.encodeUtf8 (T.pack "Hello, world!\n"))