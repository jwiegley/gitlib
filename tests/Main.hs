{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Bindings.Libgit2
import           Control.Monad
import           Data.Foldable
import           Data.Git
import           Data.Text as T hiding (map)
import           Data.Text.IO
import qualified Data.Text.Encoding as E
import           Filesystem.Path.CurrentOS
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Storable
import           Prelude hiding (FilePath, putStr, putStrLn)
import           System.Exit
import           System.Process(system)

default (Text)

main :: IO ()
main = do
  putStrLn "Creating Git repository..."

  _ <- system "git init smoke.git"

  putStrLn "Accessing directly..."

  alloca $ \ptr -> do
    withCString "smoke.git/.git" $ \str -> do
      r <- c'git_repository_open ptr str
      when (r < 0) $ exitWith (ExitFailure 1)
      peek ptr >>= c'git_repository_free

  putStrLn "Accessing via higher-level types..."

  repo <- openRepository (fromText "smoke.git/.git")
  update_ $ createBlob repo (E.encodeUtf8 "Hello, world!\n")

  putStrLn "Looking up Blob by its full SHA..."
  catBlob repo "af5626b4a114abcb82d63db7c8082c3c4756e51b"

  putStrLn "Looking up Blob by its short SHA..."
  catBlob repo "af5626b"

catBlob :: Repository -> Text -> IO ()
catBlob repo sha = do
  hash <- stringToOid sha
  print hash
  for_ hash $ \hash' -> do
    obj <- lookupObject repo hash'
    case obj of
      Just (BlobObj b) -> do
        putStrLn "Found a blob, contents: "
        (_, contents) <- getBlobContents b
        putStr (E.decodeUtf8 contents)

      Just _  -> error "Found something else..."
      Nothing -> error "Didn't find anything :("

-- Main.hs ends here
