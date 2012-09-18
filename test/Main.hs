{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable
import           Data.Git
import           Data.Monoid
import           Data.Text as T hiding (map)
import           Data.Text.IO
import qualified Data.Text.Encoding as E
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath, putStr, putStrLn)

default (Text)

main :: IO ()
main = do
  putStrLn "Accessing via higher-level types..."
  repo <- createRepository (fromText "smoke.git") True
  update_ $ createBlob repo (E.encodeUtf8 "Hello, world!\n")

  let bl = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
      tr = createTree repo "hello/world.txt" (BlobEntry bl False)
  oid <- objectId tr
  putStrLn $ "Wrote tree: " <> T.pack (show oid)

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
