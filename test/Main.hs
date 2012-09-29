{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Lens
import           Control.Monad
import           Data.Git
import           Data.Maybe
import           Data.Text as T hiding (map)
import qualified Data.Text.Encoding as E
import           Data.Time
import           Data.Traversable
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS
import qualified Prelude
import           Prelude hiding (FilePath, putStr, putStrLn)
import           System.Exit
import           Test.HUnit

default (Text)

main :: IO ()
main = do
  counts' <- runTestTT tests
  case counts' of
    Counts _ _ errors' failures' ->
      if errors' > 0 || failures' > 0
      then exitFailure
      else exitSuccess

catBlob :: Repository -> Text -> IO (Maybe Text)
catBlob repo sha = do
  hash <- stringToOid sha
  for hash $ \hash' -> do
    obj <- lookupObject repo hash'
    case obj of
      Just (BlobObj b) -> do
        (_, contents) <- getBlobContents b
        return (E.decodeUtf8 contents)

      Just _  -> error "Found something else..."
      Nothing -> error "Didn't find anything :("

withRepository :: Text -> (Repository -> Assertion) -> Assertion
withRepository n f = do
  let p = fromText n
  exists <- isDirectory p
  when exists $ removeTree p

  -- we want exceptions to leave the repo behind
  f =<< createRepository p True

  removeTree p

oid :: Updatable a => a -> IO Text
oid = objectId >=> return . T.pack . show

tests :: Test
tests = test [

  "singleBlob" ~:

  withRepository "singleBlob.git" $ \repo -> do
    update_ $ createBlob repo (E.encodeUtf8 "Hello, world!\n")

    x <- catBlob repo "af5626b4a114abcb82d63db7c8082c3c4756e51b"
    (@?=) x (Just "Hello, world!\n")

    x <- catBlob repo "af5626b"
    (@?=) x (Just "Hello, world!\n")

    return ()

  , "singleTree" ~:

  withRepository "singleTree.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree "hello/world.txt" (blobRef hello) (createTree repo)
    x  <- oid tr
    (@?=) x "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    return()

  , "twoTrees" ~:

  withRepository "twoTrees.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree "hello/world.txt" (blobRef hello) (createTree repo)
    x  <- oid tr
    (@?=) x "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree "goodbye/files/world.txt" (blobRef goodbye) tr
    x  <- oid tr
    (@?=) x "7757b6029419d8d2f80c688b8403dfbb7e634003"

    return()

  , "deleteTree" ~:

  withRepository "deleteTree.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree "hello/world.txt" (blobRef hello) (createTree repo)
    x  <- oid tr
    (@?=) x "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree "goodbye/files/world.txt" (blobRef goodbye) tr
    x  <- oid tr
    (@?=) x "7757b6029419d8d2f80c688b8403dfbb7e634003"

    -- Confirm that deleting world.txt also deletes the now-empty subtree
    -- goodbye/files, which also deletes the then-empty subtree goodbye,
    -- returning us back the original tree.
    tr <- removeFromTree "goodbye/files/world.txt" tr
    x  <- oid tr
    (@?=) x "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    return()

  , "createCommit" ~:

  withRepository "createCommit.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree "hello/world.txt" (blobRef hello)
                    (createTree repo)

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree "goodbye/files/world.txt" (blobRef goodbye) tr
    tr <- removeFromTree "goodbye/files/world.txt" tr

    -- The Oid has been cleared in tr, so this tests that it gets written as
    -- needed.
    now <- getCurrentTime
    let sig = Signature { _signatureName  = "John Wiegley"
                        , _signatureEmail = "johnw@newartisans.com"
                        , _signatureWhen  = now }
    c <- update
         $ commitTree      .~ ObjRef tr
         $ commitAuthor    .~ sig
         $ commitCommitter .~ sig
         $ commitLog       .~ "Sample log message."
         $ createCommit repo
    x <- oid c
    (@?=) x "05831d8210899a4e25a03e52464f45b636bb0a2b"

    return()

  ]

-- Main.hs ends here
