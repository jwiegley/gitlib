{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}

module Main where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Data.Git
import           Data.Maybe
import           Data.Monoid
import           Data.Text as T hiding (map)
import qualified Data.Text.Encoding as E
import           Data.Traversable
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS
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

checkOid :: Updatable a => a -> Text -> Assertion
checkOid obj ident = void $
  (@?=) <$> (T.pack . show <$> objectId obj)
        <*> pure ident

tests :: Test
tests = test [

  "singleBlob" ~:

  withRepository "singleBlob.git" $ \repo -> do
    update_ $ createBlob repo (E.encodeUtf8 "Hello, world!\n")

    (@?=) <$> catBlob repo "af5626b4a114abcb82d63db7c8082c3c4756e51b"
          <*> pure (Just "Hello, world!\n")
    (@?=) <$> catBlob repo "af5626b"
          <*> pure (Just "Hello, world!\n")
    return ()

  , "singleTree" ~:

  withRepository "singleTree.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
        tr    = updateTree "hello/world.txt"
                           (BlobEntry hello False) (createTree repo)

    checkOid tr "c0c848a2737a6a8533a18e6bd4d04266225e0271"

  , "twoTrees" ~:

  withRepository "twoTrees.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
        tr    = updateTree "hello/world.txt"
                           (BlobEntry hello False) (createTree repo)

    checkOid tr "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
        tr'     = updateTree "goodbye/files/world.txt"
                             (BlobEntry goodbye False) tr

    checkOid tr' "98c3f387f63c08e1ea1019121d623366ff04de7a"

  ]

-- Main.hs ends here
