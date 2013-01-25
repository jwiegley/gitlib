{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Bindings.Libgit2.OdbBackend
import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.Monad
import           Data.Git
import           Data.Git.Backend
import           Data.Git.Backend.GitHub
import           Data.Git.Backend.Trace
import           Data.Maybe
import           Data.Text as T hiding (map)
import qualified Data.Text.Encoding as E
import           Data.Time.Clock.POSIX
import           Data.Traversable
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Prelude
import           Prelude (putStrLn)
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
  stopGlobalPool

catBlob :: Repository -> Text -> IO (Maybe Text)
catBlob repo sha = do
  hash <- parseOid sha
  for hash $ \hash' -> do
    obj <- lookupObject repo hash'
    case obj of
      Just (BlobObj b) -> do
        (_, contents) <- getBlobContents b
        str <- blobSourceToString contents
        case str of
          Nothing   -> return T.empty
          Just str' -> return (E.decodeUtf8 str')

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
oid = objectId >=> return . oidToText . Just

oidToText :: Maybe Oid -> Text
oidToText = T.pack . show . fromJust

sampleCommit :: Repository -> Tree -> Signature -> Commit
sampleCommit repo tr sig =
    (createCommit repo sig) { commitTree = ObjRef tr
                            , commitLog  = "Sample log message." }

tests :: Test
tests = test [

  "singleBlob" ~:

  withRepository "singleBlob.git" $ \repo' -> do
    repo <- createGitHubBackend "jwiegley" "gitlib" Nothing Nothing True repo'

    x <- catBlob repo "ad2ba2e3b362db844025c49d66011d0ca9e86640"
    let y = Prelude.head . T.lines <$> x
    y @?= Just "Copyright (c) 2012 John Wiegley"

    return ()

  ]

-- Main.hs ends here
