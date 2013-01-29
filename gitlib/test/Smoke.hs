{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Bindings.Libgit2.OdbBackend
import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Maybe
import           Data.Tagged
import           Data.Text as T hiding (map)
import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
import           Data.Traversable
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS
import           Foreign.C.String
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import           Git
import qualified Git.Libgit2 as Lg
import           Git.Utils
import qualified Prelude
import           Prelude (putStrLn)
import           Prelude hiding (FilePath, putStr, putStrLn)
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()

default (Text)

withNewRepository :: FilePath -> Lg.LgRepository () -> IO ()
withNewRepository dir action = do
  exists <- isDirectory dir
  when exists $ removeTree dir
  a <- Lg.withLgRepository dir True action
  -- we want exceptions to leave the repo behind
  removeTree dir
  return a

sampleCommit :: Repository m => Tree m -> Signature -> m (Commit m)
sampleCommit tr sig =
    createCommit [] (treeRef tr) sig sig "Sample log message." Nothing

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Smoke tests" $ do
    it "create a single blob" $ do
        withNewRepository "singleBlob.git" $ do
          createBlobUtf8 "Hello, world!\n"

          x <- catBlob "af5626b4a114abcb82d63db7c8082c3c4756e51b"
          liftIO $ x @?= "Hello, world!\n"

          x <- catBlob "af5626b"
          liftIO $ x @?= "Hello, world!\n"

    it "create a single tree" $ do
        withNewRepository "singleTree.git" $ do
          hello <- createBlobUtf8 "Hello, world!\n"
          tr <- newTree
          putBlobInTree tr "hello/world.txt" hello
          x <- oid tr
          liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    it "create two trees" $ do
        withNewRepository "twoTrees.git" $ do
          hello <- createBlobUtf8 "Hello, world!\n"
          tr <- newTree
          putBlobInTree tr "hello/world.txt" hello
          x <- oid tr
          liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

          goodbye <- createBlobUtf8 "Goodbye, world!\n"
          putBlobInTree tr "goodbye/files/world.txt" goodbye
          x <- oid tr
          liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

    it "delete an item from a tree" $ do
        withNewRepository "deleteTree.git" $ do
          hello <- createBlobUtf8 "Hello, world!\n"
          tr <- newTree
          putBlobInTree tr "hello/world.txt" hello
          x <- oid tr
          liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

          putBlobInTree tr "goodbye/files/world.txt"
              =<< createBlobUtf8 "Goodbye, world!\n"
          x <- oid tr
          liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

          -- Confirm that deleting world.txt also deletes the now-empty subtree
          -- goodbye/files, which also deletes the then-empty subtree goodbye,
          -- returning us back the original tree.
          dropFromTree tr "goodbye/files/world.txt"
          x <- oid tr
          liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    it "create a single commit" $ do
        withNewRepository "createCommit.git" $ do
          hello <- createBlobUtf8 "Hello, world!\n"
          tr <- newTree
          putBlobInTree tr "hello/world.txt" hello

          goodbye <- createBlobUtf8 "Goodbye, world!\n"
          putBlobInTree tr "goodbye/files/world.txt" goodbye
          x <- oid tr
          liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"
          liftIO $ putStrLn $ "step 1: " ++ show x

          -- The Oid has been cleared in tr, so this tests that it gets written as
          -- needed.
          let sig  = Signature {
                  signatureName  = "John Wiegley"
                , signatureEmail = "johnw@fpcomplete.com"
                , signatureWhen  = posixSecondsToUTCTime 1348980883 }

          liftIO $ putStrLn $ "step 2"
          c <- sampleCommit tr sig
          liftIO $ putStrLn $ "step 3"
          x <- oid c
          liftIO $ putStrLn $ "step 4"
          liftIO $ x @?= "44381a5e564d19893d783a5d5c59f9c745155b56"
          liftIO $ putStrLn $ "step 5: " ++ show x

  -- , "createTwoCommits" $ do

  -- withNewRepository "createTwoCommits.git" $ \repo -> alloca $ \loosePtr -> do
  --   withCString "createTwoCommits.git/objects" $ \objectsDir -> do
  --     r <- c'git_odb_backend_loose loosePtr objectsDir (-1) 0
  --     when (r < 0) $ error "Failed to create loose objects backend"
  --   -- jww (2013-01-27): Restore
  --   -- loosePtr' <- peek loosePtr
  --   -- backend   <- traceBackend loosePtr'
  --   -- odbBackendAdd backend 3

  --   let hello = createBlobUtf8 "Hello, world!\n"
  --   tr <- putBlobInTree newTree "hello/world.txt" (blobRef hello)

  --   let goodbye = createBlobUtf8 "Goodbye, world!\n"
  --   tr <- putBlobInTree tr "goodbye/files/world.txt" (blobRef goodbye)
  --   tr <- update tr
  --   x  <- oid tr
  --   x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

  --   -- The Oid has been cleared in tr, so this tests that it gets written as
  --   -- needed.
  --   let sig = Signature {
  --           signatureName  = "John Wiegley"
  --         , signatureEmail = "johnw@newartisans.com"
  --         , signatureWhen  = posixSecondsToUTCTime 1348980883 }
  --       c   = sampleCommit tr sig
  --   c <- update c
  --   x <- oid c
  --   x @?= "44381a5e564d19893d783a5d5c59f9c745155b56"

  --   let goodbye2 = createBlobUtf8 "Goodbye, world again!\n"
  --   tr <- putBlobInTree tr "goodbye/files/world.txt" (blobRef goodbye2)
  --   tr <- update tr
  --   x  <- oid tr
  --   x @?= "f2b42168651a45a4b7ce98464f09c7ec7c06d706"

  --   let sig = Signature {
  --           signatureName  = "John Wiegley"
  --         , signatureEmail = "johnw@newartisans.com"
  --         , signatureWhen  = posixSecondsToUTCTime 1348981883 }
  --       c2  = sampleCommit [commitRef c] tr sig sig
  --                          "Second sample log message."
  --   x <- oid c2
  --   x @?= "2506e7fcc2dbfe4c083e2bd741871e2e14126603"

  --   cid <- update c2
  --   updateRef_ "refs/heads/master" (RefOid cid)
  --   updateRef_ "HEAD" (RefSymbolic "refs/heads/master")

  --   x <- oidToText <$> resolveRef "refs/heads/master"
  --   x @?= "2506e7fcc2dbfe4c083e2bd741871e2e14126603"

  --   traverseRefs (\name -> Prelude.putStrLn $ "Ref: " ++ unpack name)

  --   -- jww (2013-01-27): Restore
  --   -- ehist <- commitHistoryFirstParent c2
  --   -- Prelude.putStrLn $ "ehist: " ++ show ehist

  --   -- ehist2 <- commitEntryHistory c2 "goodbye/files/world.txt"
  --   -- Prelude.putStrLn $ "ehist2: " ++ show ehist2

  --   -- oid <- parseOid ("2506e7fc" :: Text)
  --   -- c4 <- lookupCommit (fromJust oid)
  --   -- c5 <- maybe (return Nothing) (lookupCommit) oid
  --   -- c6 <- lookupCommit (fromJust oid)
  --   -- ehist4 <- commitEntryHistory (fromJust c4) "goodbye/files/world.txt"
  --   -- Prelude.putStrLn $ "ehist4: " ++ show (Prelude.head ehist4)

  -- , "smallTest1" $ do

  -- withNewRepository "smallTest1.git" $ do
  --   let blob =
  --         createBlobUtf8 "# Auto-createdsitory for tutorial contents\n"
  --       masterRef = "refs/heads/master"
  --       sig = Signature { signatureName   = "First Name"
  --                       , signatureEmail = "user1@email.org"
  --                       , signatureWhen  = posixSecondsToUTCTime 1348981883 }
  --   tree <- putBlobInTree newTree "README.md" (blobRef blob)
  --   commit <- createCommit [] (treeRef tree) sig sig "Initial commit"
  --   c1id <- update commit
  --   print $ "commit1 sha = " ++ show c1id

  --   let sig2 = Signature { signatureName   = "Second Name"
  --                        , signatureEmail = "user2@email.org"
  --                        , signatureWhen  = posixSecondsToUTCTime 1348982883 }
  --   blob <- createBlobUtf8 "This is some content."
  --   blobOID <- update blob
  --   tree' <- putBlobInTree tree "foo.txt" (blobRef blob)
  --   commit' <- createCommit [commitRef commit] (treeRef tree') sig sig
  --                          "This is another log message."
  --   commitOID <- update commit'
  --   print $ "commit2 sha = " ++ show commitOID
  --   updateRef_ masterRef (RefOid commitOID)

  --   True @?= True

  --   return ()

-- Main.hs ends here
