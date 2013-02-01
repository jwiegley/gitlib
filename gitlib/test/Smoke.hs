{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Git.Smoke where

-- import           Bindings.Libgit2.OdbBackend
-- import           Control.Applicative
-- import           Control.Concurrent.ParallelIO
-- import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
-- import           Data.Maybe
-- import           Data.Tagged
-- import           Data.Text as T hiding (map)
-- import qualified Data.Text.Encoding as T
import           Data.Time.Clock.POSIX
-- import           Data.Traversable
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS
-- import           Foreign.C.String
-- import           Foreign.Marshal.Alloc
-- import           Foreign.Ptr
-- import           Foreign.Storable
import           Git
import           Git.Utils
import           Prelude hiding (FilePath, putStr, putStrLn)
-- import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()

-- default (Text)

withNewRepository :: Repository m
                  => (FilePath -> Bool -> m () -> IO ())
                  -> FilePath -> m () -> IO ()
withNewRepository wrapper dir action = do
  exists <- isDirectory dir
  when exists $ removeTree dir
  a <- wrapper dir True action
  -- we want exceptions to leave the repo behind
  removeTree dir
  return a

sampleCommit :: Repository m => Tree m -> Signature -> m (Commit m)
sampleCommit tr sig =
    createCommit [] (treeRef tr) sig sig "Sample log message." Nothing

smokeTestSpec :: Repository m => (FilePath -> Bool -> m () -> IO ()) -> Spec
smokeTestSpec wr = describe "Smoke tests" $ do
  it "create a single blob" $ do
    withNewRepository wr "singleBlob.git" $ do
      createBlobUtf8 "Hello, world!\n"

      x <- catBlob "af5626b4a114abcb82d63db7c8082c3c4756e51b"
      liftIO $ x @?= "Hello, world!\n"

      x <- catBlob "af5626b"
      liftIO $ x @?= "Hello, world!\n"

  it "create a single tree" $ do
    withNewRepository wr "singleTree.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

  it "create two trees" $ do
    withNewRepository wr "twoTrees.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      putBlob tr "goodbye/files/world.txt" goodbye
      x <- oid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

  it "delete an item from a tree" $ do
    withNewRepository wr "deleteTree.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      putBlob tr "goodbye/files/world.txt"
          =<< createBlobUtf8 "Goodbye, world!\n"
      x <- oid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

      -- Confirm that deleting world.txt also deletes the now-empty
      -- subtree goodbye/files, which also deletes the then-empty subtree
      -- goodbye, returning us back the original tree.
      dropFromTree tr "goodbye/files/world.txt"
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

  it "create a single commit" $ do
    withNewRepository wr "createCommit.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      putBlob tr "goodbye/files/world.txt" goodbye
      x <- oid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

      -- The Oid has been cleared in tr, so this tests that it gets
      -- written as needed.
      let sig  = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = posixSecondsToUTCTime 1348980883 }

      c <- sampleCommit tr sig
      let x = renderOid (commitOid c)
      liftIO $ x @?= "df1b2a6bfc78e57ac336de9ca6ed3ae8c850104d"

  it "create two commits" $ do
    withNewRepository wr "createTwoCommits.git" $ do
      -- liftIO $ withCString "createTwoCommits.git/objects" $ \objectsDir ->
      --   alloca $ \loosePtr -> do
      --     r <- c'git_odb_backend_loose loosePtr objectsDir (-1) 0
      --     when (r < 0) $ error "Failed to create loose objects backend"
      --     -- jww (2013-01-27): Restore
      --     -- loosePtr' <- peek loosePtr
      --     -- backend   <- traceBackend loosePtr'
      --     -- odbBackendAdd backend 3

      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      putBlob tr "goodbye/files/world.txt" goodbye
      x <- oid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

      -- The Oid has been cleared in tr, so this tests that it gets written as
      -- needed.
      let sig = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = posixSecondsToUTCTime 1348980883 }
      c <- sampleCommit tr sig
      let x = renderOid (commitOid c)
      liftIO $ x @?= "df1b2a6bfc78e57ac336de9ca6ed3ae8c850104d"

      goodbye2 <- createBlobUtf8 "Goodbye, world again!\n"
      putBlob tr "goodbye/files/world.txt" goodbye2
      x <- oid tr
      liftIO $ x @?= "f2b42168651a45a4b7ce98464f09c7ec7c06d706"

      let sig = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = posixSecondsToUTCTime 1348981883 }
      c2 <- createCommit [commitRef c] (treeRef tr) sig sig
                        "Second sample log message." Nothing
      let x = renderOid (commitOid c2)
      liftIO $ x @?= "02727e0666c0092cb6dac62d70876ae99097a74a"

      updateRef_ "refs/heads/master" (RefObj (commitRef c2))
      updateRef_ "HEAD" (RefSymbolic "refs/heads/master")

      c3 <- resolveRef "refs/heads/master"
      c3 <- resolveCommit c3
      let x = renderOid (commitOid c3)
      liftIO $ x @?= "02727e0666c0092cb6dac62d70876ae99097a74a"

      refs <- allRefNames
      liftIO $ show refs @?= "[\"refs/heads/master\"]"

      -- jww (2013-01-27): Restore
      -- ehist <- commitHistoryFirstParent c2
      -- Prelude.putStrLn $ "ehist: " ++ show ehist

      -- ehist2 <- commitEntryHistory c2 "goodbye/files/world.txt"
      -- Prelude.putStrLn $ "ehist2: " ++ show ehist2

      -- oid <- parseOid ("2506e7fc" :: Text)
      -- c4 <- lookupCommit (fromJust oid)
      -- c5 <- maybe (return Nothing) (lookupCommit) oid
      -- c6 <- lookupCommit (fromJust oid)
      -- ehist4 <- commitEntryHistory (fromJust c4) "goodbye/files/world.txt"
      -- Prelude.putStrLn $ "ehist4: " ++ show (Prelude.head ehist4)

      return ()

  it "another small test" $ do
    withNewRepository wr "smallTest1.git" $ do
      blob <- createBlobUtf8 "# Auto-createdsitory for tutorial contents\n"
      let masterRef = "refs/heads/master"
          sig = Signature { signatureName   = "First Name"
                          , signatureEmail = "user1@email.org"
                          , signatureWhen  = posixSecondsToUTCTime 1348981883 }
      tree <- newTree
      putBlob tree "README.md" blob
      commit <- createCommit [] (treeRef tree) sig sig "Initial commit" Nothing
      liftIO $ print $ "commit1 sha = " ++ show (renderOid (commitOid commit))

      let sig2 = Signature { signatureName   = "Second Name"
                           , signatureEmail = "user2@email.org"
                           , signatureWhen  = posixSecondsToUTCTime 1348982883 }
      blob <- createBlobUtf8 "This is some content."
      putBlob tree "foo.txt" blob
      commit' <- createCommit [commitRef commit] (treeRef tree) sig sig
                             "This is another log message." (Just masterRef)
      liftIO $ print $ "commit2 sha = " ++ show (renderOid (commitOid commit'))

      liftIO $ True @?= True

-- Main.hs ends here
