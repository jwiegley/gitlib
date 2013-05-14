{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Git.Smoke where

import Control.Applicative
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.List (sort)
import Data.Tagged
import Data.Time
import Data.Time.Clock.POSIX
import Git
import Git.Utils
import Prelude hiding (FilePath, putStr)
import Test.HUnit
import Test.Hspec (Spec, describe, it)
import Test.Hspec.HUnit ()

sampleCommit :: Repository m => Tree m -> Signature -> m (Commit m)
sampleCommit tr sig =
    createCommit [] (treeRef tr) sig sig "Sample log message.\n" Nothing

smokeTestSpec :: (Repository (t IO), MonadGit (t IO), MonadTrans t,
                  Repository (t2 (t IO)), MonadGit (t2 (t IO)), MonadTrans t2,
                  MonadBaseControl IO (t IO))
              => RepositoryFactory t IO c
              -> RepositoryFactory t2 (t IO) c2
              -> Spec
smokeTestSpec pr _pr2 = describe "Smoke tests" $ do
  it "create a single blob" $ withNewRepository pr "singleBlob.git" $ do
      createBlobUtf8 "Hello, world!\n"

      x <- catBlob "af5626b4a114abcb82d63db7c8082c3c4756e51b"
      liftIO $ x @?= "Hello, world!\n"

      -- jww (2013-02-01): Restore when S3 support prefix lookups
      -- x <- catBlob "af5626b"
      -- liftIO $ x @?= "Hello, world!\n"

  it "create a single tree" $ withNewRepository pr "singleTree.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      toid <- Git.parseOid "c0c848a2737a6a8533a18e6bd4d04266225e0271"
      tr <- lookupTree (Tagged toid)
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

  it "create two trees" $ withNewRepository pr "twoTrees.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello
      x <- oid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      putBlob tr "goodbye/files/world.txt" goodbye
      x <- oid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

  it "delete an item from a tree" $ withNewRepository pr "deleteTree.git" $ do
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

  it "create a single commit" $ withNewRepository pr "createCommit.git" $ do
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
            , signatureWhen  = fakeTime 1348980883 }

      c <- sampleCommit tr sig
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "4e0529eb30f53e65c1e13836e73023c9d23c25ae"

      coid <- Git.parseOid "4e0529eb30f53e65c1e13836e73023c9d23c25ae"
      c <- lookupCommit (Tagged coid)
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "4e0529eb30f53e65c1e13836e73023c9d23c25ae"

  it "modify a commit" $ withNewRepository pr "modifyCommit.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- newTree
      putBlob tr "hello/world.txt" hello
      oid <- Git.writeTree tr
      let x = renderObjOid oid
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      let sig  = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = fakeTime 1348980883 }

      c <- sampleCommit tr sig
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "d592871f56aa949d726fcc211370d1af305e9597"

      tr' <- Git.resolveTreeRef (Git.commitTree c)
      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      putBlob tr' "hello/goodbye.txt" goodbye

      oid <- Git.writeTree tr'
      let x = renderObjOid oid
      liftIO $ x @?= "19974fde643bddd26c46052f7a8bdf87f7772c1e"

      let sig  = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = fakeTime 1348980883 }

      c <- createCommit [commitRef c] (treeRef tr') sig sig
                       "Sample log message 2.\n" (Just "refs/heads/master")
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "61a2c6425d2e60a480d272aa921d4f4ffe5dd20f"

  it "create two commits" $ withNewRepository pr "createTwoCommits.git" $ do
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
            , signatureWhen  = fakeTime 1348980883 }
      c <- sampleCommit tr sig
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "4e0529eb30f53e65c1e13836e73023c9d23c25ae"

      goodbye2 <- createBlobUtf8 "Goodbye, world again!\n"
      putBlob tr "goodbye/files/world.txt" goodbye2
      x <- oid tr
      liftIO $ x @?= "f2b42168651a45a4b7ce98464f09c7ec7c06d706"

      let sig = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = fakeTime 1348981883 }
      c2 <- createCommit [commitRef c] (treeRef tr) sig sig
                        "Second sample log message.\n" Nothing
      let x = renderObjOid (commitOid c2)
      liftIO $ x @?= "967b647bd11990d1bb15ff5209ad44a002779454"

      updateRef_ "refs/heads/master" (RefObj (commitRef c2))
      hasSymRefs <- hasSymbolicReferences <$> facts
      when hasSymRefs $
          updateRef_ "HEAD" (RefSymbolic "refs/heads/master")

      Just c3 <- resolveRef "refs/heads/master"
      c3 <- resolveCommitRef c3
      let x = renderObjOid (commitOid c3)
      liftIO $ x @?= "967b647bd11990d1bb15ff5209ad44a002779454"

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

  it "another small test" $ withNewRepository pr "smallTest1.git" $ do
      blob <- createBlobUtf8 "# Auto-createdsitory for tutorial contents\n"
      let masterRef = "refs/heads/master"
          sig = Signature { signatureName   = "First Name"
                          , signatureEmail = "user1@email.org"
                          , signatureWhen  = fakeTime 1348981883 }
      tree <- newTree
      putBlob tree "README.md" blob
      commit <- createCommit [] (treeRef tree) sig sig "Initial commit" Nothing

      blob <- createBlobUtf8 "This is some content."
      putBlob tree "foo.txt" blob
      createCommit [commitRef commit] (treeRef tree) sig sig
          "This is another log message." (Just masterRef)

      liftIO $ True @?= True

  it "traversal test" $ withNewRepository pr "traversalTest.git" $ do
      let masterRef = "refs/heads/master"
          sig = Signature
              { signatureName  = "First Name"
              , signatureEmail = "user1@email.org"
              , signatureWhen  = fakeTime 1348981883 }
      tree <- newTree
      putBlob tree "One"         =<< createBlobUtf8 "One\n"
      putBlob tree "Two"         =<< createBlobUtf8 "Two\n"
      putBlob tree "Files/Three" =<< createBlobUtf8 "Three\n"
      putBlob tree "More/Four"   =<< createBlobUtf8 "Four\n"
      createCommit [] (treeRef tree) sig sig "Initial commit"
          (Just masterRef)

      paths <- traverseEntries tree $ \fp _ -> return fp
      liftIO $ sort paths @?= [ "Files", "More", "One", "Two"
                              , "Files/Three", "More/Four" ]

  -- it "push commit between repositories" $ do
  --     let masterRef = "refs/heads/master"
  --         sig = Signature { signatureName   = "First Name"
  --                         , signatureEmail = "user1@email.org"
  --                         , signatureWhen  = fakeTime 1348981883 }

  --     withNewRepository pr "pushSource.git" $ do
  --         tree <- newTree

  --         putBlob tree "foo/README.md"
  --             =<< createBlobUtf8 "one\n"
  --         c <- createCommit [] (treeRef tree) sig sig
  --                  "Initial commit.\n" Nothing

  --         putBlob tree "bar/foo.txt"
  --             =<< createBlobUtf8 "This is some content."
  --         c <- createCommit [commitRef c] (treeRef tree) sig sig
  --                  "This is another log message.\n" (Just masterRef)

  --         putBlob tree "foo/bar/baz.txt"
  --             =<< createBlobUtf8 "This is some content."
  --         c <- createCommit [commitRef c] (treeRef tree) sig sig
  --                  "This is another log message.\n" (Just masterRef)

  --         putBlob tree "bar/bar.txt"
  --             =<< createBlobUtf8 "This is some content."
  --         c <- createCommit [commitRef c] (treeRef tree) sig sig
  --                  "This is another log message.\n" (Just masterRef)

  --         putBlob tree "foo/hello.txt"
  --             =<< createBlobUtf8 "This is some content."
  --         c <- createCommit [commitRef c] (treeRef tree) sig sig
  --                  "This is another log message.\n" (Just masterRef)

  --         Just cref <- resolveRef masterRef
  --         let coid = renderObjOid (commitRefOid cref)

  --         withNewRepository pr2 "pushDest.git" $ do
  --             pushCommit (CommitRefName masterRef) Nothing masterRef
  --             mref2 <- lookupRef masterRef
  --             Just cref <- referenceToRef Nothing mref2
  --             lift . lift $
  --                 coid @?= renderObjOid (commitRefOid cref)

  --         withNewRepository pr2 "pushDest.git" $ do
  --             pushCommit (CommitRefName masterRef)
  --                 (Just "file://pushDest.git") masterRef
  --             mref2 <- lookupRef masterRef
  --             Just cref <- referenceToRef Nothing mref2
  --             lift . lift $
  --                 coid @?= renderObjOid (commitRefOid cref)

  where
    fakeTime secs = utcToZonedTime utc (posixSecondsToUTCTime secs)

-- Main.hs ends here
