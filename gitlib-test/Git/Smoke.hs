{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Git.Smoke where

import           Conduit
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Catch
import qualified Data.ByteString.Lazy as BL
import           Data.List (sort)
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Typeable
import           Git
import           Prelude hiding (putStr)
import           Test.HUnit
import           Test.Hspec (Spec, Example, describe, it)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import Control.Concurrent

sampleCommit :: MonadGit r m => TreeOid r -> Signature -> m (Commit r)
sampleCommit tr sig =
    createCommit [] tr sig sig "Sample log message.\n" Nothing

smokeTestSpec :: (MonadGit r m, MonadIO m, MonadBaseControl IO m, MonadThrow m,
                  MonadGit s n, MonadIO n, MonadBaseControl IO n, MonadThrow n)
              => RepositoryFactory m IO r
              -> RepositoryFactory n m s
              -> Spec
smokeTestSpec pr _pr2 = describe "Smoke tests" $ do
  it "hash without persisting" $ withNewRepository pr "hashObject.git" $ do
      r <- hashContents $ BlobString "flim-flam"
      liftIO $ renderObjOid r @?= "5cc144cc0c3c101342b3f0c546c014d1e0e66da9"

  it "create a single blob" $ withNewRepository pr "singleBlob.git" $ do
      createBlobUtf8 "Hello, world!\n"

      oid <- parseObjOid "af5626b4a114abcb82d63db7c8082c3c4756e51b"
      x <- catBlob oid
      liftIO $ x @?= "Hello, world!\n"

      e1 <- existsObject $ untag oid
      liftIO $ e1 @? "Expected object doesn't exist."

      e2 <- existsObject =<< parseOid "efefefefefefefefefefefefefefefefefefefef"
      liftIO $ not e2 @? "Unexpected object exists."

      -- jww (2013-02-01): Restore when S3 support prefix lookups
      -- x <- catBlob "af5626b"
      -- liftIO $ x @?= "Hello, world!\n"

  it "create a single tree" $ withNewRepository pr "singleTree.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- createTree $ putBlob "hello/world.txt" hello
      let x = renderObjOid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      toid <- Git.parseOid "c0c848a2737a6a8533a18e6bd4d04266225e0271"
      tr <- lookupTree (Tagged toid)
      x <- renderObjOid <$> treeOid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

  it "create two trees" $ withNewRepository pr "twoTrees.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- createTree $ putBlob "hello/world.txt" hello
      let x = renderObjOid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      tr <- mutateTreeOid tr $ putBlob "goodbye/files/world.txt" goodbye
      let x = renderObjOid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

  it "delete an item from a tree" $ withNewRepository pr "deleteTree.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- createTree $ putBlob "hello/world.txt" hello
      let x = renderObjOid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      tr <- mutateTreeOid tr $
          putBlob "goodbye/files/world.txt"
              =<< lift (createBlobUtf8 "Goodbye, world!\n")
      let x = renderObjOid tr
      liftIO $ x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

      -- Confirm that deleting world.txt also deletes the now-empty
      -- subtree goodbye/files, which also deletes the then-empty subtree
      -- goodbye, returning us back the original tree.
      tr <- mutateTreeOid tr $ dropEntry "goodbye/files/world.txt"
      let x = renderObjOid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

  it "create a single commit" $ withNewRepository pr "createCommit.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- createTree $ putBlob "hello/world.txt" hello

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      tr <- mutateTreeOid tr $ putBlob "goodbye/files/world.txt" goodbye
      let x = renderObjOid tr
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
      tr <- createTree $ putBlob "hello/world.txt" hello
      let x = renderObjOid tr
      liftIO $ x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

      let sig  = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = fakeTime 1348980883 }

      c <- sampleCommit tr sig
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "d592871f56aa949d726fcc211370d1af305e9597"

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      tr' <- mutateTreeOid (Git.commitTree c) $
                 putBlob "hello/goodbye.txt" goodbye

      let x = renderObjOid tr'
      liftIO $ x @?= "19974fde643bddd26c46052f7a8bdf87f7772c1e"

      let sig  = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = fakeTime 1348980883 }

      c <- createCommit [commitOid c] tr' sig sig
               "Sample log message 2.\n" (Just "refs/heads/master")
      let x = renderObjOid (commitOid c)
      liftIO $ x @?= "61a2c6425d2e60a480d272aa921d4f4ffe5dd20f"

  it "create two commits" $ withNewRepository pr "createTwoCommits.git" $ do
      hello <- createBlobUtf8 "Hello, world!\n"
      tr <- createTree $ putBlob "hello/world.txt" hello

      goodbye <- createBlobUtf8 "Goodbye, world!\n"
      tr <- mutateTreeOid tr $ putBlob "goodbye/files/world.txt" goodbye
      let x = renderObjOid tr
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
      tr <- mutateTreeOid tr $ putBlob "goodbye/files/world.txt" goodbye2
      let x = renderObjOid tr
      liftIO $ x @?= "f2b42168651a45a4b7ce98464f09c7ec7c06d706"

      let sig = Signature {
              signatureName  = "John Wiegley"
            , signatureEmail = "johnw@fpcomplete.com"
            , signatureWhen  = fakeTime 1348981883 }
      c2 <- createCommit [commitOid c] tr sig sig
                "Second sample log message.\n" Nothing
      let x = renderObjOid (commitOid c2)
      liftIO $ x @?= "967b647bd11990d1bb15ff5209ad44a002779454"

      updateReference "refs/heads/master" (RefObj (untag (commitOid c2)))
      hasSymRefs <- hasSymbolicReferences <$> facts
      when hasSymRefs $
          updateReference "HEAD" (RefSymbolic "refs/heads/master")

      Just c3 <- resolveReference "refs/heads/master"
      let x = renderOid c3
      liftIO $ x @?= "967b647bd11990d1bb15ff5209ad44a002779454"

      refs <- listReferences
      liftIO $ show refs @?= "[\"refs/heads/master\"]"

      deleteReference "refs/heads/master"
      deleteReference "HEAD"

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
      let masterRef = "refs/heads/master"
          sig = Signature { signatureName   = "First Name"
                          , signatureEmail = "user1@email.org"
                          , signatureWhen  = fakeTime 1348981883 }
      blob <- createBlobUtf8 "# Auto-createdsitory for tutorial contents\n"
      tree <- createTree $ putBlob "README.md" blob
      commit <- createCommit [] tree sig sig "Initial commit" Nothing

      blob <- createBlobUtf8 "This is some content."
      tree <- mutateTreeOid tree $ putBlob "foo.txt" blob
      createCommit [commitOid commit] tree sig sig
          "This is another log message." (Just masterRef)

      liftIO $ True @?= True

  it "traversal test" $ withNewRepository pr "traversalTest.git" $ do
      let masterRef = "refs/heads/master"
          sig = Signature
              { signatureName  = "First Name"
              , signatureEmail = "user1@email.org"
              , signatureWhen  = fakeTime 1348981883 }
      tree <- createTree $ do
          mkBlob "One"
          mkBlob "Two"
          mkBlob "Files/Three"
          mkBlob "More/Four"
          mkBlob "Five/More/Four"
      createCommit [] tree sig sig "Initial commit" (Just masterRef)

      tree' <- lookupTree tree
      paths <- map fst <$> listTreeEntries tree'
      liftIO $ sort paths @?= [ "Files"
                              , "Files/Three"
                              , "Five"
                              , "Five/More"
                              , "Five/More/Four"
                              , "More"
                              , "More/Four"
                              , "One"
                              , "Two"
                              ]

  treeit "adds a file" pr
      [ Bl "one"
      ] $ do
          mkBlob "one"

  treeit "adds two files" pr
      [ Bl "one"
      , Bl "two"
      ] $ do
          mkBlob "one"
          mkBlob "two"

  treeit "adds three files" pr
      [ Bl "one"
      , Bl "three"
      , Bl "two"
      ] $ do
          mkBlob "one"
          mkBlob "two"
          mkBlob "three"

  treeit "adds a file at a subpath" pr
      [ Tr "a"
      , Bl "a/one"
      ] $ do
          mkBlob "a/one"

  treeit "adds a file at a deep subpath" pr
      [ Tr "a"
      , Tr "a/b"
      , Tr "a/b/c"
      , Tr "a/b/c/d"
      , Tr "a/b/c/d/e"
      , Bl "a/b/c/d/e/one"
      ] $ do
          mkBlob "a/b/c/d/e/one"

  treeit "adds files at multiple depths" pr
      [ Tr "a"
      , Tr "a/b"
      , Tr "a/b/c"
      , Tr "a/b/c/d"
      , Tr "a/b/c/d/e"
      , Bl "a/b/c/d/e/five"
      , Bl "a/b/c/d/four"
      , Bl "a/b/c/three"
      , Bl "a/b/two"
      , Bl "a/one"
      ] $ do
          mkBlob "a/one"
          mkBlob "a/b/two"
          mkBlob "a/b/c/three"
          mkBlob "a/b/c/d/four"
          mkBlob "a/b/c/d/e/five"

  treeit "adds files at mixed depths" pr
      [ Tr "a"
      , Bl "a/one"
      , Tr "b"
      , Tr "b/c"
      , Bl "b/c/two"
      , Tr "d"
      , Tr "d/e"
      , Tr "d/e/f"
      , Bl "d/e/f/three"
      , Tr "g"
      , Tr "g/h"
      , Tr "g/h/i"
      , Tr "g/h/i/j"
      , Bl "g/h/i/j/four"
      , Tr "k"
      , Tr "k/l"
      , Tr "k/l/m"
      , Tr "k/l/m/n"
      , Tr "k/l/m/n/o"
      , Bl "k/l/m/n/o/five"
      ] $ do
          mkBlob "a/one"
          mkBlob "b/c/two"
          mkBlob "d/e/f/three"
          mkBlob "g/h/i/j/four"
          mkBlob "k/l/m/n/o/five"

  treeit "adds and drops a file" pr
      [] $ do
          mkBlob "one"
          dropEntry "one"

  treeit "adds two files and drops one" pr
      [ Bl "one"
      ] $ do
          mkBlob "one"
          mkBlob "two"
          dropEntry "two"

  treeit "adds and drops files at mixed depths" pr
      [ Tr "a"
      , Bl "a/one"
      , Tr "b"
      , Tr "b/c"
      , Bl "b/c/two"
      , Tr "g"
      , Tr "g/h"
      , Tr "g/h/i"
      , Tr "g/h/i/j"
      , Bl "g/h/i/j/four"
      ] $ do
          mkBlob "a/one"
          mkBlob "b/c/two"
          mkBlob "b/c/three"
          mkBlob "d/e/f/three"
          mkBlob "g/h/i/j/four"
          mkBlob "k/l/m/n/o/five"
          dropEntry "b/c/three"
          dropEntry "d/e/f/three"
          dropEntry "k/l"

  where
    fakeTime secs = utcToZonedTime utc (posixSecondsToUTCTime secs)

data Kind = Bl TreeFilePath | Tr TreeFilePath deriving (Eq, Show)

isBlobKind :: Kind -> Bool
isBlobKind (Bl _) = True
isBlobKind _      = False

kindPath :: Kind -> TreeFilePath
kindPath (Bl path) = path
kindPath (Tr path) = path

data TreeitException = TreeitException Text deriving (Eq, Show, Typeable)

instance Exception TreeitException

mkBlob :: MonadGit r m => TreeFilePath -> TreeT r m ()
mkBlob path = putBlob path
    =<< lift (createBlob $ BlobStream $
                  sourceLazy (BL.fromChunks [path <> "\n"]))

doTreeit :: (MonadBaseControl IO m, MonadIO m,
             MonadGit r n, MonadBaseControl IO n, MonadIO n)
       => String -> RepositoryFactory n m r -> [Kind] -> TreeT r n a -> m ()
doTreeit label pr kinds action = withNewRepository pr fullPath $ do
    tref <- createTree action
    tree <- lookupTree tref
    forM_ kinds $ \kind -> do
        let path = kindPath kind
        entry <- treeEntry tree path
        case entry of
            Just (BlobEntry boid _) -> do
                liftIO $ isBlobKind kind @?= True
                bs <- lookupBlob boid >>= blobToByteString
                liftIO $ bs @?= path <> "\n"
            Just (TreeEntry _) ->
                liftIO $ isBlobKind kind @?= False
            Nothing ->
                liftIO $ throwIO (TreeitException "Expected entry not found")
            _ -> do
                liftIO $ isBlobKind kind @?= False
                liftIO $ throwIO (TreeitException "Entry is of unexpected kind")
    paths <- map fst <$> listTreeEntries tree
    liftIO $ sort paths @?= map kindPath kinds
  where
    fullPath  = normalize label <> ".git"
    normalize = map (\x -> if x == ' ' then '-' else x)

treeit :: (Example (m ()), MonadIO m, MonadBaseControl IO m,
           MonadGit r n, MonadIO n, MonadBaseControl IO n)
       => String -> RepositoryFactory n m r -> [Kind] -> TreeT r n a -> Spec
treeit label pr kinds action = it label $ doTreeit label pr kinds action

treeitFail :: (MonadGit r m, MonadIO m, MonadBaseControl IO m)
           => String -> RepositoryFactory m IO r -> [Kind] -> TreeT r m a
           -> Spec
treeitFail label pr kinds action =
    it label $ doTreeit label pr kinds action
        `shouldThrow` (\(_ :: TreeitException) -> True)

-- Main.hs ends here
