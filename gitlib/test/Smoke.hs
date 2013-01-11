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
    (createCommit repo) { commitTree      = ObjRef tr
                        , commitAuthor    = sig
                        , commitCommitter = sig
                        , commitLog       = "Sample log message." }

tests :: Test
tests = test [

  "singleBlob" ~:

  withRepository "singleBlob.git" $ \repo -> do
    update_ $ createBlob repo (E.encodeUtf8 "Hello, world!\n")

    x <- catBlob repo "af5626b4a114abcb82d63db7c8082c3c4756e51b"
    x @?= Just "Hello, world!\n"

    x <- catBlob repo "af5626b"
    x @?= Just "Hello, world!\n"

    return ()

  , "singleTree" ~:

  withRepository "singleTree.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree (createTree repo) "hello/world.txt" (blobRef hello)
    x  <- oid tr
    x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    return()

  , "twoTrees" ~:

  withRepository "twoTrees.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree (createTree repo) "hello/world.txt" (blobRef hello)
    x  <- oid tr
    x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree tr "goodbye/files/world.txt" (blobRef goodbye)
    x  <- oid tr
    x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

    return()

  , "deleteTree" ~:

  withRepository "deleteTree.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree (createTree repo) "hello/world.txt" (blobRef hello)
    x  <- oid tr
    x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree tr "goodbye/files/world.txt" (blobRef goodbye)
    x  <- oid tr
    x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

    -- Confirm that deleting world.txt also deletes the now-empty subtree
    -- goodbye/files, which also deletes the then-empty subtree goodbye,
    -- returning us back the original tree.
    tr <- removeFromTree "goodbye/files/world.txt" tr
    x  <- oid tr
    x @?= "c0c848a2737a6a8533a18e6bd4d04266225e0271"

    return()

  , "createCommit" ~:

  withRepository "createCommit.git" $ \repo -> do
    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree (createTree repo) "hello/world.txt" (blobRef hello)

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree tr "goodbye/files/world.txt" (blobRef goodbye)
    x  <- oid tr
    x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

    -- The Oid has been cleared in tr, so this tests that it gets written as
    -- needed.
    let sig  = Signature {
            signatureName  = "John Wiegley"
          , signatureEmail = "johnw@newartisans.com"
          , signatureWhen  = posixSecondsToUTCTime 1348980883 }

    x <- oid $ sampleCommit repo tr sig
    x @?= "44381a5e564d19893d783a5d5c59f9c745155b56"

    return()

  , "createTwoCommits" ~:

  withRepository "createTwoCommits.git" $ \repo -> alloca $ \loosePtr -> do
    withCString "createTwoCommits.git/objects" $ \objectsDir -> do
      r <- c'git_odb_backend_loose loosePtr objectsDir (-1) 0
      when (r < 0) $ error "Failed to create loose objects backend"
    loosePtr' <- peek loosePtr
    backend   <- traceBackend loosePtr'
    odbBackendAdd repo backend 3

    let hello = createBlob repo (E.encodeUtf8 "Hello, world!\n")
    tr <- updateTree (createTree repo) "hello/world.txt" (blobRef hello)

    let goodbye = createBlob repo (E.encodeUtf8 "Goodbye, world!\n")
    tr <- updateTree tr "goodbye/files/world.txt" (blobRef goodbye)
    tr <- update tr
    x  <- oid tr
    x @?= "98c3f387f63c08e1ea1019121d623366ff04de7a"

    -- The Oid has been cleared in tr, so this tests that it gets written as
    -- needed.
    let sig = Signature {
            signatureName  = "John Wiegley"
          , signatureEmail = "johnw@newartisans.com"
          , signatureWhen  = posixSecondsToUTCTime 1348980883 }
        c   = sampleCommit repo tr sig
    c <- update c
    x <- oid c
    x @?= "44381a5e564d19893d783a5d5c59f9c745155b56"

    let goodbye2 = createBlob repo (E.encodeUtf8 "Goodbye, world again!\n")
    tr <- updateTree tr "goodbye/files/world.txt" (blobRef goodbye2)
    tr <- update tr
    x  <- oid tr
    x @?= "f2b42168651a45a4b7ce98464f09c7ec7c06d706"

    let sig = Signature {
            signatureName  = "John Wiegley"
          , signatureEmail = "johnw@newartisans.com"
          , signatureWhen  = posixSecondsToUTCTime 1348981883 }
        c2  = (sampleCommit repo tr sig) {
                  commitLog       = "Second sample log message."
                , commitParents   = [ObjRef c] }
    x <- oid c2
    x @?= "2506e7fcc2dbfe4c083e2bd741871e2e14126603"

    c2 <- update c2
    cid <- objectId c2
    writeRef $ createRef repo "refs/heads/master" (RefTargetId cid)
    writeRef $ createRef repo "HEAD" (RefTargetSymbolic "refs/heads/master")

    x <- oidToText <$> resolveRef repo "refs/heads/master"
    x @?= "2506e7fcc2dbfe4c083e2bd741871e2e14126603"

    mapAllRefs repo (\name -> Prelude.putStrLn $ "Ref: " ++ unpack name)

    ehist <- commitHistoryFirstParent c2
    Prelude.putStrLn $ "ehist: " ++ show ehist

    ehist2 <- commitEntryHistory c2 "goodbye/files/world.txt"
    Prelude.putStrLn $ "ehist2: " ++ show ehist2

    -- oid <- parseOid ("2506e7fc" :: Text)
    -- c3 <- lookupCommit (fromJust oid) repo
    -- ehist3 <- commitEntryHistory "goodbye/files/world.txt" (fromJust c3)
    -- Prelude.putStrLn $ "ehist3: " ++ show ehist3

    oid <- parseOid ("2506e7fc" :: Text)
    c4 <- lookupCommit repo (fromJust oid)
    c5 <- maybe (return Nothing) (lookupCommit repo) oid
    c6 <- lookupCommit repo (fromJust oid)
    ehist4 <- commitEntryHistory (fromJust c4) "goodbye/files/world.txt"
    Prelude.putStrLn $ "ehist4: " ++ show (Prelude.head ehist4)

  , "smallTest1" ~:

  withRepository "smallTest1.git" $ \repo -> do
    let blob =
          createBlob repo "# Auto-created repository for tutorial contents\n"
        masterRef = "refs/heads/master"
        sig = Signature { signatureName   = "First Name"
                        , signatureEmail = "user1@email.org"
                        , signatureWhen  = posixSecondsToUTCTime 1348981883 }
    tree <- updateTree (createTree repo) "README.md" (blobRef blob)
    commit <- writeCommit (createCommit repo)
            { commitLog       = "Initial commit"
            , commitAuthor    = sig
            , commitCommitter = sig
            , commitTree      = ObjRef tree
            } (Just masterRef)
    c1id <- objectId commit
    print $ "commit1 sha = " ++ show c1id

    let sig2 = Signature { signatureName   = "Second Name"
                         , signatureEmail = "user2@email.org"
                         , signatureWhen  = posixSecondsToUTCTime 1348982883 }
    blob <- writeBlob $ createBlob repo "This is some content."
    blobOID <- objectId blob
    commit' <- updateCommit commit {
          commitAuthor = sig
        , commitCommitter = sig
        , commitLog = "This is another log message."
        , commitParents = [ObjRef commit]
        } "foo.txt" (blobRef blob)
    commitOID <- objectId commit'
    print $ "commit2 sha = " ++ show commitOID
    writeRef_ $ createRef repo masterRef $ RefTargetId commitOID

    True @?= True

    return ()

  ]

-- Main.hs ends here
