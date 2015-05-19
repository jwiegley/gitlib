{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.ByteString
import qualified Data.ByteString.Lazy as BL
import           Data.Digest.Pure.SHA
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hex
import           Data.Tagged
import           Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Git as Git
import qualified Git.Smoke as Git
import           System.Exit
import           System.FilePath
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

data Repository = Repository

newtype MockRepository a = MockRepository
    { runMockRepository :: ReaderT Repository IO a }

instance Eq Repository where
  x == y = undefined

instance Show Repository where
  show x = undefined

type Oid       = Git.Oid MockRepository
type Tree      = Git.Tree MockRepository
type TreeEntry = Git.TreeEntry MockRepository
type Commit    = Git.Commit MockRepository

instance Functor MockRepository where
    fmap f (MockRepository x) = MockRepository (fmap f x)

instance Applicative MockRepository where
    pure = MockRepository . pure
    MockRepository f <*> MockRepository x = MockRepository (f <*> x)

instance Monad MockRepository where
    return = MockRepository . return
    MockRepository m >>= f = MockRepository (m >>= runMockRepository . f)

instance MonadIO MockRepository where
    liftIO m = MockRepository (liftIO m)

instance Git.RepositoryBase MockRepository where
    data Oid MockRepository    = Oid BL.ByteString
    data Tree MockRepository   = Tree (HashMap Text TreeEntry)
    data Commit MockRepository = Commit
    data Tag MockRepository    = Tag

    facts = undefined

    parseOid x = do
        y <- unhex (T.encodeUtf8 x)
        return $ Oid (BL.fromChunks [y])
    renderOid (Oid x) = T.pack (hex (TL.unpack (TL.decodeUtf8 x)))
    lookupRef name = do
        liftIO $ Prelude.putStrLn $ "lookupRef.."
        return undefined
    updateRef name commit = do
        liftIO $ Prelude.putStrLn $ "updateRef.."
        return undefined
    createRef name target = do
        liftIO $ Prelude.putStrLn $ "createRef.."
        return undefined
    deleteRef name = do
        liftIO $ Prelude.putStrLn $ "deleteRef.."
        return undefined
    resolveRef name = do
        liftIO $ Prelude.putStrLn $ "resolveRef.."
        return undefined
    allRefNames = return []
    lookupCommit oid = do
        liftIO $ Prelude.putStrLn $ "lookupCommit.."
        return undefined
    lookupTree oid = do
        liftIO $ Prelude.putStrLn $ "lookupTree.."
        return undefined
    lookupBlob oid = do
        oid' <- Git.parseOid "af5626b4a114abcb82d63db7c8082c3c4756e51b"
        if unTagged oid == oid'
            then return (Git.Blob oid (Git.BlobString "Hello, world!\n"))
            else undefined
    lookupTag oid = do
        liftIO $ Prelude.putStrLn $ "lookupTag.."
        return undefined

    lookupObject oidText = do
        if oidText == "af5626b"
            then do
            oid <- Git.parseOid "af5626b4a114abcb82d63db7c8082c3c4756e51b"
            return (Git.BlobObj
                    (Git.Known (Git.Blob (Tagged oid)
                                         (Git.BlobString "Hello, world!\n"))))
            else undefined

    existsObject = undefined

    newTree = return (Tree HashMap.empty)

    createBlob (Git.BlobString x) =
        return $ Tagged (Oid (bytestringDigest (sha1 (BL.fromChunks [x]))))
    createBlob _ = do
        liftIO $ Prelude.putStrLn $ "createBlob (with non-BlobString)"
        return undefined

    createCommit _ _ _ _ _ _ = return Commit
    createTag _ _ _ _ = return Tag

instance Show (Git.Oid MockRepository) where
    show (Oid x) = show x

instance Ord (Git.Oid MockRepository) where
    compare (Oid x) (Oid y) = compare x y

instance Eq (Git.Oid MockRepository) where
    Oid x == Oid y = x == y

instance Git.Treeish Tree where
    type TreeRepository Tree = MockRepository
    modifyTree      = undefined
    writeTree       = undefined
    traverseEntries = undefined

instance Git.Commitish Commit where
    type CommitRepository Commit = MockRepository
    commitOid       = undefined
    commitParents   = undefined
    commitTree      = undefined
    commitAuthor    = undefined
    commitCommitter = undefined
    commitLog       = undefined
    commitEncoding  = undefined

instance Git.Treeish Commit where
    type TreeRepository Commit = MockRepository
    modifyTree      = Git.defaultCommitModifyTree
    writeTree       = Git.defaultCommitWriteTree
    traverseEntries = undefined

withOpenMockRepository :: Repository -> MockRepository a -> IO a
withOpenMockRepository repo action =
    runReaderT (runMockRepository action) repo

withMockRepository :: FilePath -> Bool -> Bool -> MockRepository a -> IO a
withMockRepository _ _ _ action = do
    withOpenMockRepository Repository action

main :: IO ()
main = do
    summary <- hspecWith (defaultConfig { configVerbose = True })
                         (Git.smokeTestSpec withMockRepository)
    -- when (summaryFailures summary > 0) $ exitFailure
    return ()

-- Main.hs ends here
