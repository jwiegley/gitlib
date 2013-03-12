{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Arrow (first)
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString as B (readFile)
import           Data.Function (fix)
import           Data.Foldable (foldlM)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (unpack, toStrict, init)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Data.Traversable (sequenceA)
import           Filesystem (getModified, isDirectory, isFile, canonicalizePath)
import           Filesystem.Path.CurrentOS (FilePath, (</>), parent, null)
import           Git
import           Git.Libgit2 (withLgRepository)
import           Git.Utils (treeBlobEntries)
import           Options.Applicative
import           Prelude hiding (FilePath, null)
import           Shelly (toTextIgnore, fromText, silently, shelly, run)
import           System.IO (stderr)
import           System.Log.Formatter (tfLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

instance Read FilePath

data Options = Options
    { verbose    :: Bool
    , debug      :: Bool
    , gitDir     :: FilePath
    , workingDir :: FilePath
    , interval   :: Int
    , resume     :: Bool
    }

options :: Parser Options
options = Options
    <$> switch (short 'v' <> long "verbose" <> help "Display info")
    <*> switch (short 'D' <> long "debug" <> help "Display debug")
    <*> option (long "git-dir" <> value ".git"
                <> help "Git repository to store snapshots in (def: \".git\")")
    <*> option (short 'd' <> long "work-dir" <> value ""
                <> help "The working tree to snapshot (def: \".\")")
    <*> option (short 'i' <> long "interval" <> value 60
                <> help "Snapshot each N seconds")
    <*> switch (short 'r' <> long "resume" <> value False
                <> help "Resumes using last set of snapshots")

main :: IO ()
main = execParser opts >>= doMain
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc desc <> header hdr)
    hdr  = "git-monitor 1.0.0 - quickly snapshot working tree changes"
    desc = "\nPassively snapshot working tree changes efficiently.\n\n\
           \The intended usage is to run \"git monitor &\" in your project\n\
           \directory before you begin a hacking session.\n\n\
           \Snapshots are kept in refs/snapshots/refs/heads/$BRANCH"

doMain :: Options -> IO ()
doMain opts = do
    -- Setup logging service if --verbose is used
    when (verbose opts) $ initLogging (debug opts)

    -- Ask Git for the user name and email in this repository
    (userName,userEmail) <- shelly $ silently $
        (,) <$> (TL.init <$> run "git" ["config", "user.name"])
            <*> (TL.init <$> run "git" ["config", "user.email"])

    isDir <- isDirectory (gitDir opts)
    gd    <- if isDir
             then return (gitDir opts)
             else shelly $ silently $
                  fromText . TL.init <$> run "git" ["rev-parse", "--git-dir"]

    let wd = if null (workingDir opts) then parent gd else "."

    -- Make sure we're in a known branch, and if so, let it begin
    forever $ withLgRepository gd True False $ do
        infoL $ "Saving snapshots in " ++ fileStr gd
        infoL $ "Working tree: " ++ fileStr wd
        ref <- lookupRef "HEAD"
        void $ case ref of
            Just (Reference _ (RefSymbolic name)) -> do
                infoL $ "Tracking branch " ++ T.unpack name
                start wd (TL.toStrict userName) (TL.toStrict userEmail) name
            _ -> error "Cannot use git-monitor if no branch is checked out"
  where
    initLogging debugMode = do
        let level | debugMode    = DEBUG
                  | verbose opts = INFO
                  | otherwise    = NOTICE
        h <- (`setFormatter` tfLogFormatter "%H:%M:%S" "$time - [$prio] $msg")
             <$> streamHandler System.IO.stderr level
        removeAllHandlers
        updateGlobalLogger "git-monitor" (setLevel level)
        updateGlobalLogger "git-monitor" (addHandler h)

    start wd userName userEmail ref = do
        let sref = "refs/snapshots/" <> ref

        -- Usually we ignore any snapshot history from a prior invocation of
        -- git-monitor, leaving those objects for git gc to cleanup; however,
        -- if --resume, continue the snapshot history from where we last left
        -- off.  Note that if --resume is always used, this can end up
        -- consuming a lot of disk space over time.
        --
        -- Also, it is useful to root the snapshot history at the current HEAD
        -- commit.  Note that it must be a Just value at this point, see
        -- above.
        scr  <- if resume opts
                then resolveRef sref
                else return Nothing
        scr' <- maybe (fromJust <$> resolveRef "HEAD") return scr
        sc   <- resolveCommitRef scr'
        str  <- resolveTreeRef (commitTree sc)
        toid <- writeTree str
        ft   <- readFileTree' str wd (isNothing scr)

        -- Begin the snapshotting process, which continues indefinitely until
        -- the process is stopped.  It is safe to cancel this process at any
        -- time, typically using SIGINT (C-c) or even SIGKILL.
        (snapshotTree opts wd userName userEmail ref sref) sc str toid ft

-- | 'snapshotTree' is the core workhorse of this utility.  It periodically
--   checks the filesystem for changes to Git-tracked files, and snapshots
--   any changes that have occurred in them.
snapshotTree :: (Repository m, MonadIO m)
             => Options -> FilePath
             -> Text -> Text -> Text -> Text
             -> Commit m -> Tree m -> TreeOid m
             -> Map FilePath (FileEntry m)
             -> m ()
snapshotTree opts wd name email ref sref = fix $ \loop sc str toid ft -> do
    -- Read the current working tree's state on disk
    ft' <- readFileTree ref wd False

    -- Prune files which have been removed since the last interval, and find
    -- files which have been added or changed
    Map.foldlWithKey' (const (scanOldEntry str ft')) (return ()) ft
    Map.foldlWithKey' (const (scanNewEntry str ft)) (return ()) ft'

    -- If the snapshot tree changed, create a new commit to reflect it
    toid' <- writeTree str
    sc'   <- if toid /= toid'
            then do
                now <- liftIO getCurrentTime
                let sig = Signature
                          { signatureName  = name
                          , signatureEmail = email
                          , signatureWhen  = now
                          }
                    msg = "Snapshot"

                c <- createCommit [commitRef sc] (treeRef str)
                                  sig sig msg (Just sref)
                infoL $ "Commit "
                     ++ (T.unpack . renderObjOid . commitOid $ c)
                return c
            else return sc

    -- Wait a given number of seconds
    liftIO $ threadDelay (interval opts * 1000000)

    -- Rinse, wash, repeat.
    ref' <- lookupRef "HEAD"
    let curRef = case ref' of
            Just (Reference _ (RefSymbolic ref'')) -> ref''
            _ -> ""
    if ref /= curRef
        then infoL $ "Branch changed to " ++ T.unpack curRef
                  ++ ", restarting"
        else loop sc' str toid' ft'

  where
    scanOldEntry :: (Repository m, MonadIO m)
                 => Tree m
                 -> Map FilePath (FileEntry m)
                 -> FilePath -> FileEntry m -> m ()
    scanOldEntry str ft fp _ = case Map.lookup fp ft of
        Nothing -> do
            infoL $ "Removed: " ++ fileStr fp
            dropFromTree str fp
        _ -> return ()

    scanNewEntry :: (Repository m, MonadIO m)
                 => Tree m
                 -> Map FilePath (FileEntry m)
                 -> FilePath -> FileEntry m -> m ()
    scanNewEntry str ft fp (FileEntry mt (BlobEntry oid exe) foid) =
        case Map.lookup fp ft of
            Nothing -> do
                infoL $ "Added to snapshot: " ++ fileStr fp
                putBlob' str fp oid exe
            Just (FileEntry oldMt (BlobEntry oldOid oldExe) fileOid)
                | oid /= oldOid || exe /= oldExe -> do
                    infoL $ "Changed: " ++ fileStr fp
                    putBlob' str fp oid exe
                | mt /= oldMt || oid /= fileOid -> do
                    path <- fileStr <$>
                            (liftIO $ canonicalizePath (wd </> fp))
                    infoL $ "Changed: " ++ fileStr fp
                    contents <- liftIO $ B.readFile path
                    newOid   <- createBlob (BlobString contents)
                    putBlob' str fp newOid exe
                | otherwise -> return ()
            _ -> return ()

data FileEntry m = FileEntry
    { fileModTime   :: UTCTime
    , fileBlobEntry :: TreeEntry m
    , fileChecksum  :: BlobOid m
    }

type FileTree m = Map FilePath (FileEntry m)

readFileTree :: (Repository m, MonadIO m)
             => Text -> FilePath -> Bool -> m (FileTree m)
readFileTree ref wdir getHash = do
    h <- resolveRef ref
    case h of
        Nothing -> pure Map.empty
        Just h' -> do
            tr <- resolveTreeRef . commitTree =<< resolveCommitRef h'
            readFileTree' tr wdir getHash

readFileTree' :: (Repository m, MonadIO m)
              => Tree m -> FilePath -> Bool -> m (FileTree m)
readFileTree' tr wdir getHash = do
    blobs <- treeBlobEntries tr
    foldlM (\m (fp,ent) -> do
                 fent <- readModTime wdir getHash fp ent
                 return $ maybe m (flip (Map.insert fp) m) fent)
           Map.empty blobs

readModTime :: (Repository m, MonadIO m)
            => FilePath -> Bool -> FilePath -> TreeEntry m
            -> m (Maybe (FileEntry m))
readModTime wdir getHash fp ent = do
    let path = wdir </> fp
    debugL $ "Checking file: " ++ fileStr path
    exists <- liftIO $ isFile path
    if exists
        then Just <$>
             (FileEntry
                  <$> liftIO (getModified path)
                  <*> pure ent
                  <*> if getHash
                      then do contents <- liftIO $ B.readFile (fileStr path)
                              hashContents (BlobString contents)
                      else return (blobEntryOid ent))
        else return Nothing

fileStr :: FilePath -> String
fileStr = TL.unpack . toTextIgnore

infoL :: (Repository m, MonadIO m) => String -> m ()
infoL = liftIO . infoM "git-monitor"

debugL :: (Repository m, MonadIO m) => String -> m ()
debugL = liftIO . debugM "git-monitor"

-- Main.hs (git-monitor) ends here
