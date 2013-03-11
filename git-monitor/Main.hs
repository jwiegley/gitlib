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
import           Data.Map (Map)
import qualified Data.Map as Map (traverseWithKey, lookup, fromList, empty)
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T (unpack)
import qualified Data.Text.Lazy as TL (unpack, toStrict, init)
import           Data.Time.Clock (UTCTime, getCurrentTime)
import           Filesystem (getModified)
import           Filesystem.Path.CurrentOS (FilePath, (</>))
import           Git
import           Git.Libgit2 (withLgRepository)
import           Git.Utils (treeBlobEntries)
import           Options.Applicative
import           Prelude hiding (FilePath)
import           Shelly (toTextIgnore, silently, shelly, run)
import           System.IO (stderr)
import           System.Log.Formatter (tfLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

instance Read FilePath

data Options = Options
    { verbose    :: Bool
    , gitDir     :: FilePath
    , workingDir :: FilePath
    , interval   :: Int
    , resume     :: Bool
    }

options :: Parser Options
options = Options
    <$> switch (short 'v' <> long "verbose" <> help "Display statistics")
    <*> option (long "git-dir" <> value ".git"
                <> help "Git repository to store snapshots in")
    <*> option (short 'd' <> long "dir" <> value "."
                <> help "The working tree to snapshot")
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
    desc = "Passively snapshot working tree changes efficiently.\n\
           \These are kept in refs/snapshots/refs/heads/$BRANCH"

doMain :: Options -> IO ()
doMain opts = do
    -- Setup logging service if --verbose is used
    when (verbose opts) initLogging

    -- Ask Git for the user name and email in this repository
    (userName,userEmail) <- shelly $ silently $
        (,) <$> (TL.init <$> run "git" ["config", "user.name"])
            <*> (TL.init <$> run "git" ["config", "user.email"])

    -- Make sure we're in a known branch, and if so, let it begin
    withLgRepository (gitDir opts) True False $ do
        ref <- lookupRef "HEAD"
        void $ case ref of
            Just (Reference _ (RefSymbolic name)) ->
                start (TL.toStrict userName) (TL.toStrict userEmail) name
            _ -> error "Cannot use git-monitor if no branch is checked out"
  where
    initLogging = do
        let level | verbose opts = INFO
                  | otherwise    = NOTICE
        h <- (`setFormatter` tfLogFormatter "%H:%M:%S" "$time - [$prio] $msg")
             <$> streamHandler System.IO.stderr level
        removeAllHandlers
        updateGlobalLogger "git-monitor" (setLevel level)
        updateGlobalLogger "git-monitor" (addHandler h)

    start userName userEmail ref = do
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
        scr    <- if resume opts
                  then resolveRef sref
                  else return Nothing
        scr'   <- maybe (fromJust <$> resolveRef "HEAD") return scr
        sc     <- resolveCommitRef scr'
        str    <- resolveTreeRef (commitTree sc)
        strOid <- writeTree str
        ft     <- readFileTree' str (workingDir opts) (isNothing scr)
        return (str, Just sc, Just strOid, ft)

        -- Begin the snapshotting process, which continues indefinitely until
        -- the process is stopped.  It is safe to cancel this process at any
        -- time, typically using SIGINT (C-c) or even SIGKILL.
        (snapshotTree opts userName userEmail ref sref) sc str strOid ft

snapshotTree :: (Repository m, MonadIO m)
             => Options
             -> Text -> Text -> Text -> Text
             -> Commit m -> Tree m -> TreeOid m
             -> Map FilePath (FileEntry m)
             -> m ()
snapshotTree opts name email ref sref = fix $ \f sc str strOid ft -> do
    -- Read the current working tree's state on disk
    ft' <- readFileTree ref (workingDir opts) False

    -- Prune files which have been dropped from the last interval
    void $ flip Map.traverseWithKey ft $
        \fp _ -> case Map.lookup fp ft' of
            Nothing -> do
                infoL $ "Removed: " ++ TL.unpack (toTextIgnore fp)
                dropFromTree str fp
            _ -> return ()

    -- Find files which have been added or changed since last interval
    void $ flip Map.traverseWithKey ft' $
        \fp (FileEntry mt (BlobEntry oid exe) foid) ->
            case Map.lookup fp ft of
                Nothing -> do
                    infoL $ "Added to snapshot: "
                         ++ TL.unpack (toTextIgnore fp)
                    putBlob' str fp oid exe
                Just (FileEntry oldMt (BlobEntry oldOid oldExe) fileOid)
                    | oid /= oldOid || exe /= oldExe -> do
                        infoL $ "Changed (hash or exe flag): "
                              ++ TL.unpack (toTextIgnore fp)
                        putBlob' str fp oid exe
                    | mt /= oldMt || oid /= fileOid -> changed str fp exe
                    | otherwise -> return ()
                _ -> return ()

    -- If the snapshot tree has changed, create a new commit to reflect it
    strOid' <- writeTree str

    sc' <- if strOid /= strOid'
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
              infoL $ "Committed: "
                    ++ (T.unpack . renderObjOid . commitOid $ c)
              return c
          else return sc

    -- Wait a given number of seconds
    liftIO $ threadDelay (interval opts * 1000000)

    -- Rinse, wash, repeat.
    f sc' str strOid' ft'
  where
    changed :: (Repository m, MonadIO m) => Tree m -> FilePath -> Bool -> m ()
    changed str fp exe = do
        let path = TL.unpack (toTextIgnore fp)
        infoL $ "Changed: " ++ path
        contents <- liftIO $ B.readFile path
        newOid   <- createBlob (BlobString contents)
        putBlob' str fp newOid exe

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
    Map.traverseWithKey (readModTime wdir getHash)
                        (Map.fromList blobs)

readModTime :: (Repository m, MonadIO m)
            => FilePath -> Bool -> FilePath -> TreeEntry m -> m (FileEntry m)
readModTime wdir getHash fp ent =
    FileEntry
        <$> liftIO (getModified (wdir </> fp))
        <*> pure ent
        <*> if getHash
            then do
                contents <-
                    liftIO $
                    B.readFile (TL.unpack (toTextIgnore (wdir </> fp)))
                hashContents (BlobString contents)
            else return (blobEntryOid ent)

infoL :: (Repository m, MonadIO m) => String -> m ()
infoL = liftIO . infoM "git-monitor"

-- Main.hs (git-monitor) ends here
