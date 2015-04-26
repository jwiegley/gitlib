{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Logging
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as B8
import           Data.Function (fix)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Tagged
import           Data.Text (pack)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Git hiding (Options)
import           Git.Tree.Working
import           Git.Libgit2 (MonadLg, LgRepo, lgFactoryLogger)
import           Options.Applicative
import           Prelude hiding (log)
import           Shelly (silently, shelly, run)
import           System.Directory
import           System.FilePath.Posix
#if !MIN_VERSION_time (1,5,0)
import           System.Locale (defaultTimeLocale)
#endif

data Options = Options
    { optQuiet      :: Bool
    , optVerbose    :: Bool
    , optDebug      :: Bool
    , optGitDir     :: FilePath
    , optWorkingDir :: FilePath
    , optInterval   :: Int
    , optResume     :: Bool
    }

options :: Parser Options
options = Options
    <$> switch (short 'q' <> long "quiet" <> help "Do not display progress")
    <*> switch (short 'v' <> long "verbose" <> help "Display more info")
    <*> switch (short 'D' <> long "debug" <> help "Display debug")
    <*> strOption
        (long "git-dir" <> value ".git"
         <> help "Git repository to store snapshots in (def: \".git\")")
    <*> strOption (short 'd' <> long "work-dir" <> value ""
                   <> help "The working tree to snapshot (def: \".\")")
    <*> option auto (short 'i' <> long "interval" <> value 60
                <> help "Snapshot each N seconds")
    <*> switch (short 'r' <> long "resume"
                <> help "Resumes using last set of snapshots")

main :: IO ()
main = execParser opts >>= withStdoutLogging . doMain
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc desc <> header hdr)
    hdr  = "git-monitor 3.1.1.2 - quickly snapshot working tree changes"
    desc = "\nPassively snapshot working tree changes efficiently.\n\nThe intended usage is to run \"git monitor &\" in your project\ndirectory before you begin a hacking session.\n\nSnapshots are kept in refs/snapshots/refs/heads/$BRANCH"

doMain :: Options -> IO ()
doMain opts = do
    -- Setup logging service if --verbose is used
    setLogTimeFormat "%H:%M:%S"
    setLogLevel $ if optQuiet opts
                  then LevelError
                  else if optDebug opts
                       then LevelDebug
                       else LevelInfo

    -- Ask Git for the user name and email in this repository
    (userName,userEmail) <- shelly $ silently $
        (,) <$> (T.init <$> run "git" ["config", "user.name"])
            <*> (T.init <$> run "git" ["config", "user.email"])

    let gDir = optGitDir opts
    isDir <- doesDirectoryExist gDir
    gd    <- if isDir
             then return gDir
             else shelly $ silently $
                  T.unpack . T.init <$> run "git" ["rev-parse", "--git-dir"]

    let wDir = optWorkingDir opts
        wd   = if null wDir then takeDirectory gd else wDir

    -- Make sure we're in a known branch, and if so, let it begin
    forever $ withRepository lgFactoryLogger gd $ do
        log' $ pack $ "Working tree: " ++ wd
        ref <- lookupReference "HEAD"
        case ref of
            Just (RefSymbolic name) -> do
                log' $ "Tracking branch " <> name
                log' $ "Saving snapshots under " <> T.pack gd
                log' $ "Snapshot ref is refs/snapshots/" <> name
                void $ start wd userName userEmail name
            _ -> do
                log' "Cannot use git-monitor if no branch is checked out"
                liftIO $ threadDelay (optInterval opts * 1000000)
  where
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
        scr  <- if optResume opts
                then resolveReference sref
                else return Nothing
        scr' <- maybe (resolveReference "HEAD") (return . Just) scr
        case scr' of
            Nothing -> errorL "Failed to lookup reference"
            Just r -> do
                sc   <- lookupCommit (Tagged r)
                let toid = commitTree sc
                tree <- lookupTree toid
                ft   <- readFileTree' tree wd (isNothing scr)

                -- Begin the snapshotting process, which continues
                -- indefinitely until the process is stopped.  It is safe to
                -- cancel this process at any time, typically using SIGINT
                -- (C-c) or even SIGKILL.
                mutateTreeOid toid $
                    snapshotTree opts wd userName userEmail ref sref sc toid ft

-- | 'snapshotTree' is the core workhorse of this utility.  It periodically
--   checks the filesystem for changes to Git-tracked files, and snapshots any
--   changes that have occurred in them.
snapshotTree :: (MonadGit LgRepo m, MonadLg m)
             => Options
             -> FilePath
             -> CommitAuthor
             -> CommitEmail
             -> RefName
             -> RefName
             -> Commit LgRepo
             -> TreeOid LgRepo
             -> HashMap TreeFilePath (FileEntry LgRepo)
             -> TreeT LgRepo m ()
snapshotTree opts wd name email ref sref = fix $ \loop sc toid ft -> do
    -- Read the current working tree's state on disk
    ft' <- lift $ readFileTree ref wd False

    -- Prune files which have been removed since the last interval, and find
    -- files which have been added or changed
    Map.foldlWithKey' (\a p e -> a >> scanOldEntry ft' p e) (return ()) ft
    Map.foldlWithKey' (\a p e -> a >> scanNewEntry ft p e) (return ()) ft'

    toid' <- currentTreeOid

    -- If the snapshot tree changed, create a new commit to reflect it
    sc' <- if toid /= toid'
          then do
              now <- liftIO getZonedTime
              let sig = Signature
                        { signatureName  = name
                        , signatureEmail = email
                        , signatureWhen  = now
                        }
                  msg = "Snapshot at "
                     ++ formatTime defaultTimeLocale "%F %T %Z" now

              c <- lift $ createCommit [commitOid sc] toid'
                  sig sig (T.pack msg) (Just sref)
              lift $ log' $ "Commit " <> (renderObjOid . commitOid $ c)
              return c
          else return sc

    -- Wait a given number of seconds
    liftIO $ threadDelay (optInterval opts * 1000000)

    -- Rinse, wash, repeat.
    ref' <- lift $ lookupReference "HEAD"
    let curRef = case ref' of Just (RefSymbolic ref'') -> ref''; _ -> ""
    if ref /= curRef
        then lift $ log' $ "Branch changed to " <> curRef <> ", restarting"
        else loop sc' toid' ft'

  where
    scanOldEntry :: (MonadGit LgRepo m, MonadLg m)
                 => HashMap TreeFilePath (FileEntry LgRepo)
                 -> TreeFilePath
                 -> FileEntry LgRepo
                 -> TreeT LgRepo m ()
    scanOldEntry ft fp _ = case Map.lookup fp ft of
        Nothing -> do
            log' $ "Removed: " <> T.decodeUtf8 fp
            dropEntry fp
        _ -> return ()

    scanNewEntry :: (MonadGit LgRepo m, MonadLg m)
                 => HashMap TreeFilePath (FileEntry LgRepo)
                 -> TreeFilePath
                 -> FileEntry LgRepo
                 -> TreeT LgRepo m ()
    scanNewEntry ft fp (FileEntry mt oid kind _) =
        case Map.lookup fp ft of
            Nothing -> do
                log' $ "Added to snapshot: " <> T.decodeUtf8 fp
                putBlob' fp oid kind
            Just (FileEntry oldMt oldOid oldKind fileOid)
                | oid /= oldOid || kind /= oldKind -> do
                    log' $ "Changed: " <> T.decodeUtf8 fp
                    putBlob' fp oid kind
                | mt /= oldMt || oid /= fileOid -> do
                    log' $ "Changed: " <> T.decodeUtf8 fp
                    path <- liftIO $ canonicalizePath (wd </> B8.unpack fp)
                    contents <- liftIO $ B.readFile path
                    newOid   <- lift $ createBlob (BlobString contents)
                    putBlob' fp newOid kind
                | otherwise -> return ()

-- Main.hs (git-monitor) ends here
