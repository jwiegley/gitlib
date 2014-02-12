{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Logger
import           Control.Monad.Trans.Class
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as B8
import           Data.Foldable (foldlM)
import           Data.Function (fix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Tagged
import qualified Data.Text as T
#if MIN_VERSION_shelly(1, 0, 0)
import qualified Data.Text as TL
#else
import qualified Data.Text.Lazy as TL
#endif
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Git hiding (Options)
import           Git.Libgit2 (MonadLg, LgRepo, lgFactoryLogger)
import           Options.Applicative
import           Shelly (silently, shelly, run)
import           System.Directory
import           System.FilePath.Posix
import           System.IO (stderr)
import           System.Locale (defaultTimeLocale)
import           System.Log.FastLogger
import           System.Log.Formatter (tfLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger
import           System.Posix.Files

logMLogger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logMLogger _loc src lvl str =
    logM (T.unpack src) (prio lvl) (convert str)
  where
    prio LevelDebug     = DEBUG
    prio LevelInfo      = INFO
    prio LevelWarn      = WARNING
    prio LevelError     = ERROR
    prio (LevelOther _) = INFO

    convert = T.unpack . T.decodeUtf8 . fromLogStr

toStrict :: TL.Text -> T.Text
#if MIN_VERSION_shelly(1, 0, 0)
toStrict = id
#else
toStrict = TL.toStrict
#endif

fromStrict :: T.Text -> TL.Text
#if MIN_VERSION_shelly(1, 0, 0)
fromStrict = id
#else
fromStrict = TL.fromStrict
#endif

data Options = Options
    { quiet      :: Bool
    , verbose    :: Bool
    , debug      :: Bool
    , gitDir     :: FilePath
    , workingDir :: FilePath
    , interval   :: Int
    , resume     :: Bool
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
    desc = "\nPassively snapshot working tree changes efficiently.\n\nThe intended usage is to run \"git monitor &\" in your project\ndirectory before you begin a hacking session.\n\nSnapshots are kept in refs/snapshots/refs/heads/$BRANCH"

doMain :: Options -> IO ()
doMain opts = do
    -- Setup logging service if --verbose is used
    unless (quiet opts) $ initLogging (debug opts)

    -- Ask Git for the user name and email in this repository
    (userName,userEmail) <- shelly $ silently $
        (,) <$> (TL.init <$> run "git" ["config", "user.name"])
            <*> (TL.init <$> run "git" ["config", "user.email"])

    let gDir = gitDir opts
    isDir <- doesDirectoryExist gDir
    gd    <- if isDir
             then return gDir
             else shelly $ silently $
                  TL.unpack . TL.init <$> run "git" ["rev-parse", "--git-dir"]

    let wDir = workingDir opts
        wd   = if null wDir then takeDirectory gd else wDir

    -- Make sure we're in a known branch, and if so, let it begin
    forever $ flip runLoggingT logMLogger $
        withRepository lgFactoryLogger gd $ do
            infoL $ "Saving snapshots under " ++ gd
            infoL $ "Working tree: " ++ wd
            ref <- lookupReference "HEAD"
            case ref of
                Just (RefSymbolic name) -> do
                    infoL $ "Tracking branch " ++ T.unpack name
                    void $ start wd (toStrict userName) (toStrict userEmail)
                        name
                _ -> do
                    infoL "Cannot use git-monitor if no branch is checked out"
                    liftIO $ threadDelay (interval opts * 1000000)
  where
    initLogging debugMode = do
        let level | debugMode = DEBUG
                  | otherwise = INFO
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
                then resolveReference sref
                else return Nothing
        scr' <- maybe (fromJust <$> resolveReference "HEAD") return scr
        sc   <- lookupCommit (Tagged scr')
        let toid = commitTree sc
        tree <- lookupTree toid
        ft   <- readFileTree' tree wd (isNothing scr)

        -- Begin the snapshotting process, which continues indefinitely until
        -- the process is stopped.  It is safe to cancel this process at any
        -- time, typically using SIGINT (C-c) or even SIGKILL.
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
             -> Map TreeFilePath (FileEntry LgRepo)
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
              lift $ infoL $ "Commit "
                  ++ (T.unpack . renderObjOid . commitOid $ c)
              return c
          else return sc

    -- Wait a given number of seconds
    liftIO $ threadDelay (interval opts * 1000000)

    -- Rinse, wash, repeat.
    ref' <- lift $ lookupReference "HEAD"
    let curRef = case ref' of Just (RefSymbolic ref'') -> ref''; _ -> ""
    if ref /= curRef
        then lift $ infoL $ "Branch changed to " ++ T.unpack curRef
            ++ ", restarting"
        else loop sc' toid' ft'

  where
    scanOldEntry :: (MonadGit LgRepo m, MonadLg m)
                 => Map TreeFilePath (FileEntry LgRepo)
                 -> TreeFilePath
                 -> FileEntry LgRepo
                 -> TreeT LgRepo m ()
    scanOldEntry ft fp _ = case Map.lookup fp ft of
        Nothing -> do
            lift . infoL $ "Removed: " <> B8.unpack fp
            dropEntry fp
        _ -> return ()

    scanNewEntry :: (MonadGit LgRepo m, MonadLg m)
                 => Map TreeFilePath (FileEntry LgRepo)
                 -> TreeFilePath
                 -> FileEntry LgRepo
                 -> TreeT LgRepo m ()
    scanNewEntry ft fp (FileEntry mt oid kind _) =
        case Map.lookup fp ft of
            Nothing -> do
                lift . infoL $ "Added to snapshot: " ++ B8.unpack fp
                putBlob' fp oid kind
            Just (FileEntry oldMt oldOid oldKind fileOid)
                | oid /= oldOid || kind /= oldKind -> do
                    lift . infoL $ "Changed: " ++ B8.unpack fp
                    putBlob' fp oid kind
                | mt /= oldMt || oid /= fileOid -> do
                    lift . infoL $ "Changed: " ++ B8.unpack fp
                    path <- liftIO $ canonicalizePath (wd </> B8.unpack fp)
                    contents <- liftIO $ B.readFile path
                    newOid   <- lift $ createBlob (BlobString contents)
                    putBlob' fp newOid kind
                | otherwise -> return ()

data FileEntry m = FileEntry
    { fileModTime  :: UTCTime
    , fileBlobOid  :: BlobOid m
    , fileBlobKind :: BlobKind
    , fileChecksum :: BlobOid m
    }

type FileTree m = Map TreeFilePath (FileEntry m)

readFileTree :: (MonadGit LgRepo m, MonadLg m)
             => RefName
             -> FilePath
             -> Bool
             -> m (FileTree LgRepo)
readFileTree ref wdir getHash = do
    h <- resolveReference ref
    case h of
        Nothing -> pure Map.empty
        Just h' -> do
            tr <- lookupTree . commitTree =<< lookupCommit (Tagged h')
            readFileTree' tr wdir getHash

readFileTree' :: (MonadGit LgRepo m, MonadLg m)
              => Tree LgRepo -> FilePath -> Bool
              -> m (FileTree LgRepo)
readFileTree' tr wdir getHash = do
    blobs <- treeBlobEntries tr
    foldlM (\m (fp,oid,kind) -> do
                 fent <- readModTime wdir getHash (B8.unpack fp) oid kind
                 return $ maybe m (flip (Map.insert fp) m) fent)
           Map.empty blobs

readModTime :: (MonadGit LgRepo m, MonadLg m)
            => FilePath
            -> Bool
            -> FilePath
            -> BlobOid LgRepo
            -> BlobKind
            -> m (Maybe (FileEntry LgRepo))
readModTime wdir getHash fp oid kind = do
    let path = wdir </> fp
    debugL $ "Checking file: " ++ path
    estatus <- liftIO $ try $ getSymbolicLinkStatus path
    case (estatus :: Either SomeException FileStatus) of
        Right status | isRegularFile status ->
            Just <$> (FileEntry
                          <$> pure (posixSecondsToUTCTime
                                    (realToFrac (modificationTime status)))
                          <*> pure oid
                          <*> pure kind
                          <*> if getHash
                              then hashContents . BlobString
                                  =<< liftIO (B.readFile path)
                              else return oid)
        _ -> return Nothing

infoL :: MonadIO m => String -> m ()
infoL = liftIO . infoM "git-monitor"

debugL :: MonadIO m => String -> m ()
debugL = liftIO . debugM "git-monitor"

-- Main.hs (git-monitor) ends here
