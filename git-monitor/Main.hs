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
import           Control.Monad.Trans.Class
import qualified Data.ByteString as B (readFile)
import           Data.Foldable (foldlM)
import           Data.Function (fix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
#if MIN_VERSION_shelly(1, 0, 0)
import qualified Data.Text as TL
#else
import qualified Data.Text.Lazy as TL
#endif
import           Data.Time
import           Filesystem (getModified, isDirectory, isFile, canonicalizePath)
import           Filesystem.Path.CurrentOS (FilePath, (</>), parent, null)
import           Git hiding (Options)
import           Git.Libgit2 (LgRepository, lgFactory, withLibGitDo)
import           Options.Applicative
import           Prelude hiding (FilePath, null)
import           Shelly (toTextIgnore, fromText, silently, shelly, run)
import           System.IO (stderr)
import           System.Locale (defaultTimeLocale)
import           System.Log.Formatter (tfLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

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

instance Read FilePath

data Options = Options
    { quiet      :: Bool
    , verbose    :: Bool
    , debug      :: Bool
    , gitDir     :: String
    , workingDir :: String
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
main = withLibGitDo $ execParser opts >>= doMain
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

    let gDir = fromText (TL.pack (gitDir opts))
    isDir <- isDirectory gDir
    gd    <- if isDir
             then return gDir
             else shelly $ silently $
                  fromText . TL.init <$> run "git" ["rev-parse", "--git-dir"]

    let wDir = fromText (TL.pack (workingDir opts))
        wd   = if null wDir then parent gd else wDir

    -- Make sure we're in a known branch, and if so, let it begin
    forever $ withRepository lgFactory gd $ do
        infoL $ "Saving snapshots under " ++ fileStr gd
        infoL $ "Working tree: " ++ fileStr wd
        ref <- lookupReference "HEAD"
        void $ case ref of
            Just (RefSymbolic name) -> do
                infoL $ "Tracking branch " ++ T.unpack name
                start wd (toStrict userName) (toStrict userEmail) name
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
        sc   <- lookupCommit scr'
        let toid = commitTree sc
        tree <- lookupTree toid
        ft   <- readFileTree' tree wd (isNothing scr)

        -- Begin the snapshotting process, which continues indefinitely until
        -- the process is stopped.  It is safe to cancel this process at any
        -- time, typically using SIGINT (C-c) or even SIGKILL.
        snapshotTree opts wd userName userEmail ref sref sc toid ft

-- | 'snapshotTree' is the core workhorse of this utility.  It periodically
--   checks the filesystem for changes to Git-tracked files, and snapshots
--   any changes that have occurred in them.
snapshotTree :: MonadGit m
             => Options
             -> FilePath
             -> Text
             -> Text
             -> Text
             -> Text
             -> Commit (LgRepository m)
             -> TreeOid (LgRepository m)
             -> Map Text (FileEntry (LgRepository m))
             -> LgRepository m ()
snapshotTree opts wd name email ref sref = fix $ \loop sc toid ft -> do
    -- Read the current working tree's state on disk
    ft' <- readFileTree ref wd False

    -- Prune files which have been removed since the last interval, and find
    -- files which have been added or changed
    toid' <- mutateTreeOid toid $ do
        Map.foldlWithKey' (\a p e -> a >> scanOldEntry ft' p e) (return ()) ft
        Map.foldlWithKey' (\a p e -> a >> scanNewEntry ft p e) (return ()) ft'

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

              c <- createCommit [commitOid sc] toid'
                                sig sig (T.pack msg) (Just sref)
              infoL $ "Commit "
                   ++ (T.unpack . renderObjOid . commitOid $ c)
              return c
          else return sc

    -- Wait a given number of seconds
    liftIO $ threadDelay (interval opts * 1000000)

    -- Rinse, wash, repeat.
    ref' <- lookupReference "HEAD"
    let curRef = case ref' of Just (RefSymbolic ref'') -> ref''; _ -> ""
    if ref /= curRef
        then infoL $ "Branch changed to " ++ T.unpack curRef
                  ++ ", restarting"
        else loop sc' toid' ft'

  where
    scanOldEntry :: MonadGit m
                 => Map Text (FileEntry (LgRepository m))
                 -> Text
                 -> FileEntry (LgRepository m)
                 -> TreeT (LgRepository m) ()
    scanOldEntry ft fp _ = case Map.lookup fp ft of
        Nothing -> do
            lift . infoL $ "Removed: " <> T.unpack fp
            dropEntry fp
        _ -> return ()

    scanNewEntry :: MonadGit m
                 => Map Text (FileEntry (LgRepository m))
                 -> Text
                 -> FileEntry (LgRepository m)
                 -> TreeT (LgRepository m) ()
    scanNewEntry ft fp (FileEntry mt oid kind _) =
        case Map.lookup fp ft of
            Nothing -> do
                lift . infoL $ "Added to snapshot: " ++ T.unpack fp
                putBlob' fp oid kind
            Just (FileEntry oldMt oldOid oldKind fileOid)
                | oid /= oldOid || kind /= oldKind -> do
                    lift . infoL $ "Changed: " ++ T.unpack fp
                    putBlob' fp oid kind
                | mt /= oldMt || oid /= fileOid -> do
                    path <- fileStr <$>
                            liftIO (canonicalizePath
                                    (wd </> fromText (fromStrict fp)))
                    lift . infoL $ "Changed: " ++ T.unpack fp
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

type FileTree m = Map Text (FileEntry m)

readFileTree :: MonadGit m
             => Text
             -> FilePath
             -> Bool
             -> LgRepository m (FileTree (LgRepository m))
readFileTree ref wdir getHash = do
    h <- resolveReference ref
    case h of
        Nothing -> pure Map.empty
        Just h' -> do
            tr <- lookupTree . commitTree =<< lookupCommit h'
            readFileTree' tr wdir getHash

readFileTree' :: MonadGit m
              => Tree (LgRepository m) -> FilePath -> Bool
              -> LgRepository m (FileTree (LgRepository m))
readFileTree' tr wdir getHash = do
    blobs <- treeBlobEntries tr
    foldlM (\m (fp,oid,kind) -> do
                 fent <- readModTime wdir getHash fp oid kind
                 return $ maybe m (flip (Map.insert fp) m) fent)
           Map.empty blobs

readModTime :: MonadGit m
            => FilePath
            -> Bool
            -> Text
            -> BlobOid (LgRepository m)
            -> BlobKind
            -> LgRepository m (Maybe (FileEntry (LgRepository m)))
readModTime wdir getHash fp oid kind = do
    let path = wdir </> fromText (fromStrict fp)
    debugL $ "Checking file: " ++ fileStr path
    exists <- liftIO $ isFile path
    if exists
        then Just <$>
             (FileEntry
                  <$> liftIO (getModified path)
                  <*> pure oid
                  <*> pure kind
                  <*> if getHash
                      then do contents <- liftIO $ evaluate
                                              =<< B.readFile (fileStr path)
                              hashContents (BlobString contents)
                      else return oid)
        else return Nothing

fileStr :: FilePath -> String
fileStr = TL.unpack . toTextIgnore

infoL :: (Repository m, MonadIO m) => String -> m ()
infoL = liftIO . infoM "git-monitor"

debugL :: (Repository m, MonadIO m) => String -> m ()
debugL = liftIO . debugM "git-monitor"

-- Main.hs (git-monitor) ends here
