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
import           Control.Monad.Trans.Cont
import qualified Data.ByteString as B (readFile)
import           Data.Foldable
import           Data.Function (fix)
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Tagged
import qualified Data.Text as T (pack, unpack)
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import           Data.Time
import           Data.Traversable
import           Filesystem (getModified, isDirectory, isFile, canonicalizePath)
import           Filesystem.Path.CurrentOS (FilePath, (</>), parent, null)
import           Git hiding (Options)
import           Git.Libgit2 (lgFactory, withLibGitDo)
import           Git.Utils (treeBlobEntries)
import           Options.Applicative
import           Prelude hiding (FilePath, null)
import           Shelly
import           System.IO (stderr)
import           System.Locale (defaultTimeLocale)
import           System.Log.Formatter (tfLogFormatter)
import           System.Log.Handler (setFormatter)
import           System.Log.Handler.Simple (streamHandler)
import           System.Log.Logger

instance Read FilePath

data Options = Options
    { verbose    :: Bool
    , debug      :: Bool
    , gitDir     :: String
    , remote     :: Maybe String
    , args       :: [String]
    }

options :: Parser Options
options = Options
    <$> switch (short 'v' <> long "verbose" <> help "Display info")
    <*> switch (short 'D' <> long "debug" <> help "Display debug")
    <*> strOption
        (long "git-dir" <> value ".git" <> help "Git repository to push")
    <*> argument (Just . Just) (value Nothing)
    <*> arguments Just hidden

main :: IO ()
main = withLibGitDo $ execParser opts >>= doMain
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc desc <> header hdr)
    hdr  = "git-gpush 1.0.0 - push to GitHub with smarter behavior"
    desc = "\nDescription goes here."

doMain :: Options -> IO ()
doMain opts = do
    -- Setup logging service if --verbose is used
    when (verbose opts) $ initLogging (debug opts)

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

    pushToGitHub opts gd
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

pushToGitHub :: Options -> FilePath -> IO ()
pushToGitHub opts gd = do
    -- Determine the current HEAD at the remote
    remoteName <- case remote opts of
        Nothing -> return "origin" -- jww (2013-04-28): bug
        Just x  -> return (TL.pack x)

    remoteHead <-
        head . TL.words . TL.init
            <$> (shellyNoDir $ verbosely $
                 git [ "ls-remote", remoteName, "HEAD" ])

    putStrLn $ "remoteHead is " ++ show remoteHead

    withRepository lgFactory gd $ do
        cref <- resolveRef "HEAD"
        for cref $ \cref -> do
            hc <- resolveCommitRef cref
            let hcoid = renderObjOid (commitOid hc)
            rcoid <- Tagged <$> parseOid (TL.toStrict remoteHead)
            liftIO $ putStrLn $ "localHead is " ++ show hcoid
            objs <- missingObjects
                        (Just (CommitObjectId rcoid))
                        (CommitObjectId (commitOid hc))
            for_ objs $ \obj -> case obj of
                Git.CommitObj cref -> do
                    c <- resolveCommitRef cref
                    liftIO $ putStrLn $ "Commit: " ++ show (commitOid c)
                    --commitLog c
                _ -> return ()

    -- shellyNoDir $ verbosely $
    --     git_ $ [ "push", remoteName ] <> map TL.pack (args opts)
    return ()

git :: [Text] -> Sh Text
git = run "git"

git_ :: [Text] -> Sh ()
git_ = run_ "git"

fileStr :: FilePath -> String
fileStr = TL.unpack . toTextIgnore

infoL :: (Repository m, MonadIO m) => String -> m ()
infoL = liftIO . infoM "git-monitor"

debugL :: (Repository m, MonadIO m) => String -> m ()
debugL = liftIO . debugM "git-monitor"

-- Main.hs (git-monitor) ends here
