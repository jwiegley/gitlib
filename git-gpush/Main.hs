{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Arrow (first)
import           Control.Concurrent (threadDelay)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Cont
import qualified Data.ByteString as B (readFile)
import           Data.Foldable hiding (concat)
import           Data.Function (fix)
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Tagged
import qualified Data.Text as T
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL hiding (tails)
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
    , dryRun     :: Bool
    , remote     :: Maybe String
    , args       :: [String]
    }

options :: Parser Options
options = Options
    <$> switch (short 'v' <> long "verbose" <> help "Display info")
    <*> switch (short 'n' <> long "dry-run" <> help "Dry run")
    <*> argument (Just . Just) (value Nothing)
    <*> arguments Just hidden

main :: IO ()
main = withLibGitDo $ execParser opts >>= pushToGitHub
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc desc <> header hdr)
    hdr  = "git-gpush 1.0.0 - push to GitHub with smarter behavior"
    desc = "\nDescription goes here."

volume :: Options -> Sh a -> Sh a
volume opts = if verbose opts then verbosely else silently

pushToGitHub :: Options -> IO ()
pushToGitHub opts = do
    -- Determine the current HEAD at the remote
    remoteName <- case remote opts of
        Nothing -> return "origin" -- jww (2013-04-28): bug
        Just x  -> return (TL.pack x)

    remoteHead <-
        head . TL.words . TL.init
            <$> (shelly $ volume opts $
                 git [ "ls-remote", remoteName, "HEAD" ])

    gd <- shelly $ volume opts $
          fromText . TL.init <$> run "git" ["rev-parse", "--git-dir"]

    void $ withRepository lgFactory gd $ do
        cref <- resolveRef "HEAD"
        for cref $ \cref -> do
            hc <- resolveCommitRef cref
            let hcoid = renderObjOid (commitOid hc)
            rcoid <- Tagged <$> parseOid (TL.toStrict remoteHead)
            objs <- missingObjects
                        (Just (CommitObjectId rcoid))
                        (CommitObjectId (commitOid hc))
            for_ objs $ \obj -> case obj of
                Git.CommitObj cref -> do
                    c <- resolveCommitRef cref
                    shelly $ volume opts $
                        processCommitTags (commitLog c)
                _ -> return ()

    unless (dryRun opts) $
        shelly $ volume opts $
            git_ $ [ "push", remoteName ] <> map TL.pack (args opts)

data CommitTag
    = ConfirmTag
      { confirmIssue  :: Text
      , confirmReport :: Maybe Text
      }
    deriving (Eq, Show)

findTags :: Text -> [CommitTag]
findTags = concat . map go . tails . TL.words
  where
    go [] = []
    go [x] = []
    go ["confirm", TL.splitOn ":" -> [issue]] =
        [ ConfirmTag (TL.tail issue) Nothing | isIssue issue ]
    go ["confirm", TL.splitOn ":" -> [issue, report]] =
        [ ConfirmTag (TL.tail issue) (Just report) | isIssue issue ]
    go _ = []

    isIssue issue = not (TL.null issue) && TL.head issue == '#'

processCommitTags :: T.Text -> Sh ()
processCommitTags msg =
    for_ (findTags (TL.fromStrict msg)) $ \tag -> case tag of
        ConfirmTag issue mreport -> do
            mreport' <- case mreport of
                x@(Just report) -> return x
                Nothing -> errExit False $ do
                    who <- git [ "config", "fpco.directreport" ]
                    ec  <- lastExitCode
                    return $ if ec == 0
                        then Just (TL.init who)
                        else Nothing
            run_ "ghi"
                $ [ "edit", "-m"
                  , "Please confirm that this has been fixed."
                  , "-L", "need confirmation"
                  ]
               <> [ x | x <- [ "-u", fromJust mreport' ], isJust mreport' ]
               <> [ issue ]


git :: [Text] -> Sh Text
git = run "git"

git_ :: [Text] -> Sh ()
git_ = run_ "git"

-- Main.hs (git-monitor) ends here
