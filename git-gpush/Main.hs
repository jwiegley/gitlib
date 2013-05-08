g{-# LANGUAGE OverloadedStrings #-}
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
import           Data.Char
import           Data.Foldable hiding (concat,elem)
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

volume :: Options -> Sh a -> Sh a
volume opts = if verbose opts then verbosely else silently

sh :: MonadIO m => Options -> Sh a -> m a
sh opts = shelly . volume opts

git :: [Text] -> Sh Text
git = run "git"

git_ :: [Text] -> Sh ()
git_ = run_ "git"

main :: IO ()
main = withLibGitDo $ execParser opts >>= pushToGitHub
  where
    opts = info (helper <*> options)
                (fullDesc <> progDesc desc <> header hdr)
    hdr  = "git-gpush 1.0.0 - push to GitHub with smarter behavior"
    desc = "\nDescription goes here."

pushToGitHub :: Options -> IO ()
pushToGitHub opts = do
    gd <- fromText . TL.init
              <$> (sh opts $ run "git" ["rev-parse", "--git-dir"])

    remoteName <- case remote opts of
        Nothing -> sh opts $ getRemoteName gd
        Just x  -> return (TL.pack x)

    remoteHead <-
        head . TL.words . TL.init
            <$> (sh opts $ git [ "ls-remote", remoteName, "HEAD" ])

    withRepository lgFactory gd $ do
        cref <- fromJust <$> resolveRef "HEAD"
        hc   <- resolveCommitRef cref
        let hcoid = renderObjOid (commitOid hc)
        rcoid <- Tagged <$> parseOid (TL.toStrict remoteHead)
        objs <- missingObjects
                    (Just (CommitObjectId rcoid))
                    (CommitObjectId (commitOid hc))
        for_ objs $ \obj -> case obj of
            Git.CommitObj cref -> do
                commit <- resolveCommitRef cref
                sh opts $ processCommitTags (commitLog commit)
            _ -> return ()

    unless (dryRun opts) $
        sh opts $ git_ $ [ "push", remoteName ] <> map TL.pack (args opts)

getRemoteName :: FilePath -> Sh Text
getRemoteName gd = do
    mref <- liftIO $ withRepository lgFactory gd $ lookupRef "HEAD"
    case mref of
        Nothing -> error "Could not find HEAD"
        Just (Reference _ (RefObj _)) ->
            error "Cannot push from a detached HEAD"
        Just (Reference _ (RefSymbolic (TL.fromStrict -> branch)))
            | "refs/heads/" `TL.isPrefixOf` branch ->
                TL.init <$> git [ "config"
                                , "branch." <> base branch <> ".remote" ]
            | otherwise ->
                error "Cannot push from a branch outside refs/heads"
  where
    base = TL.drop (TL.length "refs/heads/")

data CommitTag
    = ConfirmTag
      { confirmIssue  :: Text
      , confirmReport :: Maybe Text
      }
    deriving (Eq, Show)

findTags :: Text -> [CommitTag]
findTags = concat . map go . tails . map norm . TL.words
  where
    go [] = []
    go [x] = []
    go ["confirm", TL.splitOn ":" -> [issue]] =
        [ ConfirmTag (TL.tail issue) Nothing | isIssue issue ]
    go ["confirm", TL.splitOn ":" -> [issue, report]] =
        [ ConfirmTag (TL.tail issue) (Just report) | isIssue issue ]
    go _ = []

    isIssue issue = not (TL.null issue) && TL.head issue == '#'

    norm = TL.filter $ \c -> c `elem` "#:_-" || isDigit c || isLetter c

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

            -- jww (2013-04-28): Instead of using ghi here, we should use the
            -- github library, after reading in ghi.token.
            run_ "ghi"
                $ [ "edit", "-L", "need confirmation" ]
               <> [ x | x <- [ "-u", fromJust mreport' ], isJust mreport' ]
               <> [ issue ]

-- Main.hs (git-gpush) ends here
