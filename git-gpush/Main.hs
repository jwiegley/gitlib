{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.Char
import           Data.Foldable hiding (concat, concatMap, elem)
import           Data.List
import           Data.Maybe
import           Data.Tagged
import qualified Data.Text as T
#if MIN_VERSION_shelly(1, 0, 0)
import           Data.Text (Text)
import qualified Data.Text as TL hiding (tails)
#else
import           Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL hiding (tails)
#endif
import           Git hiding (Options)
import           Git.Libgit2 (lgFactory, withLibGitDo)
import           Options.Applicative
import           Prelude hiding (FilePath, null)
import           Shelly

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
              <$> sh opts (run "git" ["rev-parse", "--git-dir"])

    remoteName <- case remote opts of
        Nothing -> sh opts $ getRemoteName gd
        Just x  -> return (TL.pack x)

    remoteHead <-
        head . TL.words . TL.init
            <$> sh opts (git [ "ls-remote", remoteName, "HEAD" ])

    withRepository lgFactory gd $ do
        cref <- fromJust <$> resolveReference "HEAD"
        hc   <- lookupCommit cref
        rcoid <- Tagged <$> parseOid (toStrict remoteHead)
        objs <- missingObjects (Just rcoid) (commitOid hc)
        for_ objs $ \obj -> case obj of
            Git.CommitObjOid coid -> do
                commit <- lookupCommit coid
                sh opts $ processCommitTags (commitLog commit)
            _ -> return ()

    unless (dryRun opts) $
        sh opts $ git_ $ [ "push", remoteName ] <> map TL.pack (args opts)

getRemoteName :: FilePath -> Sh Text
getRemoteName gd = do
    mref <- liftIO $ withRepository lgFactory gd $ lookupReference "HEAD"
    case mref of
        Nothing -> error "Could not find HEAD"
        Just (Reference _ (RefObj _)) ->
            error "Cannot push from a detached HEAD"
        Just (Reference _ (RefSymbolic (fromStrict -> branch)))
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
findTags = concatMap go . tails . map norm . TL.words
  where
    go [] = []
    go [_] = []
    go ["confirm", TL.splitOn ":" -> [issue]] =
        [ ConfirmTag (TL.tail issue) Nothing | isIssue issue ]
    go ["confirm", TL.splitOn ":" -> [issue, report]] =
        [ ConfirmTag (TL.tail issue) (Just report) | isIssue issue ]
    go _ = []

    isIssue issue = not (TL.null issue) && TL.head issue == '#'

    norm = TL.filter $ \c -> c `elem` "#:_-" || isDigit c || isLetter c

processCommitTags :: T.Text -> Sh ()
processCommitTags msg =
    for_ (findTags (fromStrict msg)) $ \tag -> case tag of
        ConfirmTag issue mreport -> do
            mreport' <- case mreport of
                x@(Just _) -> return x
                Nothing -> errExit False $ do
                    who <- git [ "config", "fpco.directreport" ]
                    ec  <- lastExitCode
                    return $ if ec == 0
                        then Just (TL.init who)
                        else Nothing

            -- jww (2013-04-28): Instead of using ghi here, we should use the
            -- github library, after reading in ghi.token.
            run_ "ghi"
                $ [ "edit" ]
               <> [ x | x <- [ "-u", fromJust mreport' ], isJust mreport' ]
               <> [ issue ]
            run_ "ghi" [ "comment"
                       , "-m", "Please confirm that this has been fixed."
                       , issue ]
            run_ "ghi" [ "label", issue, "-a", "need confirmation" ]

-- Main.hs (git-gpush) ends here
