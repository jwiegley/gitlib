{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Data.Text as T
import qualified Git.Libgit2 as Lg
import qualified Git.GitHub as Git
import qualified Git.Smoke as Git
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = do
    owner <- T.pack <$> getEnv "GITHUB_OWNER"
    repo  <- T.pack <$> getEnv "GITHUB_REPO"
    token <- T.pack <$> getEnv "GITHUB_TOKEN"
    summary <-
        hspecWith
        (defaultConfig { configVerbose = True })
        (Git.smokeTestSpec
         (\path _ act ->
           Git.withGitHubRepository owner repo token act))
    when (summaryFailures summary > 0) $ exitFailure
    return ()

-- Smoke.hs ends here
