{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Text as T
import           Filesystem.Path.CurrentOS
import qualified Git
import qualified Git.Utils as Git
import qualified Git.GitHub as Gh
import qualified Git.Smoke as Git
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

ghFactory :: Git.MonadGit m
          => Text
          -> Maybe Text
          -> Git.RepositoryFactory Gh.GitHubRepository m Gh.Repository
ghFactory owner token = Gh.ghFactory (Gh.GitHubUser owner) token

main :: IO ()
main = do
    owner <- T.pack <$> getEnv "GITHUB_OWNER"
    token <- T.pack <$> getEnv "GITHUB_TOKEN"
    let f = ghFactory owner (Just token)
        g = ghFactory owner (Just token)
    hspec $ Git.smokeTestSpec f g

-- Smoke.hs ends here
