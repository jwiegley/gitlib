{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Data.Proxy
import           Data.Text as T
import           Filesystem.Path.CurrentOS
import qualified Git
import qualified Git.Utils as Git
import qualified Git.CmdLine as Cli
import qualified Git.Smoke as Git
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = hspec $ Git.smokeTestSpec Cli.cliFactory

-- Smoke.hs ends here
