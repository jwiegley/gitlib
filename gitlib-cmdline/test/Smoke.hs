{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import qualified Git.CmdLine as Cli
import qualified Git.Smoke as Git
import           Test.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = hspec $ Git.smokeTestSpec Cli.cliFactory Cli.cliFactory

-- Smoke.hs ends here
