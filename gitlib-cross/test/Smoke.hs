{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import qualified Git.CmdLine as Cli
import qualified Git.Libgit2 as Lg
import qualified Git.Smoke as Git
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = hspec $ Git.smokeTestSpec Cli.cliFactory Lg.lgFactory

-- Smoke.hs ends here
