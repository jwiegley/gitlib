{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Monad
import qualified Git as Git
import qualified Git.Smoke as Git
import qualified Git.Libgit2 as Lg
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()

type LgRepoFactoryIO =
    Git.RepositoryFactory (Lg.LgRepository IO) IO Lg.Repository

main :: IO ()
main = hspec $ Git.smokeTestSpec Lg.lgFactory Lg.lgFactory

-- Smoke.hs ends here
