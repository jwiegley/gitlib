{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Control.Monad
import qualified Git.Smoke as Git
import qualified Git.Sample as Samp
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.Runner
import           Test.Hspec.HUnit ()

main :: IO ()
main = do
    summary <- hspecWith
               (defaultConfig { configVerbose = True })
               (Git.smokeTestSpec
                (\path _ act -> Samp.withSampleRepository path True True act))
    when (summaryFailures summary > 0) $ exitFailure
    return ()

-- Smoke.hs ends here
