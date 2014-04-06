{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import qualified Git.Hit as Hit
import qualified Git.Smoke as GS
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner


main :: IO ()
main = hspec $ GS.smokeTestSpec Hit.hitFactory Hit.hitFactory

-- Smoke.hs ends here
