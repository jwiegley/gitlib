{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import qualified Git.Hit as Hit
import qualified Git.Smoke as GS
import           Test.Hspec.Runner (hspec)
import           Test.HUnit.Base (assertEqual)
import           Data.Git.Types (toUTCTime)
import           Data.List (intercalate)
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime)
import           Data.Time.LocalTime (getZonedTime, zonedTimeToUTC)
import           System.Locale (defaultTimeLocale)

fmt :: UTCTime -> String
fmt = formatTime defaultTimeLocale "%x %X"

testHitTime :: IO ()
testHitTime = do
    now <- getZonedTime
    let gt = Hit.hitTime now
    let u1 = fmt $ zonedTimeToUTC now
    let u2 = fmt $ toUTCTime gt
    putStrLn $ intercalate "\n" [show now, show gt, u1]
    assertEqual "" u1 u2

main :: IO ()
main = do
    testHitTime
    hspec $ GS.smokeTestSpec Hit.hitFactory Hit.hitFactory

-- Smoke.hs ends here
