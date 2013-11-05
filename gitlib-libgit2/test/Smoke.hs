module Main where

import qualified Git.Libgit2 as Lg
import qualified Git.Smoke as Git
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = hspec $ Git.smokeTestSpec Lg.lgFactory Lg.lgFactory

-- Smoke.hs ends here
