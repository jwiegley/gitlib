{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Time
import           Data.Time.Clock.POSIX
import           Git
import           Git.Lens
import qualified Git.Libgit2 as Lg
import           Test.Hspec (describe, it)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = hspec $ describe "Basic Gitlib lens usage" $
    it "passes the sanity test" $ do
        mres <- withNewRepository Lg.lgFactory "modifyCommit.git" $ do
            hello <- createBlobUtf8 "Hello, world!\n"
            tr <- createTree $ putBlob "hello/world.txt" hello
            let x = renderObjOid tr
            liftIO $ x `shouldBe` "c0c848a2737a6a8533a18e6bd4d04266225e0271"

            let sig  = Signature {
                    signatureName  = "John Wiegley"
                  , signatureEmail = "johnw@fpcomplete.com"
                  , signatureWhen  = fakeTime 1348980883 }

            c <- createCommit [] tr sig sig "Sample log message.\n" Nothing

            renderObjOid (commitOid c)
                ^! _oid._commit._tree._blob "hello/world.txt"
        mres `shouldBe` Just "Hello, world!\n"
  where
    fakeTime secs = utcToZonedTime utc (posixSecondsToUTCTime secs)
