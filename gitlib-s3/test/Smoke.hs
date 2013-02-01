{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Main where

import           Aws
import           Aws.Core
import           Aws.S3 hiding (bucketName)
import           Control.Applicative
import           Control.Monad
import           Data.Text as T
import qualified Git.Libgit2 as Lg
import qualified Git.S3 as Git
import qualified Git.Smoke as Git
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

main :: IO ()
main = do
    bucket    <- T.pack <$> getEnv "S3_BUCKET"
    accessKey <- T.pack <$> getEnv "AWS_ACCESS_KEY"
    secretKey <- T.pack <$> getEnv "AWS_SECRET_KEY"
    summary   <-
        hspecWith
        (defaultConfig { configVerbose = True })
        (Git.smokeTestSpec
         (\path _ act ->
           Lg.withLgRepository path True
           (Git.addS3Backend bucket "" accessKey secretKey
                             Nothing (Just "127.0.0.1") Error
            >> act)))
    when (summaryFailures summary > 0) $ exitFailure
    return ()

-- Smoke.hs ends here
