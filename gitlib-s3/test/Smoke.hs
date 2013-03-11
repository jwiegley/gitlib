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
import           Control.Monad.IO.Class
import           Data.Maybe (fromMaybe, isNothing)
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
    env <- getEnvironment
    let bucket    = T.pack <$> lookup "S3_BUCKET" env
        accessKey = T.pack <$> lookup "AWS_ACCESS_KEY" env
        secretKey = T.pack <$> lookup "AWS_SECRET_KEY" env
    summary   <-
        hspecWith
        (defaultConfig { configVerbose = True })
        (Git.smokeTestSpec
         (\path _ act ->
           Lg.withLgRepository path True True $ do
               repo <- Lg.lgGet
               liftIO $ Git.addS3Backend
                   repo
                   (fromMaybe "test-bucket" bucket)
                   ""
                   (fromMaybe "" accessKey)
                   (fromMaybe "" secretKey)
                   Nothing
                   (if isNothing bucket
                    then Just "127.0.0.1"
                    else Nothing)
                   Error
               act))
    when (summaryFailures summary > 0) $ exitFailure
    return ()

-- Smoke.hs ends here
