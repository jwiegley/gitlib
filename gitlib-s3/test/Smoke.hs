{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
import qualified Git as Git
import qualified Git.Libgit2 as Lg
import qualified Git.S3 as S3
import qualified Git.Smoke as Git
import           System.Environment
import           System.Exit
import           Test.HUnit
import           Test.Hspec (Spec, describe, it, hspec)
import           Test.Hspec.Expectations
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

s3Factory :: Git.MonadGit m
          => Git.RepositoryFactory Lg.LgRepository m Lg.Repository
s3Factory = Lg.lgFactory
    { Git.runRepository = \ctxt -> Lg.runLgRepository ctxt . (s3back >>) }
  where
    s3back = do
        env <- liftIO $ getEnvironment
        let bucket    = T.pack <$> lookup "S3_BUCKET" env
            accessKey = T.pack <$> lookup "AWS_ACCESS_KEY" env
            secretKey = T.pack <$> lookup "AWS_SECRET_KEY" env
        repo <- Lg.lgGet
        void $ liftIO $ S3.addS3Backend
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

main :: IO ()
main = hspec $ Git.smokeTestSpec s3Factory s3Factory

-- Smoke.hs ends here
