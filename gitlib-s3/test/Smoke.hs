{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# OPTIONS_GHC -fno-warn-wrong-do-bind #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Aws
import           Control.Applicative
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.Resource
import           Data.Default (def)
import           Data.Maybe (fromMaybe, isNothing)
import           Data.Text as T
import qualified Git
import qualified Git.Libgit2 as Lg
import qualified Git.S3 as S3
import qualified Git.Smoke as Git
import           System.Directory
import           System.Environment
import           System.FilePath.Posix
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner

s3Factory :: (MonadThrow m, MonadIO m, MonadBaseControl IO m)
          => Git.RepositoryFactory (ReaderT Lg.LgRepo (NoLoggingT m)) m Lg.LgRepo
s3Factory = Lg.lgFactory
    { Git.runRepository = \ctxt m ->
       runNoLoggingT $ Lg.runLgRepository ctxt (s3back >> m) }
  where
    s3back = do
        repo <- Git.getRepository
        env <- liftIO getEnvironment
        let bucket    = T.pack <$> lookup "S3_BUCKET" env
            accessKey = T.pack <$> lookup "AWS_ACCESS_KEY" env
            secretKey = T.pack <$> lookup "AWS_SECRET_KEY" env
        cwd <- liftIO getCurrentDirectory
        svc <- liftIO S3.s3MockService
        let tmpDir = cwd </> "s3cache"
        liftIO $ createDirectoryIfMissing True tmpDir
        S3.addS3Backend
            repo
            (fromMaybe "test-bucket" bucket)
            ""
            (fromMaybe "" accessKey)
            (fromMaybe "" secretKey)
            Nothing
            (if isNothing bucket then Just "127.0.0.1" else Nothing)
            Error
            tmpDir
            def { -- S3.registerObject = \sha _ -> do
                --        putStrLn $ "registerObject: " ++ show sha
                --        modifyMVar_ objectMap
                --            (return . Map.insert sha S3.ObjectLoose)
                -- , S3.registerPackFile = \packBase shas -> do
                --        putStrLn $ "registerPackFile: " ++ show packBase
                --        modifyMVar_ objectMap
                --            (\m -> return $ foldr
                --                   (flip Map.insert
                --                    (S3.ObjectInPack packBase)) m shas)
                -- , S3.lookupObject = \sha -> do
                --        putStrLn $ "lookupObject: " ++ show sha
                --        Map.lookup sha <$> readMVar objectMap
                  S3.headObject = \bucket path ->
                   S3.mockHeadObject svc bucket path
                , S3.getObject  = \bucket path range ->
                   S3.mockGetObject svc bucket path range
                , S3.putObject  = \bucket path len bytes ->
                   S3.mockPutObject svc bucket path
                       (fromIntegral (S3.getObjectLength len)) bytes
                }

main :: IO ()
main = hspec $ Git.smokeTestSpec s3Factory s3Factory

-- Smoke.hs ends here
