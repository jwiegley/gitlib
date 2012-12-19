{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Aws
import           Aws.Core
import           Aws.S3
import           Control.Applicative
import           Data.ByteString
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List hiding (mapM_)
import           Data.Maybe (isJust)
import           Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import           Network.HTTP.Conduit

default (Text)

bucket = "fpco-john-development"
access = "AKIAJT6ZIAY5FKAGVTOA"
secret = "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"
creds  = Credentials { accessKeyID     = E.encodeUtf8 access
                     , secretAccessKey = E.encodeUtf8 secret }
config = Configuration Timestamp creds $ defaultLog Debug

-- svcConfig = defServiceConfig
svcConfig = ((s3 HTTP "127.0.0.1" False) {
                  s3Port = 10001
                , s3RequestStyle = PathStyle })

headFile :: Manager -> Text -> ResourceT IO Bool
headFile manager filepath =
  isJust . readResponse <$> aws config svcConfig manager
                                (headObject bucket filepath)

getFile :: Manager -> Text
           -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFile manager filepath = do
  let req = getObject bucket filepath
  res <- aws config svcConfig manager req
  gor <- readResponseIO res
  return (responseBody (gorResponse gor))

putFile :: Manager -> Text -> Source (ResourceT IO) ByteString
           -> ResourceT IO BL.ByteString
putFile manager filepath src = do
  lbs <- BL.fromChunks <$> (src $$ consume)
  let req = putObject bucket filepath (RequestBodyLBS lbs)
  res <- aws config svcConfig manager req
  _ <- readResponseIO res
  return lbs

main :: IO ()
main = do
  manager <- newManager def
  _ <- runResourceT $
       putFile manager "test2.txt"
               (sourceLbs (LE.encodeUtf8 "Hello, world!\n"))

  exists1 <- runResourceT $ headFile manager "test99.txt"
  Prelude.putStrLn $ "exists1: " ++ show exists1

  exists2 <- runResourceT $ headFile manager "test2.txt"
  Prelude.putStrLn $ "exists2: " ++ show exists2

  contents2 <- runResourceT $ do
    object2 <- getFile manager "test2.txt"
    object2 $$+- await
  Prelude.putStrLn $ "read results: " ++ show contents2

-- Main.hs ends here
