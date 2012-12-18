{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Aws
import           Aws.S3
import           Control.Applicative
import           Data.ByteString
import qualified Data.ByteString.Lazy as BL
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List hiding (mapM_)
import           Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import           Network.HTTP.Conduit

default (Text)

getFile :: Manager -> Text -> Text -> Text -> Text
           -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFile manager bucket access secret filepath = do
  let req    = getObject bucket filepath
      creds  = Credentials { accessKeyID     = E.encodeUtf8 access
                           , secretAccessKey = E.encodeUtf8 secret }
  res <- aws (Configuration Timestamp creds $ defaultLog Error)
            defServiceConfig manager req
  gor <- readResponseIO res
  return (responseBody (gorResponse gor))

putFile :: Manager -> Text -> Text -> Text -> Text
           -> Source (ResourceT IO) ByteString
           -> ResourceT IO BL.ByteString
putFile manager bucket access secret filepath src = do
  lbs <- BL.fromChunks <$> (src $$ consume)
  let req   = putObject bucket filepath (RequestBodyLBS lbs)
      creds = Credentials { accessKeyID     = E.encodeUtf8 access
                          , secretAccessKey = E.encodeUtf8 secret }
  res <- aws (Configuration Timestamp creds $ defaultLog Error)
            defServiceConfig manager req
  _ <- readResponseIO res
  return lbs

main :: IO ()
main = do
  let access = "AKIAJT6ZIAY5FKAGVTOA"
      secret = "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"

  manager  <- newManager def

  object   <- runResourceT $
              getFile manager "fpco-john-development"
                      access secret "test.txt"
  contents <- runResourceT $ object $$+- await
  print contents

  _ <- runResourceT $
       putFile manager "fpco-john-development"
               access secret "test2.txt"
               (sourceLbs (LE.encodeUtf8 "Hello, world!\n"))

  object2   <- runResourceT $
               getFile manager "fpco-john-development"
                       access secret "test2.txt"
  contents2 <- runResourceT $ object2 $$+- await
  print contents2

  -- This is the hS3 version:

  -- let conn = amazonS3Connection "AKIAJT6ZIAY5FKAGVTOA"
  --                               "kOWkdTeHg4Evl+wv55i7Py8g9e1Dw7fKpl2CFjI+"
  -- buckets <- listBuckets conn
  -- case buckets of
  --   Left err -> putStrLn (prettyReqError err)
  --   Right bs -> mapM_ print bs

  -- objects <- listObjects conn "fpco-john-development"
  --                        (ListRequest "" "" "" 100)
  -- case objects of
  --   Left err      -> putStrLn (prettyReqError err)
  --   Right (_, os) -> mapM_ (print . key) os

-- Main.hs ends here
