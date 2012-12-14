module Main where

import Network.AWS.AWSConnection
import Network.AWS.AWSResult
import Network.AWS.S3Bucket

main :: IO ()
main = do
  let conn = amazonS3Connection "AKIAJT6ZIAY5FKAGVTOA"
                                "kOWkdTeHg4Evl+wv55i7Py8g9e1Dw7fKpl2CFjI+"
  buckets <- listBuckets conn
  case buckets of
    Left err -> putStrLn (prettyReqError err)
    Right bs -> mapM_ print bs

  objects <- listObjects conn "fpco-john-development"
                         (ListRequest "" "" "" 100)
  case objects of
    Left err      -> putStrLn (prettyReqError err)
    Right (_, os) -> mapM_ (print . key) os

-- Main.hs ends here
