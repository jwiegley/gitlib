{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Git.Backend.S3 ( odbS3Backend ) where

import           Aws hiding (credentials)
import           Aws.S3 hiding (bucketName)
import           Bindings.Libgit2.Odb
import           Bindings.Libgit2.OdbBackend
import           Bindings.Libgit2.Oid
import           Bindings.Libgit2.Types
import           Control.Applicative
import           Control.Monad.IO.Class
import           Data.Attempt
import           Data.ByteString as B hiding (putStrLn)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.ByteString.Unsafe
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List hiding (mapM_, peek)
import           Data.Git hiding (getObject)
import           Data.Git.Backend
import           Data.Git.Error
import           Data.Git.Oid
import qualified Data.List as L
import           Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import           Debug.Trace (trace)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.StablePtr
import           Foreign.Storable
import           Network.HTTP.Conduit hiding (Response)
import           Prelude hiding ((.), mapM_)

default (Text)

data OdbS3Backend = OdbS3Backend { odbS3Parent :: C'git_odb_backend
                                 , httpManager :: StablePtr Manager
                                 , bucketName  :: StablePtr Text
                                 , credentials :: StablePtr Credentials }

instance Storable OdbS3Backend where
  sizeOf p = sizeOf (undefined :: C'git_odb_backend) +
             sizeOf (undefined :: StablePtr Manager) +
             sizeOf (undefined :: StablePtr Text) +
             sizeOf (undefined :: StablePtr Credentials)
  alignment p = alignment (odbS3Parent p)
  peek p = do
    v0 <- peekByteOff p 0
    let sizev1 = (sizeOf (undefined :: C'git_odb_backend))
    v1 <- peekByteOff p sizev1
    let sizev2 = sizev1 + (sizeOf (undefined :: StablePtr Manager))
    v2 <- peekByteOff p sizev2
    let sizev3 = sizev2 + (sizeOf (undefined :: StablePtr Text))
    v3 <- peekByteOff p sizev3
    return (OdbS3Backend v0 v1 v2 v3)
  poke p (OdbS3Backend v0 v1 v2 v3) = do
    pokeByteOff p 0 v0
    let sizev1 = (sizeOf (undefined :: C'git_odb_backend))
    pokeByteOff p sizev1 v1
    let sizev2 = sizev1 + (sizeOf (undefined :: StablePtr Manager))
    pokeByteOff p sizev2 v2
    let sizev3 = sizev2 + (sizeOf (undefined :: StablePtr Text))
    pokeByteOff p sizev3 v3
    return ()

testFileS3 :: OdbS3Backend -> Text -> ResourceT IO [Text]
testFileS3 odbs3 filepath = do
  manager <- liftIO $ deRefStablePtr (httpManager odbs3)
  bucket  <- liftIO $ deRefStablePtr (bucketName odbs3)
  creds   <- liftIO $ deRefStablePtr (credentials odbs3)

  let req = (getBucket bucket) {
                gbMaxKeys = Just 1
              , gbPrefix  = Just filepath }
  res <- aws (Configuration Timestamp creds $ defaultLog Error)
            defServiceConfig manager req
  gbr <- readResponseIO res
  return $ L.map objectKey (gbrContents gbr)

getFileS3 :: OdbS3Backend -> Text
             -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFileS3 odbs3 filepath = do
  manager <- liftIO $ deRefStablePtr (httpManager odbs3)
  bucket  <- liftIO $ deRefStablePtr (bucketName odbs3)
  creds   <- liftIO $ deRefStablePtr (credentials odbs3)

  let req = getObject bucket filepath
  res <- aws (Configuration Timestamp creds $ defaultLog Error)
            defServiceConfig manager req

  gor <- readResponseIO res
  return (responseBody (gorResponse gor))

putFileS3 :: OdbS3Backend -> Text -> Source (ResourceT IO) ByteString
              -> ResourceT IO BL.ByteString
putFileS3 odbs3 filepath src = do
  manager <- liftIO $ deRefStablePtr (httpManager odbs3)
  bucket  <- liftIO $ deRefStablePtr (bucketName odbs3)
  creds   <- liftIO $ deRefStablePtr (credentials odbs3)

  lbs <- BL.fromChunks <$> (src $$ consume)

  let req = putObject bucket filepath (RequestBodyLBS lbs)
  res <- aws (Configuration Timestamp creds $ defaultLog Error)
            defServiceConfig manager req

  _ <- readResponseIO res
  return lbs

odbS3BackendReadCallback :: F'git_odb_backend_read_callback
odbS3BackendReadCallback data_p len_p type_p be oid = do
  oidStr <- oidToStr oid
  code   <- odbS3BackendReadHeaderCallback len_p type_p be oid
  case code of
    0 -> do
      odbs3  <- peek (castPtr be :: Ptr OdbS3Backend)
      result <- runResourceT $ getFileS3 odbs3 (T.pack oidStr)
      bytes  <- runResourceT $ result $$+- await
      case bytes of
        Nothing -> return (-1)
        Just bs -> do
          let len = B.length bs
          content <- mallocBytes (len + 1)
          unsafeUseAsCString bs $ \cstr ->
            copyBytes content cstr (len + 1)
          poke data_p (castPtr content)
          return 0
    n -> return n

odbS3BackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
odbS3BackendReadPrefixCallback out_oid oid_p len_p type_p be oid len = do
  oidStr <- oidToStr oid
  return (-1)

odbS3BackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
odbS3BackendReadHeaderCallback len_p type_p be oid = do
  oidStr <- oidToStr oid
  odbs3  <- peek (castPtr be :: Ptr OdbS3Backend)
  result <- runResourceT $ getFileS3 odbs3 (T.pack (oidStr ++ ".meta"))
  bytes  <- runResourceT $ result $$+- await
  case bytes of
    Nothing -> return (-1)
    Just bs -> do let (len,typ) = read (BC.unpack bs) :: (Int,Int)
                  poke len_p (fromIntegral len)
                  poke type_p (fromIntegral typ)
                  return 0

odbS3BackendWriteCallback :: F'git_odb_backend_write_callback
odbS3BackendWriteCallback oid be obj_data len obj_type = do
  r <- c'git_odb_hash oid obj_data len obj_type
  case r of
    0 -> do
      oidStr <- oidToStr oid
      odbs3  <- peek (castPtr be :: Ptr OdbS3Backend)

      let metadata = BLC.pack (show ((fromIntegral len,
                                      fromIntegral obj_type) :: (Int,Int)))
      runResourceT $ putFileS3 odbs3 (T.pack (oidStr ++ ".meta"))
                               (sourceLbs metadata)

      bytes <- curry unsafePackCStringLen (castPtr obj_data)
                                         (fromIntegral len)
      runResourceT $ putFileS3 odbs3 (T.pack oidStr)
                               (sourceLbs (BL.fromChunks [bytes]))
      return 0
    n -> return n

odbS3BackendExistsCallback :: F'git_odb_backend_exists_callback
odbS3BackendExistsCallback be oid = do
  oidStr <- oidToStr oid
  odbs3   <- peek (castPtr be :: Ptr OdbS3Backend)
  entries <- runResourceT $ testFileS3 odbs3 (T.pack oidStr)
  if L.length entries == 1
    then return 0
    else return (-1)

odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
odbS3BackendFreeCallback be = do
  backend <- peek be
  freeHaskellFunPtr (c'git_odb_backend'read backend)
  freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
  freeHaskellFunPtr (c'git_odb_backend'read_header backend)
  freeHaskellFunPtr (c'git_odb_backend'write backend)
  freeHaskellFunPtr (c'git_odb_backend'exists backend)

  odbs3 <- peek (castPtr be :: Ptr OdbS3Backend)
  freeStablePtr (httpManager odbs3)
  freeStablePtr (bucketName odbs3)
  freeStablePtr (credentials odbs3)

foreign export ccall "odbS3BackendFreeCallback"
  odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
foreign import ccall "&odbS3BackendFreeCallback"
  odbS3BackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

odbS3Backend :: Manager -> Text -> Text -> Text -> IO (Ptr C'git_odb_backend)
odbS3Backend manager bucket access secret = do
  readFun       <- mk'git_odb_backend_read_callback odbS3BackendReadCallback
  readPrefixFun <-
    mk'git_odb_backend_read_prefix_callback odbS3BackendReadPrefixCallback
  readHeaderFun <-
    mk'git_odb_backend_read_header_callback odbS3BackendReadHeaderCallback
  writeFun      <- mk'git_odb_backend_write_callback odbS3BackendWriteCallback
  existsFun     <- mk'git_odb_backend_exists_callback odbS3BackendExistsCallback

  manager'      <- newStablePtr manager
  bucket'       <- newStablePtr bucket
  credentials'  <- newStablePtr (Credentials {
                                      accessKeyID      = E.encodeUtf8 access
                                    , secretAccessKey = E.encodeUtf8 secret })

  castPtr <$> new OdbS3Backend {
    odbS3Parent = C'git_odb_backend {
         c'git_odb_backend'odb         = nullPtr
       , c'git_odb_backend'read        = readFun
       , c'git_odb_backend'read_prefix = readPrefixFun
       , c'git_odb_backend'readstream  = nullFunPtr
       , c'git_odb_backend'read_header = readHeaderFun
       , c'git_odb_backend'write       = writeFun
       , c'git_odb_backend'writestream = nullFunPtr
       , c'git_odb_backend'exists      = existsFun
       , c'git_odb_backend'free        = odbS3BackendFreeCallbackPtr }
    , httpManager = manager'
    , bucketName  = bucket'
    , credentials = credentials' }

-- S3.hs
