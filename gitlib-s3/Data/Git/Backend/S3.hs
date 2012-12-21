{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Backend.S3
       ( odbS3Backend
       , createS3backend
       , readRefs, writeRefs
       , mirrorRefsFromS3, mirrorRefsToS3 )
       where

import           Aws
import           Aws.Core
import           Aws.S3 hiding (bucketName)
import           Bindings.Libgit2.Odb
import           Bindings.Libgit2.OdbBackend
import           Bindings.Libgit2.Oid
import           Bindings.Libgit2.Refs
import           Bindings.Libgit2.Types
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attempt
import           Data.Binary
import           Data.ByteString as B hiding (putStrLn)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.ByteString.Unsafe
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List hiding (mapM_, peek, catMaybes, sequence)
import           Data.Git hiding (getObject)
import           Data.Git.Backend
import           Data.Git.Backend.Trace
import           Data.Git.Error
import           Data.Git.Oid
import qualified Data.List as L
import           Data.Map
import           Data.Maybe
import           Data.Stringable
import           Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy.Encoding as LE
import qualified Data.Yaml as Y
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           Network.HTTP.Conduit hiding (Response)
import           Prelude hiding (mapM_, catch)
import           System.IO.Unsafe

default (Text)

data OdbS3Backend =
  OdbS3Backend { odbS3Parent     :: C'git_odb_backend
               , httpManager     :: StablePtr Manager
               , bucketName      :: StablePtr Text
               , objectPrefix    :: StablePtr Text
               , configuration   :: StablePtr Configuration
               , s3configuration :: StablePtr (S3Configuration NormalQuery) }

instance Storable OdbS3Backend where
  sizeOf _ = sizeOf (undefined :: C'git_odb_backend) +
             sizeOf (undefined :: StablePtr Manager) +
             sizeOf (undefined :: StablePtr Text) +
             sizeOf (undefined :: StablePtr Text) +
             sizeOf (undefined :: StablePtr Configuration) +
             sizeOf (undefined :: StablePtr (S3Configuration NormalQuery))
  alignment _ = alignment (undefined :: Ptr C'git_odb_backend)
  peek p = do
    v0 <- peekByteOff p 0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    v1 <- peekByteOff p sizev1
    let sizev2 = sizev1 + sizeOf (undefined :: StablePtr Manager)
    v2 <- peekByteOff p sizev2
    let sizev3 = sizev2 + sizeOf (undefined :: StablePtr Text)
    v3 <- peekByteOff p sizev3
    let sizev4 = sizev3 + sizeOf (undefined :: StablePtr Text)
    v4 <- peekByteOff p sizev4
    let sizev5 = sizev4 + sizeOf (undefined :: StablePtr Configuration)
    v5 <- peekByteOff p sizev5
    return (OdbS3Backend v0 v1 v2 v3 v4 v5)
  poke p (OdbS3Backend v0 v1 v2 v3 v4 v5) = do
    pokeByteOff p 0 v0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    pokeByteOff p sizev1 v1
    let sizev2 = sizev1 + sizeOf (undefined :: StablePtr Manager)
    pokeByteOff p sizev2 v2
    let sizev3 = sizev2 + sizeOf (undefined :: StablePtr Text)
    pokeByteOff p sizev3 v3
    let sizev4 = sizev3 + sizeOf (undefined :: StablePtr Text)
    pokeByteOff p sizev4 v4
    let sizev5 = sizev4 + sizeOf (undefined :: StablePtr Configuration)
    pokeByteOff p sizev5 v5
    return ()

odbS3dispatch ::
  MonadIO m =>
    (Manager -> Text -> Text
       -> Configuration -> S3Configuration NormalQuery -> a -> m b)
      -> OdbS3Backend -> a -> m b
odbS3dispatch f odbS3 arg = do
  manager  <- liftIO $ deRefStablePtr (httpManager odbS3)
  bucket   <- liftIO $ deRefStablePtr (bucketName odbS3)
  prefix   <- liftIO $ deRefStablePtr (objectPrefix odbS3)
  config   <- liftIO $ deRefStablePtr (configuration odbS3)
  s3config <- liftIO $ deRefStablePtr (s3configuration odbS3)
  f manager bucket prefix config s3config arg

testFileS3' :: Manager -> Text -> Text
               -> Configuration -> S3Configuration NormalQuery
               -> Text
               -> ResourceT IO Bool
testFileS3' manager bucket prefix config s3config filepath =
  isJust . readResponse <$>
    aws config s3config manager
        (headObject bucket (T.append prefix filepath))

testFileS3 :: OdbS3Backend -> Text -> ResourceT IO Bool
testFileS3 = odbS3dispatch testFileS3'

getFileS3' :: Manager -> Text -> Text
              -> Configuration -> S3Configuration NormalQuery
              -> (Text, Maybe (Int,Int))
              -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFileS3' manager bucket prefix config s3config (filepath,range) = do
  res <- aws config s3config manager
             (getObject bucket (T.append prefix filepath))
               { goResponseContentRange = range }
  gor <- readResponseIO res
  return (responseBody (gorResponse gor))

getFileS3 :: OdbS3Backend -> Text -> Maybe (Int,Int)
             -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFileS3 = curry . odbS3dispatch getFileS3'

putFileS3' :: Manager -> Text -> Text
              -> Configuration -> S3Configuration NormalQuery
              -> (Text, Source (ResourceT IO) ByteString)
              -> ResourceT IO BL.ByteString
putFileS3' manager bucket prefix config s3config (filepath,src) = do
  lbs <- BL.fromChunks <$> (src $$ consume)
  res <- aws config s3config manager
             (putObject bucket (T.append prefix filepath)
                        (RequestBodyLBS lbs))
  _ <- readResponseIO res
  return lbs

putFileS3 :: OdbS3Backend -> Text -> Source (ResourceT IO) ByteString
             -> ResourceT IO BL.ByteString
putFileS3 = curry . odbS3dispatch putFileS3'

type RefMap = Map Text (Either Text Oid)

instance Y.FromJSON Oid where
  parseJSON (Y.String v) =
    return . Oid . COid $ unsafePerformIO $ do
      ptr <- mallocForeignPtr
      withCStringable v $ \cstr ->
        withForeignPtr ptr $ \ptr' -> do
          r <- c'git_oid_fromstr ptr' cstr
          when (r < 0) $ throwIO OidCopyFailed
          return ptr

coidToJSON :: ForeignPtr C'git_oid -> Y.Value
coidToJSON coid = unsafePerformIO $ withForeignPtr coid $ \oid ->
                    Y.toJSON <$> oidToStr oid

instance Y.ToJSON Oid where
  toJSON (PartialOid (COid coid) _) = throw RefCannotCreateFromPartialOid
  toJSON (Oid (COid coid))          = coidToJSON coid

readRefs :: Ptr C'git_odb_backend -> IO (Maybe RefMap)
readRefs be = do
  odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
  exists <- catch (runResourceT $ testFileS3 odbS3 "refs.yml")
                  (\e -> print (e :: IOException) >> throwIO e)
  if exists
    then do
    bytes  <- catch (runResourceT $ do
                        result <- getFileS3 odbS3 "refs.yml" Nothing
                        result $$+- await)
                    (\e -> print (e :: IOException) >> throwIO e)
    case bytes of
      Nothing     -> return Nothing
      Just bytes' -> return (Y.decode bytes' :: Maybe RefMap)

    else return Nothing

writeRefs :: Ptr C'git_odb_backend -> RefMap -> IO ()
writeRefs be refs = do
  let payload = Y.encode refs
  odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
  void $ runResourceT $
    putFileS3 odbS3 "refs.yml" (sourceLbs (BL.fromChunks [payload]))

mirrorRefsFromS3 :: Ptr C'git_odb_backend -> Repository -> IO ()
mirrorRefsFromS3 be repo = do
  refs <- readRefs be
  case refs of
    Nothing -> return ()
    Just refs' ->
      forM_ (toList refs') $ \(name, ref) ->
        withForeignPtr (repositoryPtr repo) $ \repoPtr ->
          withCStringable name $ \namePtr -> alloca $ \ptr -> do
            r <- case ref of
                  Left target ->
                    withCStringable target $ \targetPtr ->
                      c'git_reference_create_symbolic ptr repoPtr namePtr
                                                      targetPtr 1
                  Right (PartialOid {}) ->
                    throwIO RefCannotCreateFromPartialOid
                  Right (Oid (COid coid)) ->
                    withForeignPtr coid $ \coidPtr ->
                      c'git_reference_create_oid ptr repoPtr namePtr coidPtr 1
            when (r < 0) $ throwIO RepositoryInvalid

mirrorRefsToS3 :: Ptr C'git_odb_backend -> Repository -> IO ()
mirrorRefsToS3 be repo = do
  odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
  refs  <- catMaybes <$>
           mapAllRefs repo (\name -> do ref <- lookupRef name repo
                                        return (go name <$> ref))
  writeRefs be (fromList refs)
  where go name ref =
          case refTarget ref of
            RefTargetSymbolic target -> (name, Left target)
            RefTargetId oid          -> (name, Right oid)

odbS3BackendReadCallback :: F'git_odb_backend_read_callback
odbS3BackendReadCallback data_p len_p type_p be oid =
  catch go (\e -> print (e :: IOException) >> return (-1))
  where
    go = do
      odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
      oidStr <- oidToStr oid
      bytes  <- runResourceT $ do
        result <- getFileS3 odbS3 (T.pack oidStr) Nothing
        result $$+- await
      case bytes of
        Nothing -> return (-1)
        Just bs -> do
          let blen      = B.length bs
              (len,typ) = decode (BL.fromChunks [bs]) :: (Int,Int)
              hdrLen    = sizeOf (undefined :: Int) * 2
          content <- mallocBytes (len + 1)
          unsafeUseAsCString bs $ \cstr ->
            copyBytes content (cstr `plusPtr` hdrLen) (len + 1)
          poke len_p (fromIntegral len)
          poke type_p (fromIntegral typ)
          poke data_p (castPtr content)
          return 0

odbS3BackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
odbS3BackendReadPrefixCallback out_oid oid_p len_p type_p be oid len =
  return 0

odbS3BackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
odbS3BackendReadHeaderCallback len_p type_p be oid =
  catch go (\e -> print (e :: IOException) >> return (-1))
  where
    go = do
      let hdrLen = sizeOf (undefined :: Int) * 2
      odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
      oidStr <- oidToStr oid
      bytes  <- runResourceT $ do
        result <- getFileS3 odbS3 (T.pack oidStr) (Just (0,hdrLen - 1))
        result $$+- await
      case bytes of
        Nothing -> return (-1)
        Just bs -> do
          let (len,typ) = decode (BL.fromChunks [bs]) :: (Int,Int)
          poke len_p (fromIntegral len)
          poke type_p (fromIntegral typ)
          return 0

odbS3BackendWriteCallback :: F'git_odb_backend_write_callback
odbS3BackendWriteCallback oid be obj_data len obj_type = do
  r <- c'git_odb_hash oid obj_data len obj_type
  case r of
    0 -> do
      oidStr <- oidToStr oid
      odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
      let hdr = encode ((fromIntegral len, fromIntegral obj_type) :: (Int,Int))
      bytes <- curry unsafePackCStringLen (castPtr obj_data) (fromIntegral len)
      let payload = BL.append hdr (BL.fromChunks [bytes])
      catch (go odbS3 oidStr payload >> return 0)
            (\e -> print (e :: IOException) >> return (-1))
    n -> return n
  where
    go odbS3 oidStr payload =
      runResourceT $ putFileS3 odbS3 (T.pack oidStr) (sourceLbs payload)

odbS3BackendExistsCallback :: F'git_odb_backend_exists_callback
odbS3BackendExistsCallback be oid = do
  oidStr <- oidToStr oid
  odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
  exists <- catch (runResourceT $ testFileS3 odbS3 (T.pack oidStr))
                 (\e -> print (e :: IOException) >> return False)
  return $ if exists then 1 else 0

odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
odbS3BackendFreeCallback be = do
  backend <- peek be
  freeHaskellFunPtr (c'git_odb_backend'read backend)
  freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
  freeHaskellFunPtr (c'git_odb_backend'read_header backend)
  freeHaskellFunPtr (c'git_odb_backend'write backend)
  freeHaskellFunPtr (c'git_odb_backend'exists backend)

  odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
  freeStablePtr (httpManager odbS3)
  freeStablePtr (bucketName odbS3)
  freeStablePtr (objectPrefix odbS3)
  freeStablePtr (configuration odbS3)
  freeStablePtr (s3configuration odbS3)

foreign export ccall "odbS3BackendFreeCallback"
  odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
foreign import ccall "&odbS3BackendFreeCallback"
  odbS3BackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

odbS3Backend :: S3Configuration NormalQuery
                -> Configuration
                -> Manager -> Text -> Text
                -> IO (Ptr C'git_odb_backend)
odbS3Backend s3config config manager bucket prefix = do
  readFun       <- mk'git_odb_backend_read_callback odbS3BackendReadCallback
  readPrefixFun <-
    mk'git_odb_backend_read_prefix_callback odbS3BackendReadPrefixCallback
  readHeaderFun <-
    mk'git_odb_backend_read_header_callback odbS3BackendReadHeaderCallback
  writeFun      <- mk'git_odb_backend_write_callback odbS3BackendWriteCallback
  existsFun     <- mk'git_odb_backend_exists_callback odbS3BackendExistsCallback

  manager'  <- newStablePtr manager
  bucket'   <- newStablePtr bucket
  prefix'   <- newStablePtr prefix
  s3config' <- newStablePtr s3config
  config'   <- newStablePtr config

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
    , httpManager     = manager'
    , bucketName      = bucket'
    , objectPrefix    = prefix'
    , configuration   = config'
    , s3configuration = s3config' }

createS3backend :: Text -> Text -> Text -> Maybe Text -> LogLevel -> Bool
                   -> Repository -> IO Repository
createS3backend bucket access secret mockAddr level tracing repo = do
    manager <- newManager def
    odbS3 <-
      odbS3Backend
        (case mockAddr of
            Nothing   -> defServiceConfig
            Just addr -> (s3 HTTP (E.encodeUtf8 addr) False) {
                               s3Port         = 10001
                             , s3RequestStyle = PathStyle })
        (Configuration Timestamp Credentials {
              accessKeyID     = E.encodeUtf8 access
            , secretAccessKey = E.encodeUtf8 secret }
         (defaultLog level))
        manager bucket ""

    if tracing
      then do backend <- traceBackend odbS3
              odbBackendAdd repo backend 100
      else odbBackendAdd repo odbS3 100

    -- Start by reading in the known refs from S3
    mirrorRefsFromS3 odbS3 repo

    -- Whenever a ref is written, update the refs in S3
    let onWrite = const $ mirrorRefsToS3 odbS3 repo
    return repo { repoOnWriteRef = onWrite : repoOnWriteRef repo }

-- S3.hs
