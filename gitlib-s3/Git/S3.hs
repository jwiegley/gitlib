{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing
                -fno-warn-unused-binds
                -fno-warn-orphans #-}

module Git.S3
       ( s3Factory, odbS3Backend, addS3Backend
       , ObjectStatus(..), BackendCallbacks(..)
       , ObjectType(..), ObjectLength(..), QuotaStatus(..)
       , S3MockService(), s3MockService
       , mockGetBucket, mockHeadObject, mockGetObject, mockPutObject
       -- , readRefs, writeRefs
       -- , mirrorRefsFromS3, mirrorRefsToS3
       ) where

import           Aws
import           Aws.Core
import qualified Aws.S3 as Aws
import           Bindings.Libgit2.Errors
import           Bindings.Libgit2.Odb
import           Bindings.Libgit2.OdbBackend
import           Bindings.Libgit2.Oid
import           Bindings.Libgit2.Refs
import           Bindings.Libgit2.Types
import           Control.Applicative
import           Control.Concurrent.STM hiding (orElse)
import           Control.Exception.Lifted
import           Control.Lens ((??))
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Aeson as A
import           Data.Attempt
import           Data.Bifunctor
import           Data.Binary as Bin
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import           Data.Conduit
import           Data.Conduit.Binary hiding (drop)
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Foldable (for_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.IORef
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time.Clock
import           Data.Traversable (for)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.StablePtr
import           Foreign.Storable
import           GHC.Generics
import qualified Git
import           Git (SHA(..), shaToText)
import           Git.Libgit2
import           Git.Libgit2.Backend
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Network.HTTP.Conduit hiding (Response)
import           Prelude hiding (mapM_, catch)
import           System.Directory
import           System.FilePath.Posix
import           System.IO.Unsafe

debug :: MonadIO m => String -> m ()
--debug = liftIO . putStrLn
debug = const (return ())

newtype ObjectLength = ObjectLength { getObjectLength :: Int64 }
                     deriving (Eq, Show, Generic)
newtype ObjectType   = ObjectType { getObjectType :: Int }
                     deriving (Eq, Show, Generic)

instance Bin.Binary ObjectLength where
    put (ObjectLength x) = Bin.put x
    get = ObjectLength <$> Bin.get
instance Bin.Binary ObjectType where
    put (ObjectType x) = Bin.put x
    get = ObjectType <$> Bin.get

plainFile :: ObjectType
plainFile = ObjectType 0

data ObjectInfo = ObjectInfo
      { infoLength :: ObjectLength
      , infoType   :: ObjectType
      , infoPath   :: Maybe FilePath
      , infoData   :: Maybe ByteString
      } deriving Eq

instance Show ObjectInfo where
    show ObjectInfo {..} = "ObjectInfo {"
                        ++ "infoLength = " ++ show infoLength
                        ++ ", infoType = " ++ show infoType
                        ++ ", infoPath = " ++ show infoPath
                        ++ ", infoData = " ++ show (isJust infoData)
                        ++ "}"

fromSha :: SHA -> FilePath
fromSha = T.unpack . shaToText

data ObjectStatus = ObjectLoose
                  | ObjectLooseMetaKnown ObjectLength ObjectType
                  | ObjectInPack Text
                  deriving (Eq, Show, Generic)

instance A.ToJSON ObjectLength; instance A.FromJSON ObjectLength
instance A.ToJSON ObjectType;   instance A.FromJSON ObjectType
instance A.ToJSON ObjectStatus; instance A.FromJSON ObjectStatus

data QuotaStatus = QuotaCheckSuccess
                 | QuotaSoftLimitExceeded
                   { quotaStatusAmount :: Int64
                   , quotaStatusLimit  :: Int64
                   }
                 | QuotaHardLimitExceeded
                   { quotaStatusAmount :: Int64
                   , quotaStatusLimit  :: Int64
                   }
                  deriving (Eq, Show, Generic)

data BackendCallbacks = BackendCallbacks
    { checkQuota :: ObjectLength -> IO (Maybe QuotaStatus)
      -- 'checkQuota' gives the backend a chance to reject the upload of
      -- objects that may exceed per-user quotas.

    , registerObject :: SHA -> Maybe (ObjectLength, ObjectType) -> IO ()
      -- 'registerObject' reports that a SHA has been written as a loose
      -- object to the S3 repository.  The for tracking it is that sometimes
      -- calling 'locateObject' can be much faster than querying Amazon.

    , registerPackFile :: Text -> [SHA] -> IO ()
      -- 'registerPackFile' takes the basename of a pack file, and a list of
      -- SHAs which are contained with the pack.  It must register this in an
      -- index, for the sake of the next function.

    , lookupObject :: SHA -> IO (Maybe ObjectStatus)
      -- 'locateObject' takes a SHA, and returns: Nothing if the object is
      -- "loose", or Just SHA identifying the basename of the packfile that
      -- the object is located within.

    , getBucket  :: (MonadIO m, MonadBaseControl IO m)
                 => Text -> Text -> ResourceT m (Maybe [Text])
    , headObject :: (MonadIO m, MonadBaseControl IO m)
                 => Text -> Text -> ResourceT m (Maybe Bool)
    , getObject  :: (MonadIO m, MonadBaseControl IO m)
                 => Text -> Text -> Maybe (Int64, Int64)
                 -> ResourceT m (Maybe (Either Text BL.ByteString))
    , putObject  :: (MonadIO m, MonadBaseControl IO m)
                 => Text -> Text -> ObjectLength -> BL.ByteString
                 -> ResourceT m (Maybe (Either Text ()))
      -- These three methods allow mocking of S3.
      --
      -- - 'getBucket' takes the bucket and a prefix, and returns Just [xs] to
      --   indicate the list of files in the bucket, or else Nothing if the
      --   method is not mocked.
      --
      -- - 'headObject' takes the bucket and path, and returns Just True if an
      --   object exists at that path, Just False if not, and Nothing if the
      --   method is not mocked.
      --
      -- - 'getObject' takes the bucket, path and an optional range of bytes
      --   (see the S3 API for deatils), and returns a Just Right bytestring
      --   to represent the contents, a Just Left error, or Nothing if the
      --   method is not mocked.
      --
      -- - 'putObject' takes the bucket, path, length and a bytestring source,
      --   and stores the contents at that location.  It returns Just Right ()
      --   if it succeeds, a Just Left on error, or Nothing if the method is not
      --   mocked.

    , updateRef  :: Git.RefName -> Text -> IO ()
    , resolveRef :: Git.RefName -> IO (Maybe Text)

    , acquireLock :: Text -> IO Text
    , releaseLock :: Text -> IO ()

    , shuttingDown :: IO ()
      -- 'shuttingDown' informs whoever registered with this backend that we
      -- are about to disappear, and as such any resources which they acquired
      -- on behalf of this backend should be released.

    , setException :: Git.GitException -> IO ()
      -- 'setException' is used to indicate to gitlib that a more meaningful
      -- exception has occurred, from the one that will be raised by libgit2
      -- upon exiting this backend with an error status.
    }

instance Default BackendCallbacks where
    def = BackendCallbacks
        { checkQuota       = \_       -> return Nothing
        , registerObject   = \_ _     -> return ()
        , registerPackFile = \_ _     -> return ()
        , lookupObject     = \_       -> return Nothing
        , getBucket        = \_ _     -> return Nothing
        , headObject       = \_ _     -> return Nothing
        , getObject        = \_ _ _   -> return Nothing
        , putObject        = \_ _ _ _ -> return Nothing
        , updateRef        = \_ _     -> return ()
        , resolveRef       = \_       -> return Nothing
        , acquireLock      = \_       -> return ""
        , releaseLock      = \_       -> return ()
        , shuttingDown     = return ()
        , setException     = \_       -> return ()
        }

data CacheEntry
    = DoesNotExist

    | LooseRemote
    | LooseRemoteMetaKnown
      { objectLength :: ObjectLength
      , objectType   :: ObjectType
      }
    | LooseCached
      { objectLength :: ObjectLength
      , objectType   :: ObjectType
      , objectCached :: UTCTime
      , objectPath   :: FilePath
      }

    | PackedRemote
      { objectPackSha :: Text
      }
    | PackedCached
      { objectCached    :: UTCTime
      , objectPackSha   :: Text
      , objectPackPath  :: FilePath
      , objectIndexPath :: FilePath
      }
    | PackedCachedMetaKnown
      { objectLength    :: ObjectLength
      , objectType      :: ObjectType
      , objectCached    :: UTCTime
        -- Must always be a PackedCached value
      , objectPackSha   :: Text
      , objectPackPath  :: FilePath
      , objectIndexPath :: FilePath
      }
    deriving (Eq, Show)

data OdbS3Details = OdbS3Details
    { httpManager     :: Manager
    , bucketName      :: Text
    , objectPrefix    :: Text
    , configuration   :: Configuration
    , s3configuration :: Aws.S3Configuration NormalQuery
    , callbacks       :: BackendCallbacks
      -- In the 'knownObjects' map, if the object is not present, we must query
      -- via the 'lookupObject' callback above.  If it is present, it can be
      -- one of the CacheEntry's possible states.
    , knownObjects    :: TVar (HashMap SHA CacheEntry)
    , tempDirectory   :: FilePath
    }

data OdbS3Backend = OdbS3Backend
    { odbS3Parent :: C'git_odb_backend
    , packWriter  :: Ptr C'git_odb_writepack
    , details     :: StablePtr OdbS3Details
    }

instance Storable OdbS3Backend where
  alignment _ = alignment (undefined :: Ptr C'git_odb_backend)
  sizeOf _ =
        sizeOf (undefined :: C'git_odb_backend)
      + sizeOf (undefined :: Ptr C'git_odb_writepack)
      + sizeOf (undefined :: StablePtr OdbS3Details)

  peek p = do
    v0 <- peekByteOff p 0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    v1 <- peekByteOff p sizev1
    let sizev2 = sizev1 + sizeOf (undefined :: Ptr C'git_odb_writepack)
    v2 <- peekByteOff p sizev2
    return (OdbS3Backend v0 v1 v2)

  poke p (OdbS3Backend v0 v1 v2) = do
    pokeByteOff p 0 v0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    pokeByteOff p sizev1 v1
    let sizev2 = sizev1 + sizeOf (undefined :: Ptr C'git_odb_writepack)
    pokeByteOff p sizev2 v2
    return ()

toType :: ObjectType -> C'git_otype
toType (ObjectType t) = fromIntegral t

toLength :: ObjectLength -> CSize
toLength (ObjectLength l) = fromIntegral l

fromType :: C'git_otype -> ObjectType
fromType = ObjectType . fromIntegral

fromLength :: CSize -> ObjectLength
fromLength = ObjectLength . fromIntegral

wrap :: (Show a, MonadIO m, MonadBaseControl IO m)
     => String -> m a -> m a -> m a
wrap msg f g = catch
    (do debug $ msg ++ "..."
        r <- f
        debug $ msg ++ "...done, result = " ++ show r
        return r)
    $ \e -> do liftIO $ putStrLn $ msg ++ "...FAILED"
               liftIO $ print (e :: SomeException)
               g

orElse :: (MonadIO m, MonadBaseControl IO m) => m a -> m a -> m a
orElse f g = catch f $ \e -> do
    liftIO $ putStrLn "A callback operation failed"
    liftIO $ print (e :: SomeException)
    g

coidToJSON :: ForeignPtr C'git_oid -> A.Value
coidToJSON coid = unsafePerformIO $ withForeignPtr coid $
    fmap A.toJSON . flip oidToStr 40

pokeByteString :: ByteString -> Ptr (Ptr b) -> ObjectLength -> IO ()
pokeByteString bytes data_p (fromIntegral . getObjectLength -> len) = do
    content <- mallocBytes len
    BU.unsafeUseAsCString bytes $ copyBytes content ?? len
    poke data_p (castPtr content)

unpackDetails :: Ptr C'git_odb_backend -> Ptr C'git_oid
              -> IO (OdbS3Details, SHA)
unpackDetails be oid = do
    odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
    dets  <- deRefStablePtr (details odbS3)
    sha   <- oidToSha oid
    return (dets, sha)

wrapCheckQuota :: (ObjectLength -> IO (Maybe QuotaStatus))
               -> ObjectLength
               -> IO (Maybe QuotaStatus)
wrapCheckQuota f len =
    wrap ("checkQuota " ++ show len)
        (f len)
        (return Nothing)

wrapRegisterObject :: (SHA -> Maybe (ObjectLength, ObjectType) -> IO ())
                   -> SHA
                   -> Maybe (ObjectLength, ObjectType)
                   -> IO ()
wrapRegisterObject f name metadata =
    wrap ("registerObject " ++ show (shaToText name) ++ " " ++ show metadata)
        (f name metadata)
        (return ())

wrapRegisterPackFile :: (Text -> [SHA] -> IO ())
                     -> Text
                     -> [SHA]
                     -> IO ()
wrapRegisterPackFile f name shas =
    wrap ("registerPackFile: " ++ show name)
        (f name shas)
        (return ())

wrapLookupObject :: (SHA -> IO (Maybe ObjectStatus))
                 -> SHA
                 -> IO (Maybe ObjectStatus)
wrapLookupObject f name =
    wrap ("lookupObject: " ++ show (shaToText name))
        (f name)
        (return Nothing)

wrapGetBucket :: (MonadIO m, MonadBaseControl IO m)
              => (Text -> Text -> ResourceT m (Maybe [Text])) -> Text -> Text
              -> ResourceT m (Maybe [Text])
wrapGetBucket f bucket prefix =
    wrap ("getBucket: " ++ show bucket ++ " " ++ show prefix)
        (f bucket prefix)
        (return Nothing)

wrapHeadObject :: (MonadIO m, MonadBaseControl IO m)
               => (Text -> Text -> ResourceT m (Maybe Bool))
               -> Text
               -> Text
               -> ResourceT m (Maybe Bool)
wrapHeadObject f bucket path =
    wrap ("headObject: " ++ show bucket ++ "/" ++ show path)
        (f bucket path)
        (return Nothing)

wrapGetObject :: (MonadIO m, MonadBaseControl IO m)
              => (Text -> Text -> Maybe (Int64, Int64)
                  -> ResourceT m (Maybe (Either Text BL.ByteString)))
              -> Text
              -> Text
              -> Maybe (Int64, Int64)
              -> ResourceT m (Maybe (Either Text BL.ByteString))
wrapGetObject f bucket path range =
    wrap ("getObject: " ++ show bucket ++ "/" ++ show path
             ++ " " ++ show range)
        (f bucket path range)
        (return Nothing)

wrapPutObject :: (MonadIO m, MonadBaseControl IO m)
              => (Text -> Text -> ObjectLength -> BL.ByteString
                  -> ResourceT m (Maybe (Either Text ())))
              -> Text
              -> Text
              -> ObjectLength
              -> BL.ByteString
              -> ResourceT m (Maybe (Either Text ()))
wrapPutObject f bucket path len bytes =
    wrap ("putObject: " ++ show bucket ++ "/" ++ show path
             ++ " length " ++ show len)
        (f bucket path len bytes)
        (return Nothing)

wrapUpdateRef :: (Text -> Text -> IO ()) -> Text -> Text -> IO ()
wrapUpdateRef f name sha =
    wrap ("updateRef: " ++ show name ++ " " ++ show sha)
        (f name sha)
        (return ())

wrapResolveRef :: (Text -> IO (Maybe Text)) -> Text -> IO (Maybe Text)
wrapResolveRef f name =
    wrap ("resolveRef: " ++ show name)
        (f name)
        (return Nothing)

wrapAcquireLock :: (Text -> IO Text) -> Text -> IO Text
wrapAcquireLock f name =
    wrap ("acquireLock: " ++ show name)
        (f name)
        (return "")

wrapReleaseLock :: (Text -> IO ()) -> Text -> IO ()
wrapReleaseLock f name =
    wrap ("releaseLock: " ++ show name)
        (f name)
        (return ())

wrapShuttingDown :: IO () -> IO ()
wrapShuttingDown f = wrap "shuttingDown..." f (return ())

wrapSetException :: (Git.GitException -> IO ()) -> Git.GitException -> IO ()
wrapSetException f e =
    wrap ("setException: " ++ show e)
        (f e)
        (return ())

awsRetry :: Transaction r a
         => Configuration
         -> ServiceConfiguration r NormalQuery
         -> Manager
         -> r
         -> ResourceT IO (Response (ResponseMetadata a) a)
awsRetry = ((((retrying def (isFailure . responseResult) .) .) .) .) aws

listBucketS3 :: OdbS3Details -> ResourceT IO [Text]
listBucketS3 dets = do
    debug "listBucketS3"
    let bucket = bucketName dets
        prefix = objectPrefix dets
    cbResult <- wrapGetBucket (getBucket (callbacks dets))
                    bucket prefix `orElse` return Nothing
    case cbResult of
        Just r  -> return r
        Nothing -> makeRequest bucket prefix Nothing True
  where
    makeRequest _ _ _ False = return []
    makeRequest bucket prefix mmarker True =  do
        debug "Aws.getBucket"
        res <- awsRetry (configuration dets) (s3configuration dets)
                   (httpManager dets)
                   ((Aws.getBucket bucket)
                        { Aws.gbPrefix = Just prefix
                        , Aws.gbMarker = mmarker })
        gbr <- readResponseIO res
        let contents = map Aws.objectKey (Aws.gbrContents gbr)
        case contents of
            [] -> return []
            _  -> (++) <$> pure contents
                      <*> makeRequest bucket prefix
                              (Just (Prelude.last contents))
                              (Aws.gbrIsTruncated gbr)

testFileS3 :: OdbS3Details -> Text -> ResourceT IO Bool
testFileS3 dets filepath = do
    debug $ "testFileS3: " ++ show filepath

    let bucket = bucketName dets
        path   = T.append (objectPrefix dets) filepath

    cbResult <- wrapHeadObject (headObject (callbacks dets))
                    bucket path `orElse` return Nothing
    case cbResult of
        Just r  -> return r
        Nothing -> do
            debug $ "Aws.headObject: " ++ show filepath
            resp <- awsRetry (configuration dets) (s3configuration dets)
                (httpManager dets) (Aws.headObject bucket path)
            _hor <- readResponseIO resp
            -- If we reach this point, it means the answer was 200 OK, which
            -- means the object exists.
            return True

getFileS3 :: OdbS3Details -> FilePath -> Maybe (Int64,Int64)
          -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFileS3 dets filepath range = do
    debug $ "getFileS3: " ++ filepath

    let bucket = bucketName dets
        path   = T.unpack (objectPrefix dets) <> filepath

    cbResult <- wrapGetObject (getObject (callbacks dets))
                    bucket (T.pack path) range `orElse` return Nothing
    case cbResult of
        Just (Right r) -> fst <$> (sourceLbs r $$+ Data.Conduit.Binary.take 0)
        _ -> do
            debug $ "Aws.getObject: " ++ show filepath ++ " " ++ show range
            res <- awsRetry (configuration dets) (s3configuration dets)
                       (httpManager dets) (Aws.getObject bucket (T.pack path))
                           { Aws.goResponseContentRange =
                                  bimap fromIntegral fromIntegral <$> range }
            gor <- readResponseIO res
            return (responseBody (Aws.gorResponse gor))

putFileS3 :: OdbS3Details -> Text -> Source (ResourceT IO) ByteString
          -> ResourceT IO ()
putFileS3 dets filepath src = do
    debug $ "putFileS3: " ++ show filepath

    let bucket = bucketName dets
        path   = T.append (objectPrefix dets) filepath
    lbs <- BL.fromChunks <$> (src $$ CList.consume)

    cbResult <- wrapPutObject (putObject (callbacks dets)) bucket path
                    (ObjectLength (BL.length lbs)) lbs
                    `orElse` return Nothing
    case cbResult of
        Just (Right r) -> return r
        _ -> do
            debug $ "Aws.putObject: " ++ show filepath
                 ++ " len " ++ show (BL.length lbs)
            res <- awsRetry (configuration dets) (s3configuration dets)
                       (httpManager dets)
                       (Aws.putObject (bucketName dets)
                                  (T.append (objectPrefix dets) filepath)
                            (RequestBodyLBS lbs))
            void $ readResponseIO res

type RefMap m = M.HashMap Text (Maybe (Git.RefTarget (LgRepository m)))

-- jww (2013-04-26): Split these off into a gitlib-aeson library.
instance A.FromJSON (RefTarget m) where
    parseJSON j = do
        o <- A.parseJSON j
        case L.lookup "symbolic" (M.toList (o :: A.Object)) of
            Just _  -> Git.RefSymbolic <$> o .: "symbolic-target"
            Nothing -> Git.RefObj . go <$> o .: "oid-target"
      where
        go = return . mkOid . unsafePerformIO . strToOid

        strToOid :: String -> IO (ForeignPtr C'git_oid)
        strToOid oidStr = do
            ptr <- mallocForeignPtr
            withCString oidStr $ \cstr ->
              withForeignPtr ptr $ \ptr' -> do
                r <- c'git_oid_fromstr ptr' cstr
                when (r < 0) $ throwIO Git.OidCopyFailed
                return ptr

instance Git.MonadGit m => A.ToJSON (RefTarget m) where
  toJSON (Git.RefSymbolic target) =
      object [ "symbolic-target" .= target ]
  toJSON (Git.RefObj oid) =
      object [ "oid-target" .= coidToJSON (getOid (unTagged oid)) ]

readRefs :: Ptr C'git_odb_backend -> IO (Maybe (RefMap m))
readRefs be = do
    odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
    dets   <- deRefStablePtr (details odbS3)
    exists <- wrap "Failed to check whether 'refs.json' exists"
                  (runResourceT $ testFileS3 dets "refs.json")
                  (return False)
    if exists
        then do
            bytes <- wrap "Failed to read 'refs.json'"
                         (runResourceT $ do
                             result <- getFileS3 dets "refs.json" Nothing
                             result $$+- await)
                         (return Nothing)
            return . join $ A.decode . BL.fromChunks . (:[]) <$> bytes
        else return Nothing

writeRefs :: Git.MonadGit m => Ptr C'git_odb_backend -> RefMap m -> IO ()
writeRefs be refs = do
    odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
    dets   <- deRefStablePtr (details odbS3)
    void $ runResourceT $
        putFileS3 dets "refs.json" $ sourceLbs (A.encode refs)

mirrorRefsFromS3 :: Git.MonadGit m => Ptr C'git_odb_backend -> LgRepository m ()
mirrorRefsFromS3 be = do
    repo <- lgGet
    refs <- liftIO $ readRefs be
    for_ refs $ \refs' ->
        forM_ (M.toList refs') $ \(name, ref) ->
            liftIO $ withForeignPtr (repoObj repo) $ \repoPtr ->
                withCString (T.unpack name) $ \namePtr ->
                    alloca (go repoPtr namePtr ref)
  where
    go repoPtr namePtr ref ptr = do
        r <- case ref of
            Just (Git.RefSymbolic target) ->
                withCString (T.unpack target) $ \targetPtr ->
                    c'git_reference_symbolic_create ptr repoPtr namePtr
                        targetPtr 1
            Just (Git.RefObj (Tagged coid)) ->
                withForeignPtr (getOid coid) $ \coidPtr ->
                    c'git_reference_create ptr repoPtr namePtr coidPtr 1
            _ -> return 0
        when (r < 0) $ throwIO Git.RepositoryInvalid

mirrorRefsToS3 :: Git.MonadGit m => Ptr C'git_odb_backend -> LgRepository m ()
mirrorRefsToS3 be = do
    names <- Git.listReferences
    refs  <- mapM Git.lookupReference names
    liftIO $ writeRefs be (M.fromList (L.zip names refs))

observePackObjects :: OdbS3Details
                   -> Text
                   -> FilePath
                   -> Bool
                   -> Ptr C'git_odb
                   -> IO [SHA]
observePackObjects dets packSha idxFile _alsoWithRemote odbPtr = do
    debug $ "observePackObjects: " ++ show idxFile

    -- Iterate the "database", which gives us a list of all the oids contained
    -- within it
    mshas <- newIORef []
    r <- flip (lgForEachObject odbPtr) nullPtr $ \oid _ -> do
        sha <- oidToSha oid
        modifyIORef mshas (sha:)
        return c'GIT_OK
    checkResult r "lgForEachObject failed"

    -- Update the known objects map with the fact that we've got a local cache
    -- of the pack file.
    debug "observePackObjects: update known objects map"
    now  <- getCurrentTime
    shas <- readIORef mshas
    let obj = PackedCached now packSha
                  (replaceExtension idxFile "pack") idxFile
    atomically $ modifyTVar (knownObjects dets) $ \objs ->
        foldr (`M.insert` obj) objs shas

    debug $ "observePackObjects: observed "
        ++ show (Prelude.length shas) ++ " objects"
    return shas

catalogPackFile :: OdbS3Details -> Text -> FilePath -> IO [SHA]
catalogPackFile dets packSha idxPath = do
    -- Load the pack file, and iterate over the objects within it to determine
    -- what it contains.  When 'withPackFile' returns, the pack file will be
    -- closed and any associated resources freed.
    debug $ "catalogPackFile: " ++ show packSha
    lgWithPackFile idxPath $
        liftIO . observePackObjects dets packSha idxPath True

cacheLookupEntry :: OdbS3Details -> SHA -> IO (Maybe CacheEntry)
cacheLookupEntry dets sha =
    wrap ("cacheLookupEntry " ++ show (shaToText sha))
        go
        (return Nothing)
  where
    go = do
        objs <- readTVarIO (knownObjects dets)
        return $ M.lookup sha objs

cacheUpdateEntry :: OdbS3Details -> SHA -> CacheEntry -> IO ()
cacheUpdateEntry dets sha ce = do
    debug $ "cacheUpdateEntry " ++ show (shaToText sha) ++ " " ++ show ce
    atomically $ modifyTVar (knownObjects dets) $ M.insert sha ce

cacheLoadObject :: OdbS3Details -> SHA -> CacheEntry -> Bool
                -> IO (Maybe ObjectInfo)
cacheLoadObject dets sha ce metadataOnly = do
    debug $ "cacheLoadObject " ++ show sha ++ " " ++ show metadataOnly
    minfo <- go ce
    for_ minfo $ cacheStoreObject dets sha -- refresh the cache's knowledge
    return minfo
  where
    go DoesNotExist = return Nothing

    go LooseRemote = runResourceT $ remoteLoadObject dets sha
    go (LooseRemoteMetaKnown len typ) =
        if metadataOnly
        then return . Just $ ObjectInfo len typ Nothing Nothing
        else runResourceT $ remoteLoadObject dets sha

    go (LooseCached len typ _ path)
        | metadataOnly =
            return . Just $ ObjectInfo len typ (Just path) Nothing
        | otherwise = do
            exists <- liftIO $ doesFileExist path
            if exists
                then Just <$> (ObjectInfo
                               <$> pure len
                               <*> pure typ
                               <*> pure (Just path)
                               <*> (Just <$> B.readFile path))
                else go LooseRemote

    go (PackedRemote packSha) = do
        mpaths <- runResourceT $ remoteReadPackFile dets packSha True
        join <$> for mpaths (\(packPath,idxPath) -> do
            void $ catalogPackFile dets packSha idxPath
            packLoadObject dets sha packSha packPath idxPath metadataOnly)

    go (PackedCached _ packSha packPath idxPath) =
        packLoadObject dets sha packSha packPath idxPath metadataOnly

    go (PackedCachedMetaKnown len typ _ packSha packPath idxPath)
        | metadataOnly =
            return . Just $ ObjectInfo len typ Nothing Nothing
        | otherwise =
            packLoadObject dets sha packSha packPath idxPath metadataOnly

    packLoadObject _dets sha packSha packPath idxPath metadataOnly = do
        bothExist <- liftIO $ (&&) <$> doesFileExist packPath
                                  <*> doesFileExist idxPath
        if bothExist
            then do
                debug $ "getObjectFromPack " ++ show packPath ++ " " ++ show sha
                mresult <- lgReadFromPack idxPath sha metadataOnly
                for mresult $ \(typ, len, bytes) ->
                    return $ ObjectInfo (fromLength len) (fromType typ)
                        Nothing (Just bytes)
            else go (PackedRemote packSha)

cacheStoreObject :: OdbS3Details -> SHA -> ObjectInfo -> IO ()
cacheStoreObject dets sha info@ObjectInfo {..} = do
    debug $ "cacheStoreObject " ++ show sha ++ " " ++ show info
    go >>= cacheUpdateEntry dets sha
  where
    go | Just path <- infoPath = do
           for_ infoData $ B.writeFile path
           now <- getCurrentTime
           return $ LooseCached infoLength infoType now path

       | Just bytes <- infoData = do
           let path = tempDirectory dets </> fromSha sha
           B.writeFile path bytes
           now <- getCurrentTime
           return $ LooseCached infoLength infoType now path

       | otherwise =
           return $ LooseRemoteMetaKnown infoLength infoType

callbackLocateObject :: OdbS3Details -> SHA -> IO (Maybe CacheEntry)
callbackLocateObject dets sha = do
    location <- wrapLookupObject (lookupObject (callbacks dets))
                    sha `orElse` return Nothing
    debug $ "callbackLocateObject lookup: " ++ show location
    return $ case location of
        Just (ObjectInPack base) -> Just (PackedRemote base)
        Just ObjectLoose         -> Just LooseRemote
        _                        -> Nothing

callbackRegisterObject :: OdbS3Details -> SHA -> ObjectInfo -> IO ()
callbackRegisterObject dets sha info@ObjectInfo {..} = do
    debug $ "callbackRegisterObject " ++ show sha ++ " " ++ show info
    wrapRegisterObject (registerObject (callbacks dets)) sha
        (Just (infoLength, infoType))`orElse` return ()

callbackRegisterPackFile :: OdbS3Details -> Text -> [SHA] -> IO ()
callbackRegisterPackFile dets packSha shas = do
    debug $ "callbackRegisterPackFile " ++ show packSha
    -- Let whoever is listening know about this pack files and its contained
    -- objects
    wrapRegisterPackFile (registerPackFile (callbacks dets))
        packSha shas `orElse` return ()

callbackRegisterCacheEntry :: OdbS3Details -> SHA -> CacheEntry -> IO ()
callbackRegisterCacheEntry dets sha ce =
    wrap ("callbackRegisterCacheEntry "
              ++ show (shaToText sha) ++ " " ++ show ce)
        (go ce)
        (return ())
  where
    go DoesNotExist               = return ()
    go LooseRemote                = regObj Nothing
    go LooseRemoteMetaKnown {..}  = regObj (Just (objectLength, objectType))
    go LooseCached {..}           = regObj (Just (objectLength, objectType))
    go PackedRemote {..}          = err
    go PackedCached {..}          = err
    go PackedCachedMetaKnown {..} = err

    regObj = wrapRegisterObject (registerObject (callbacks dets)) sha

    err = throwIO (Git.BackendError $
                   "callbackRecordInfo called with " <> T.pack (show ce))

remoteObjectExists :: OdbS3Details -> SHA -> ResourceT IO Bool
remoteObjectExists dets sha =
    wrap "remoteObjectExists"
        (testFileS3 dets (shaToText sha))
        (return False)

remoteReadFile :: OdbS3Details -> FilePath -> ResourceT IO (Maybe ObjectInfo)
remoteReadFile dets path = do
    debug $ "remoteReadFile " ++ show path
    exists <- liftIO $ doesFileExist path
    when exists $ do
        debug $ "remoteReadFile: removing " ++ show path
        liftIO $ removeFile path
    blocks <- do
        result <- getFileS3 dets (takeFileName path) Nothing
        result $$+- CList.consume
    debug $ "remoteReadFile: downloaded " ++ show path
    case blocks of
      [] -> return Nothing
      bs -> processData bs
  where
    processData bs = do
        let hdrLen = sizeOf (undefined :: Int64) * 2
            (len,typ) =
                mapPair fromIntegral
                    (Bin.decode (BL.fromChunks [L.head bs])
                     :: (Int64,Int64))
        debug $ "downloadFile: length from header is " ++ show len
        content <- liftIO $ mallocBytes len
        foldM_ (readData hdrLen content) 0 bs
        bytes <- liftIO $ curry BU.unsafePackCStringLen
                     (castPtr content) (fromIntegral len)
        return . Just $ ObjectInfo
            { infoLength = ObjectLength (fromIntegral len)
            , infoType   = ObjectType (fromIntegral typ)
            , infoPath   = Just path
            , infoData   = Just bytes
            }

    readData hdrLen content offset x = liftIO $ do
        let xOffset  = if offset == 0 then hdrLen else 0
            innerLen = B.length x - xOffset
        BU.unsafeUseAsCString x $ \cstr ->
            copyBytes (content `plusPtr` offset)
                (cstr `plusPtr` xOffset) innerLen
        return (offset + innerLen)

    mapPair f (x,y) = (f x, f y)

remoteReadPackFile :: OdbS3Details -> Text -> Bool
                   -> ResourceT IO (Maybe (FilePath, FilePath))
remoteReadPackFile dets packSha readPackAndIndex = do
    debug $ "remoteReadPackFile " ++ show packSha
    let tmpDir   = tempDirectory dets
        packPath = tmpDir </> ("pack-" <> T.unpack packSha <> ".pack")
        idxPath  = replaceExtension packPath "idx"

    runMaybeT $ do
        -- jww (2013-07-22): It would help performance if we could drop the
        -- "True ||" here, but right now this is not working with the current
        -- libgit2.
        when (True || readPackAndIndex) $ do
            exists <- liftIO $ doesFileExist packPath
            void $ if exists
                   then return (Just ())
                   else download packPath
        exists <- liftIO $ doesFileExist idxPath
        void $ if exists
               then return (Just ())
               else download idxPath
        return (packPath,idxPath)
  where
    download path = do
        minfo <- lift $ remoteReadFile dets path
        for minfo $ \ObjectInfo {..} ->
            expectingJust
                ("failed to download data for " <> T.pack path) infoData
                >>= liftIO . B.writeFile path

remoteWriteFile :: OdbS3Details -> Text -> ObjectType -> ByteString
                -> ResourceT IO ()
remoteWriteFile dets path typ bytes = do
    mstatus <- liftIO $ wrapCheckQuota (checkQuota (callbacks dets))
                   (ObjectLength (fromIntegral (B.length bytes)))
    case mstatus of
        Nothing -> go
        Just QuotaCheckSuccess -> go
        Just (QuotaSoftLimitExceeded {}) -> go
        Just (QuotaHardLimitExceeded {..}) -> do
            let e = Git.QuotaHardLimitExceeded
                        (fromIntegral quotaStatusAmount)
                        (fromIntegral quotaStatusLimit)
            liftIO $ wrapSetException (setException (callbacks dets)) e
            throw e
  where
    go = do
        debug $ "remoteWriteFile " ++ show path
        let hdr = Bin.encode ((fromIntegral (B.length bytes),
                               fromIntegral (getObjectType typ))
                              :: (Int64,Int64))
            payload = BL.append hdr (BL.fromChunks [bytes])
        putFileS3 dets path (sourceLbs payload)

remoteLoadObject :: OdbS3Details -> SHA -> ResourceT IO (Maybe ObjectInfo)
remoteLoadObject dets sha = do
    let tmpDir = tempDirectory dets
        path   = tmpDir </> fromSha sha
    remoteReadFile dets path

remoteStoreObject :: OdbS3Details -> SHA -> ObjectInfo -> ResourceT IO ()
remoteStoreObject dets sha (ObjectInfo _ typ _ (Just bytes)) =
    remoteWriteFile dets (shaToText sha) typ bytes
remoteStoreObject _ _ _ =
    throw (Git.BackendError "remoteStoreObject was not given any data")

remoteCatalogContents :: OdbS3Details -> ResourceT IO ()
remoteCatalogContents dets = do
    debug "remoteCatalogContents"
    items <- map T.unpack <$> listBucketS3 dets
    for_ items $ \item -> case () of
        () | ".idx" `L.isSuffixOf` item -> do
                let packSha = T.pack . drop 5 . takeBaseName $ item
                debug $ "remoteCatalogContents: found pack file "
                     ++ show packSha
                mpaths <- remoteReadPackFile dets packSha False
                for_ mpaths $ \(_, idxPath) -> liftIO $ do
                    shas <- catalogPackFile dets packSha idxPath
                    callbackRegisterPackFile dets packSha shas

           | ".pack" `L.isSuffixOf` item -> return ()

           | length item == 40 -> liftIO $ do
                sha <- Git.textToSha . T.pack . takeBaseName $ item
                cacheUpdateEntry dets sha LooseRemote
                callbackRegisterCacheEntry dets sha LooseRemote

           | otherwise -> return ()

accessObject :: OdbS3Details -> SHA -> Bool -> IO (Maybe CacheEntry)
accessObject dets sha checkRemote = scoped $ \exit -> do
    mentry <- liftIO $ cacheLookupEntry dets sha
    for_ mentry $ const $
        exit mentry

    mentry <- liftIO $ callbackLocateObject dets sha
    for_ mentry $ \entry -> do
        liftIO $ cacheUpdateEntry dets sha entry
        exit mentry

    unless checkRemote $ exit Nothing

    exists <- liftIO $ runResourceT $ remoteObjectExists dets sha
    when exists $ do
        liftIO $ cacheUpdateEntry dets sha LooseRemote
        liftIO $ callbackRegisterCacheEntry dets sha LooseRemote
        exit (Just LooseRemote)

    -- This can be a very time consuming operation
    liftIO $ runResourceT $ remoteCatalogContents dets

    liftIO $ cacheLookupEntry dets sha
  where
    scoped = flip runContT return . callCC

-- All of these functions follow the same general outline:
--
--  1. Check whether the local cache can answer the request.
--
--  2. If the local cache does not know, ask the callback interface, which is
--     usually much cheaper than querying Amazon S3.
--
--  3. If the callback interface does not know, ask Amazon directly if the
--     object exists.
--
--  4. If Amazon does not know about that object per se, catalog the S3 bucket
--     and re-index its contents.  This operation is slow, but is preferable
--     to a failure.
--
--  5. If the object legitimately does not exist, register this fact in the
--     cache and with the callback interface.  This is to avoid recataloging
--     in the future.

expectingJust :: (MonadIO m, MonadBaseControl IO m) => Text -> Maybe a -> m a
expectingJust msg Nothing   = throw (Git.BackendError msg)
expectingJust _msg (Just x) = return x

objectExists :: OdbS3Details -> SHA -> Bool -> IO CacheEntry
objectExists dets sha checkRemote = do
    mce <- accessObject dets sha checkRemote
    return $ fromMaybe DoesNotExist mce

readObject :: OdbS3Details -> SHA -> Bool -> IO (Maybe ObjectInfo)
readObject dets sha metadataOnly = do
    ce <- objectExists dets sha True
    cacheLoadObject dets sha ce metadataOnly `orElse` return Nothing

readObjectMetadata :: OdbS3Details -> SHA -> IO (Maybe ObjectInfo)
readObjectMetadata dets sha = readObject dets sha True

writeObject :: OdbS3Details -> SHA -> ObjectInfo -> IO ()
writeObject dets sha info = do
    runResourceT $ remoteStoreObject dets sha info
    callbackRegisterObject dets sha info
    cacheStoreObject dets sha info

writePackFile :: OdbS3Details -> ByteString -> IO ()
writePackFile dets bytes = do
    let dir = tempDirectory dets
        len = B.length bytes
    debug $ "writePackFile: building index for " ++ show len ++ " bytes"
    (packSha, packPath, idxPath) <- lgBuildPackIndex dir bytes

    runResourceT $
        remoteWriteFile dets (T.pack (takeFileName packPath)) plainFile bytes
    runResourceT
        . remoteWriteFile dets (T.pack (takeFileName idxPath)) plainFile
        =<< B.readFile idxPath

    -- This updates the local cache and remote registry with knowledge of
    -- every object in the pack file.
    shas <- catalogPackFile dets packSha idxPath
    callbackRegisterPackFile dets packSha shas

readCallback :: F'git_odb_backend_read_callback
readCallback data_p len_p type_p be oid = do
    (dets, sha) <- unpackDetails be oid
    wrap (T.unpack $ "S3.readCallback " <> shaToText sha)
        (maybe c'GIT_ENOTFOUND (const c'GIT_OK) <$> go dets sha)
        (return c'GIT_ERROR)
  where
    go dets sha = do
        minfo <- readObject dets sha False
        for minfo $ \(ObjectInfo len typ _ (Just bytes)) -> do
            pokeByteString bytes data_p len
            poke len_p (toLength len)
            poke type_p (toType typ)
            return (Just ())

readPrefixCallback :: F'git_odb_backend_read_prefix_callback
readPrefixCallback _out_oid _oid_p _len_p _type_p _be _oid _len =
    wrap "S3.readPrefixCallback"
        -- jww (2013-04-22): Not yet implemented.
        (throwIO (Git.BackendError
                  "S3.readPrefixCallback has not been implemented yet"))
        (return c'GIT_ERROR)

readHeaderCallback :: F'git_odb_backend_read_header_callback
readHeaderCallback len_p type_p be oid = do
    (dets, sha) <- unpackDetails be oid
    wrap (T.unpack $ "S3.readHeaderCallback " <> shaToText sha)
        (maybe c'GIT_ENOTFOUND (const c'GIT_OK) <$> go dets sha)
        (return c'GIT_ERROR)
  where
    go dets sha = do
        minfo <- readObjectMetadata dets sha
        for minfo $ \(ObjectInfo len typ _ _) -> do
            poke len_p (toLength len)
            poke type_p (toType typ)

writeCallback :: F'git_odb_backend_write_callback
writeCallback oid be obj_data len obj_type = do
    r <- c'git_odb_hash oid obj_data len obj_type
    case r of
        0 -> do
            (dets, sha) <- unpackDetails be oid
            wrap (T.unpack $ "S3.writeCallback " <> shaToText sha)
                (go dets sha >> return c'GIT_OK)
                (return c'GIT_ERROR)
        n -> do
            debug "S3.writeCallback failed to hash data"
            return n
  where
    go dets sha = do
        bytes <- curry BU.unsafePackCStringLen
                     (castPtr obj_data) (fromIntegral len)
        writeObject dets sha
            (ObjectInfo (fromLength len) (fromType obj_type)
                 Nothing (Just bytes))

existsCallback :: F'git_odb_backend_exists_callback
existsCallback be oid confirmNotExists = do
    (dets, sha) <- unpackDetails be oid
    wrap (T.unpack $ "S3.existsCallback " <> shaToText sha
                  <> " " <> T.pack (show confirmNotExists))
        (do ce <- objectExists dets sha (confirmNotExists == 0)
            return $ if ce == DoesNotExist then 0 else 1)
        (return c'GIT_ERROR)

refreshCallback :: F'git_odb_backend_refresh_callback
refreshCallback _ =
    return c'GIT_OK             -- do nothing

foreachCallback :: F'git_odb_backend_foreach_callback
foreachCallback _be _callback _payload =
    return c'GIT_ERROR          -- fallback to standard method

writePackCallback :: F'git_odb_backend_writepack_callback
writePackCallback writePackPtr be _callback _payload =
    wrap "S3.writePackCallback" go (return c'GIT_ERROR)
  where
    go = do
        poke writePackPtr . packWriter
            =<< peek (castPtr be :: Ptr OdbS3Backend)
        return c'GIT_OK

freeCallback :: F'git_odb_backend_free_callback
freeCallback be = do
    debug "S3.freeCallback"
    odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
    dets  <- liftIO $ deRefStablePtr (details odbS3)

    wrapShuttingDown (shuttingDown (callbacks dets)) `orElse` return ()

    let tmpDir = tempDirectory dets
    exists <- doesDirectoryExist tmpDir
    when exists $ do
        debug $ "S3.freeCallback: removing tree " ++ show tmpDir
        removeDirectoryRecursive tmpDir `orElse` return ()

    backend <- peek be
    freeHaskellFunPtr (c'git_odb_backend'read backend)
    freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
    freeHaskellFunPtr (c'git_odb_backend'read_header backend)
    freeHaskellFunPtr (c'git_odb_backend'write backend)
    freeHaskellFunPtr (c'git_odb_backend'exists backend)

    free (packWriter odbS3)
    freeStablePtr (details odbS3)

foreign export ccall "freeCallback"
  freeCallback :: F'git_odb_backend_free_callback
foreign import ccall "&freeCallback"
  freeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

packAddCallback :: F'git_odb_writepack_add_callback
packAddCallback wp dataPtr len _progress =
    wrap "S3.packAddCallback"
        (go >> return c'GIT_OK)
        (return c'GIT_ERROR)
  where
    go = do
        be    <- c'git_odb_writepack'backend <$> peek wp
        odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
        dets  <- deRefStablePtr (details odbS3)
        bytes <- curry BU.unsafePackCStringLen
                     (castPtr dataPtr) (fromIntegral len)
        writePackFile dets bytes

packCommitCallback :: F'git_odb_writepack_commit_callback
packCommitCallback _wp _progress =
    return c'GIT_OK             -- do nothing

packFreeCallback :: F'git_odb_writepack_free_callback
packFreeCallback wp = do
    debug "S3.packFreeCallback"
    writepack <- peek wp
    freeHaskellFunPtr (c'git_odb_writepack'add writepack)
    freeHaskellFunPtr (c'git_odb_writepack'commit writepack)

foreign export ccall "packFreeCallback"
  packFreeCallback :: F'git_odb_writepack_free_callback
foreign import ccall "&packFreeCallback"
  packFreeCallbackPtr :: FunPtr F'git_odb_writepack_free_callback

odbS3Backend :: Git.MonadGit m
             => Aws.S3Configuration NormalQuery
             -> Configuration
             -> Manager
             -> Text
             -> Text
             -> FilePath
             -> BackendCallbacks
             -> m (Ptr C'git_odb_backend)
odbS3Backend s3config config manager bucket prefix dir callbacks = liftIO $ do
  readFun       <- mk'git_odb_backend_read_callback readCallback
  readPrefixFun <- mk'git_odb_backend_read_prefix_callback readPrefixCallback
  readHeaderFun <- mk'git_odb_backend_read_header_callback readHeaderCallback
  writeFun      <- mk'git_odb_backend_write_callback writeCallback
  existsFun     <- mk'git_odb_backend_exists_callback existsCallback
  refreshFun    <- mk'git_odb_backend_refresh_callback refreshCallback
  foreachFun    <- mk'git_odb_backend_foreach_callback foreachCallback
  writepackFun  <- mk'git_odb_backend_writepack_callback writePackCallback

  writePackAddFun    <- mk'git_odb_writepack_add_callback packAddCallback
  writePackCommitFun <- mk'git_odb_writepack_commit_callback packCommitCallback

  objects   <- newTVarIO M.empty
  dirExists <- doesDirectoryExist dir
  unless dirExists $ createDirectoryIfMissing True dir

  let odbS3details = OdbS3Details
          { httpManager     = manager
          , bucketName      = bucket
          , objectPrefix    = prefix
          , configuration   = config
          , s3configuration = s3config
          , callbacks       = callbacks
          , knownObjects    = objects
          , tempDirectory   = dir
          }
      odbS3Parent = C'git_odb_backend
          { c'git_odb_backend'version     = 1
          , c'git_odb_backend'odb         = nullPtr
          , c'git_odb_backend'read        = readFun
          , c'git_odb_backend'read_prefix = readPrefixFun
          , c'git_odb_backend'readstream  = nullFunPtr
          , c'git_odb_backend'read_header = readHeaderFun
          , c'git_odb_backend'write       = writeFun
          , c'git_odb_backend'writestream = nullFunPtr
          , c'git_odb_backend'exists      = existsFun
          , c'git_odb_backend'refresh     = refreshFun
          , c'git_odb_backend'foreach     = foreachFun
          , c'git_odb_backend'writepack   = writepackFun
          , c'git_odb_backend'free        = freeCallbackPtr
          }

  details' <- newStablePtr odbS3details

  ptr <- castPtr <$> new OdbS3Backend
      { odbS3Parent = odbS3Parent
      , packWriter  = nullPtr
      , details     = details'
      }
  packWriterPtr <- new C'git_odb_writepack
      { c'git_odb_writepack'backend = ptr
      , c'git_odb_writepack'add     = writePackAddFun
      , c'git_odb_writepack'commit  = writePackCommitFun
      , c'git_odb_writepack'free    = packFreeCallbackPtr
      }
  pokeByteOff ptr (sizeOf (undefined :: C'git_odb_backend)) packWriterPtr

  return ptr

-- | Given a repository object obtained from Libgit2, add an S3 backend to it,
--   making it the primary store for objects associated with that repository.
addS3Backend :: Git.MonadGit m
             => Repository
             -> Text           -- ^ bucket
             -> Text           -- ^ prefix
             -> Text           -- ^ access key
             -> Text           -- ^ secret key
             -> Maybe Manager
             -> Maybe Text     -- ^ mock address
             -> LogLevel
             -> FilePath
             -> BackendCallbacks -- ^ callbacks
             -> m Repository
addS3Backend repo bucket prefix access secret
    mmanager mockAddr level dir callbacks = do
    manager <- maybe (liftIO $ newManager def) return mmanager
    odbS3   <- liftIO $ odbS3Backend
        (case mockAddr of
            Nothing   -> defServiceConfig
            Just addr -> (Aws.s3 HTTP (T.encodeUtf8 addr) False) {
                               Aws.s3Port         = 10001
                             , Aws.s3RequestStyle = Aws.PathStyle })
        (Configuration Timestamp Credentials {
              accessKeyID     = T.encodeUtf8 access
            , secretAccessKey = T.encodeUtf8 secret }
         (defaultLog level))
        manager bucket prefix dir callbacks
    void $ liftIO $ odbBackendAdd repo odbS3 100
    return repo

s3Factory :: Git.MonadGit m
          => Maybe Text -> Text -> Text -> FilePath -> BackendCallbacks
          -> Git.RepositoryFactory LgRepository m Repository
s3Factory bucket accessKey secretKey dir callbacks = lgFactory
    { Git.runRepository = \ctxt -> runLgRepository ctxt . (s3back >>) }
  where
    s3back = do
        repo <- lgGet
        void $ liftIO $ addS3Backend
            repo
            (fromMaybe "test-bucket" bucket)
            ""
            accessKey
            secretKey
            Nothing
            (if isNothing bucket
             then Just "127.0.0.1"
             else Nothing)
            Aws.Error
            dir
            callbacks

data S3MockService = S3MockService
    { objects :: TVar (HashMap (Text, Text) BL.ByteString)
    }

s3MockService :: IO S3MockService
s3MockService = S3MockService <$> newTVarIO M.empty

mockGetBucket :: (MonadIO m, MonadBaseControl IO m)
              => S3MockService -> Text -> Text -> ResourceT m (Maybe [Text])
mockGetBucket svc _bucket prefix =
    wrap "mockGetBucket" (Just <$> go) (return Nothing)
  where
    go = do
        objs <- liftIO $ readTVarIO (objects svc)
        return $ Prelude.filter (prefix `T.isPrefixOf`)
               $ map snd
               $ M.keys objs

mockHeadObject :: (MonadIO m, MonadBaseControl IO m)
               => S3MockService -> Text -> Text -> ResourceT m (Maybe Bool)
mockHeadObject svc bucket path =
    wrap "mockHeadObject" go (return Nothing)
  where
    go = do
        objs <- liftIO $ readTVarIO (objects svc)
        return $ maybe (Just False) (const (Just True)) $
            M.lookup (bucket, path) objs

mockGetObject :: (MonadIO m, MonadBaseControl IO m)
              => S3MockService -> Text -> Text -> Maybe (Int64, Int64)
              -> ResourceT m (Maybe (Either Text BL.ByteString))
mockGetObject svc bucket path range =
    wrap "mockHeadObject" go (return Nothing)
  where
    go = do
        objs <- liftIO $ readTVarIO (objects svc)
        let obj = maybe (Left $ T.pack $ "Not found: "
                         ++ show bucket ++ "/" ++ show path)
                      Right $
                      M.lookup (bucket, path) objs
        return $ Just $ case range of
            Just (beg,end) -> BL.drop beg <$> BL.take end <$> obj
            Nothing -> obj

mockPutObject :: (MonadIO m, MonadBaseControl IO m)
              => S3MockService -> Text -> Text -> Int -> BL.ByteString
              -> ResourceT m (Maybe (Either Text ()))
mockPutObject svc bucket path _ bytes =
    wrap "mockPutObject" go (return Nothing)
  where
    go = do
        liftIO $ atomically $ modifyTVar (objects svc) $
            M.insert (bucket, path) bytes
        return $ Just $ Right ()

-- S3.hs
