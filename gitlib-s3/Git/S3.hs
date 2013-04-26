{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns #-}

module Git.S3
       ( s3Factory, odbS3Backend, addS3Backend
       , ObjectStatus(..), BackendCallbacks(..)
       , ObjectType(..), ObjectLength(..)
       , S3MockService(), s3MockService
       , mockHeadObject, mockGetObject, mockPutObject
       -- , readRefs, writeRefs
       -- , mirrorRefsFromS3, mirrorRefsToS3
       ) where

import           Aws
import           Aws.Core
import           Aws.S3 hiding (bucketName, headObject, getObject, putObject)
import qualified Aws.S3 as Aws
import           Bindings.Libgit2.Errors
import           Bindings.Libgit2.Odb
import           Bindings.Libgit2.OdbBackend
import           Bindings.Libgit2.Oid
import           Bindings.Libgit2.Refs
import           Bindings.Libgit2.Types
import           Control.Applicative
import           Control.Concurrent.MVar
import           Control.Exception
import qualified Control.Exception.Lifted as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Aeson as A
import           Data.Attempt
import           Data.Bifunctor
import           Data.Binary as Bin
import           Data.ByteString as B hiding (putStrLn, foldr)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Foldable (for_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as M
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text as T hiding (foldr)
import qualified Data.Text.Encoding as E
import           Data.Time.Clock
import           Filesystem
import           Filesystem.Path.CurrentOS hiding (encode, decode)
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
import           Git.Libgit2
import           Git.Libgit2.Backend
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Network.HTTP.Conduit hiding (Response)
import           Prelude hiding (FilePath, mapM_, catch)
import           System.IO.Unsafe

debug :: MonadIO m => String -> m ()
debug = liftIO . putStrLn

data ObjectLength = ObjectLength { getObjectLength :: Int64 }
                  deriving (Eq, Show, Generic)
data ObjectType   = ObjectType { getObjectType :: Int }
                  deriving (Eq, Show, Generic)
data ObjectStatus = ObjectLoose
                  | ObjectLooseMetaKnown ObjectLength ObjectType
                  | ObjectInPack Text
                  deriving (Eq, Show, Generic)

toType :: ObjectType -> C'git_otype
toType (ObjectType t) = fromIntegral t

toLength :: ObjectLength -> CSize
toLength (ObjectLength l) = fromIntegral l

fromType :: C'git_otype -> ObjectType
fromType = ObjectType . fromIntegral

fromLength :: CSize -> ObjectLength
fromLength = ObjectLength . fromIntegral

instance A.ToJSON ObjectLength; instance A.FromJSON ObjectLength
instance A.ToJSON ObjectType;   instance A.FromJSON ObjectType
instance A.ToJSON ObjectStatus; instance A.FromJSON ObjectStatus

data BackendCallbacks = BackendCallbacks
    { registerObject :: Text -> Maybe (ObjectLength, ObjectType) -> IO ()
      -- 'registerObject' reports that a SHA has been written as a loose
      -- object to the S3 repository.  The for tracking it is that sometimes
      -- calling 'locateObject' can be much faster than querying Amazon.

    , registerPackFile :: Text -> [Text] -> IO ()
      -- 'registerPackFile' takes the basename of a pack file, and a list of
      -- SHAs which are contained with the pack.  It must register this in an
      -- index, for the sake of the next function.

    , lookupObject :: Text -> IO (Maybe ObjectStatus)
      -- 'locateObject' takes a SHA, and returns: Nothing if the object is
      -- "loose", or Just Text identifying the basename of the packfile that
      -- the object is located within.

    , lookupPackFile :: Text -> IO (Maybe Bool)
      -- 'locatePackFile' indicates whether a pack file with the given sha is
      -- present on the remote, regardless of which objects it contains.

    , headObject :: MonadIO m => Text -> Text -> ResourceT m (Maybe Bool)
    , getObject  :: MonadIO m => Text -> Text -> Maybe (Int64, Int64)
                 -> ResourceT m (Maybe (Either Text BL.ByteString))
    , putObject  :: MonadIO m => Text -> Text -> ObjectLength -> BL.ByteString
                 -> ResourceT m (Maybe (Either Text ()))
      -- These three methods allow mocking of S3.
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

    , updateRef  :: Text -> Text -> IO ()
    , resolveRef :: Text -> IO (Maybe Text)

    , acquireLock :: Text -> IO Text
    , releaseLock :: Text -> IO ()

    , shuttingDown    :: IO ()
      -- 'shuttingDown' informs whoever registered with this backend that we
      -- are about to disappear, and as such any resources which they acquired
      -- on behalf of this backend should be released.
    }

wrap :: (MonadIO m, MonadBaseControl IO m) => String -> m a -> m a -> m a
wrap msg f g = Exc.catch
        (do debug $ msg ++ "..."
            r <- f
            debug $ msg ++ "...done"
            return r)
        $ \e -> do liftIO $ putStrLn $ msg ++ "...FAILED"
                   liftIO $ print (e :: SomeException)
                   g

wrapRegisterObject :: (Text -> Maybe (ObjectLength, ObjectType) -> IO ())
                   -> Text
                   -> Maybe (ObjectLength, ObjectType)
                   -> IO ()
wrapRegisterObject f name metadata =
    return ()
    -- wrap ("Calling registerObject " ++ show name ++ " " ++ show metadata)
    --     (f name metadata)
    --     (return ())

wrapRegisterPackFile :: (Text -> [Text] -> IO ()) -> Text -> [Text] -> IO ()
wrapRegisterPackFile f name shas =
    wrap ("Calling registerPackFile: " ++ show name)
        (f name shas)
        (return ())

wrapLookupObject :: (Text -> IO (Maybe ObjectStatus))
                 -> Text
                 -> IO (Maybe ObjectStatus)
wrapLookupObject f name =
    wrap ("Calling lookupObject: " ++ show name)
        (f name)
        (return Nothing)

wrapLookupPackFile :: (Text -> IO (Maybe Bool)) -> Text -> IO (Maybe Bool)
wrapLookupPackFile f name =
    wrap ("Calling lookupPackFile: " ++ show name)
        (f name)
        (return Nothing)

wrapHeadObject :: (MonadIO m, MonadBaseControl IO m)
               => (Text -> Text -> ResourceT m (Maybe Bool))
               -> Text
               -> Text
               -> ResourceT m (Maybe Bool)
wrapHeadObject f bucket path =
    wrap ("Calling headObject: " ++ show bucket ++ "/" ++ show path)
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
    wrap ("Calling getObject: " ++ show bucket ++ "/" ++ show path
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
    wrap ("Calling putObject: " ++ show bucket ++ "/" ++ show path
             ++ " length " ++ show len)
        (f bucket path len bytes)
        (return Nothing)

wrapUpdateRef :: (Text -> Text -> IO ()) -> Text -> Text -> IO ()
wrapUpdateRef f name sha =
    wrap ("Calling updateRef: " ++ show name ++ " " ++ show sha)
        (f name sha)
        (return ())

wrapResolveRef :: (Text -> IO (Maybe Text)) -> Text -> IO (Maybe Text)
wrapResolveRef f name =
    wrap ("Calling resolveRef: " ++ show name)
        (f name)
        (return Nothing)

wrapAcquireLock :: (Text -> IO Text) -> Text -> IO Text
wrapAcquireLock f name =
    wrap ("Calling acquireLock: " ++ show name)
        (f name)
        (return "")

wrapReleaseLock :: (Text -> IO ()) -> Text -> IO ()
wrapReleaseLock f name =
    wrap ("Calling releaseLock: " ++ show name)
        (f name)
        (return ())

wrapShuttingDown :: IO () -> IO ()
wrapShuttingDown f = wrap "Calling shuttingDown..." f (return ())

instance Default BackendCallbacks where
    def = BackendCallbacks
        { registerObject   = \_ _     -> return ()
        , registerPackFile = \_ _     -> return ()
        , lookupObject     = \_       -> return Nothing
        , lookupPackFile   = \_       -> return Nothing
        , headObject       = \_ _     -> return Nothing
        , getObject        = \_ _ _   -> return Nothing
        , putObject        = \_ _ _ _ -> return Nothing
        , updateRef        = \_ _     -> return ()
        , resolveRef       = \_       -> return Nothing
        , acquireLock      = \_       -> return ""
        , releaseLock      = \_       -> return ()
        , shuttingDown     = return ()
        }

data OdbS3Object
    = DoesNotExist

    | LooseRemote
    | LooseRemoteMetaKnown
      { objectLength :: ObjectLength
      , objectType   :: ObjectType
      }
    | LooseCached
      { objectPath   :: FilePath
      , objectLength :: ObjectLength
      , objectType   :: ObjectType
      , objectCached :: UTCTime
      }

    | PackedRemote Text
    | PackedCached FilePath FilePath UTCTime
    | PackedCachedMetaKnown
      { objectLength   :: ObjectLength
      , objectType     :: ObjectType
        -- Must always be a PackedCached value
      , objectPackInfo :: OdbS3Object
      }
    deriving (Eq, Show)

data OdbS3Details = OdbS3Details
    { httpManager     :: Manager
    , bucketName      :: Text
    , objectPrefix    :: Text
    , configuration   :: Configuration
    , s3configuration :: S3Configuration NormalQuery
    , callbacks       :: BackendCallbacks
      -- In the 'knownObjects' map, if the object is not present, we must query
      -- via the 'lookupObject' callback above.  If it is present, it can be
      -- one of the OdbS3Object's possible states.
    , knownObjects    :: MVar (HashMap Text OdbS3Object)
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
      + sizeOf (undefined :: StablePtr Manager)
      + sizeOf (undefined :: StablePtr Text)
      + sizeOf (undefined :: StablePtr Text)
      + sizeOf (undefined :: StablePtr Configuration)
      + sizeOf (undefined :: StablePtr (S3Configuration NormalQuery))
      + sizeOf (undefined :: StablePtr BackendCallbacks)

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

orElse :: (MonadIO m, MonadBaseControl IO m) => m a -> m a -> m a
orElse f g = Exc.catch f $ \e -> do
    liftIO $ putStrLn "A callback operation failed"
    liftIO $ print (e :: SomeException)
    g

awsRetry :: Transaction r a
         => Configuration
         -> ServiceConfiguration r NormalQuery
         -> Manager
         -> r
         -> ResourceT IO (Response (ResponseMetadata a) a)
awsRetry = ((((retrying def (isFailure . responseResult) .) .) .) .) aws

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
            isJust . readResponse
                <$> aws (configuration dets) (s3configuration dets)
                        (httpManager dets) (Aws.headObject bucket path)

getFileS3 :: OdbS3Details -> Text -> Maybe (Int64,Int64)
          -> ResourceT IO (ResumableSource (ResourceT IO) ByteString)
getFileS3 dets filepath range = do
    debug $ "getFileS3: " ++ show filepath
    let bucket = bucketName dets
        path   = T.append (objectPrefix dets) filepath
    cbResult <- wrapGetObject (getObject (callbacks dets))
                    bucket path range `orElse` return Nothing
    case cbResult of
        Just (Right r) -> fst <$> (sourceLbs r $$+ Data.Conduit.Binary.take 0)
        _ -> do
            debug $ "Aws.getObject: " ++ show filepath ++ " " ++ show range
            res <- awsRetry (configuration dets) (s3configuration dets)
                       (httpManager dets) (Aws.getObject bucket path)
                           { Aws.goResponseContentRange =
                                  bimap fromIntegral fromIntegral <$> range }
            gor <- readResponseIO res
            return (responseBody (Aws.gorResponse gor))

putFileS3 :: OdbS3Details -> Text -> Source (ResourceT IO) ByteString
          -> ResourceT IO ()
putFileS3 dets filepath src = do
    -- jww (2013-04-24): Reading everything in memory here is not a good idea.
    lbs <- BL.fromChunks <$> (src $$ CList.consume)
    debug $ "putFileS3: " ++ show filepath
    let bucket = bucketName dets
        path   = T.append (objectPrefix dets) filepath
    cbResult <- wrapPutObject (putObject (callbacks dets)) bucket path
                    (ObjectLength (BL.length lbs)) lbs
                    `orElse` return Nothing
    case cbResult of
        Just (Right r) -> return r
        _ -> do
            debug $ "Aws.putObject: " ++ show filepath
                 ++ " len " ++ show (BL.length lbs)
            res <- awsRetry
                       (configuration dets)
                       (s3configuration dets)
                       (httpManager dets)
                       (Aws.putObject (bucketName dets)
                                  (T.append (objectPrefix dets) filepath)
                            (RequestBodyLBS lbs))
            void $ readResponseIO res

type RefMap m =
    M.HashMap Text (Maybe (Git.Reference (LgRepository m) (Commit m)))

instance A.FromJSON (Git.Reference (LgRepository m) (Commit m)) where
    parseJSON j = do
        o <- A.parseJSON j
        case L.lookup "symbolic" (M.toList (o :: A.Object)) of
            Just _ -> Git.Reference
                          <$> o .: "symbolic"
                          <*> (Git.RefSymbolic <$> o .: "target")
            Nothing -> Git.Reference
                           <$> o .: "name"
                           <*> (Git.RefObj . Git.ByOid . go <$> o .: "target")
      where
        go = return . Oid . unsafePerformIO . strToOid

coidToJSON :: ForeignPtr C'git_oid -> A.Value
coidToJSON coid =
    unsafePerformIO $ withForeignPtr coid $ fmap A.toJSON . oidToStr

instance Git.MonadGit m
         => A.ToJSON (Git.Reference (LgRepository m) (Commit m)) where
  toJSON (Git.Reference name (Git.RefSymbolic target)) =
      object [ "symbolic" .= name
             , "target"   .= target ]
  toJSON (Git.Reference name (Git.RefObj (Git.ByOid oid))) =
      object [ "name"   .= name
             , "target" .= coidToJSON (getOid (unTagged oid)) ]
  toJSON (Git.Reference name (Git.RefObj (Git.Known commit))) =
      object [ "name"   .= name
             , "target" .=
               coidToJSON (getOid (unTagged (Git.commitOid commit))) ]

readRefs :: Ptr C'git_odb_backend -> IO (Maybe (RefMap m))
readRefs be = do
    odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
    dets   <- deRefStablePtr (details odbS3)
    exists <- wrap "Failed to check whether 'refs.json' exists"
                  (runResourceT $ testFileS3 dets "refs.json")
                  (return False)
    if exists
        then do
            bytes <- wrap ("Failed to read 'refs.json'")
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
            Just Git.Reference { Git.refTarget = Git.RefSymbolic target } ->
                withCString (T.unpack target) $ \targetPtr ->
                    c'git_reference_symbolic_create ptr repoPtr namePtr
                        targetPtr 1
            Just Git.Reference {
                Git.refTarget = Git.RefObj x@(Git.ByOid (Tagged coid)) } ->
                withForeignPtr (getOid coid) $ \coidPtr ->
                    c'git_reference_create ptr repoPtr namePtr coidPtr 1
            _ -> return 0
        when (r < 0) $ throwIO Git.RepositoryInvalid

mirrorRefsToS3 :: Git.MonadGit m => Ptr C'git_odb_backend -> LgRepository m ()
mirrorRefsToS3 be = do
    odbS3 <- liftIO $ peek (castPtr be :: Ptr OdbS3Backend)
    names <- Git.allRefNames
    refs  <- mapM Git.lookupRef names
    liftIO $ writeRefs be (M.fromList (L.zip names refs))
  where
    go name ref = case Git.refTarget ref of
        Git.RefSymbolic target     -> (name, Left target)
        Git.RefObj (Git.ByOid oid) -> (name, Right oid)

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

uploadPackAndIndex :: OdbS3Details -> FilePath -> FilePath -> Text
                   -> ResourceT IO ()
uploadPackAndIndex dets packPath idxPath packSha = do
    -- Load the pack file, and iterate over the objects within it to determine
    -- what it contains.  When 'withPackFile' returns, the pack file will be
    -- closed and any associated resources freed.
    debug $ "uploadPackAndIndex: " ++ show packSha
    shas <- liftIO $ lgWithPackFile idxPath $
        liftIO . observePackObjects dets packSha idxPath True

    -- Let whoever is listening know about this pack files and its contained
    -- objects
    liftIO $ do
        packKnown <- wrapLookupPackFile (lookupPackFile (callbacks dets))
                         packSha `orElse` return Nothing
        case packKnown of
            Just True -> return ()
            _ -> wrapRegisterPackFile (registerPackFile (callbacks dets))
                     packSha shas `orElse` return ()

    uploadFile dets packPath
    uploadFile dets idxPath

uploadFile :: OdbS3Details -> FilePath -> ResourceT IO ()
uploadFile dets path = do
    lbs <- liftIO $ BL.readFile (pathStr path)
    let hdr = Bin.encode ((fromIntegral (BL.length lbs),
                           fromIntegral 0) :: (Int64,Int64))
        payload = BL.append hdr lbs
    putFileS3 dets (pathText (filename path)) (sourceLbs payload)

downloadFile :: OdbS3Details -> Text
             -> IO (Maybe (ObjectLength, ObjectType, CString))
downloadFile dets path = do
    debug $ "downloadFile: " ++ show path
    blocks <- runResourceT $ do
        result <- getFileS3 dets path Nothing
        result $$+- CList.consume
    debug $ "downloadFile: downloaded " ++ show path
    case blocks of
      [] -> return Nothing
      bs -> do
        let hdrLen = sizeOf (undefined :: Int64) * 2
            (len,typ) =
                mapPair fromIntegral
                    (Bin.decode (BL.fromChunks [L.head bs]) :: (Int64,Int64))
        debug $ "downloadFile: length from header is " ++ show len
        content <- mallocBytes len
        foldM_ (\offset x -> do
                     let xOffset  = if offset == 0 then hdrLen else 0
                         innerLen = B.length x - xOffset
                     BU.unsafeUseAsCString x $ \cstr ->
                         copyBytes (content `plusPtr` offset)
                             (cstr `plusPtr` xOffset) innerLen
                     return (offset + innerLen)) 0 bs
        return $ Just (ObjectLength (fromIntegral len),
                       ObjectType (fromIntegral typ), content)

downloadPack :: OdbS3Details -> Text -> IO (FilePath, FilePath)
downloadPack dets packSha = do
    let idxPath  = tempDirectory dets
                       </> fromText ("pack-" <> packSha <> ".idx")
        packPath = replaceExtension idxPath "pack"

    exists <- isFile packPath
    if exists
        then return (packPath, idxPath)
        else do
            debug $ "downloadPack: " ++ show packSha
            result <- downloadFile dets $ pathText (filename packPath)
            (packLen,packBytes) <- case result of
                Just (packLen,_,packBytes) -> return (packLen,packBytes)
                Nothing -> throwIO (Git.BackendError $
                                    "Failed to download pack " <> packSha)

            result' <- downloadFile dets $ pathText (filename idxPath)
            (idxLen,idxBytes) <- case result' of
                Just (idxLen,_,idxBytes) -> return (idxLen,idxBytes)
                Nothing -> throwIO (Git.BackendError $
                                    "Failed to download index " <> packSha)

            packBS <- curry BU.unsafePackCStringLen packBytes
                          (fromIntegral (getObjectLength packLen))
            idxBS  <- curry BU.unsafePackCStringLen idxBytes
                          (fromIntegral (getObjectLength idxLen))
            writePackToCache dets packSha packBS idxBS

writeObjectToCache :: OdbS3Details
                   -> Text -> ObjectLength -> ObjectType -> ByteString -> IO ()
writeObjectToCache dets sha len typ bytes = do
    debug $ "writeObjectToCache: " ++ show sha
    let path = tempDirectory dets </> fromText sha
    B.writeFile (pathStr path) bytes
    now <- getCurrentTime
    modifyMVar_ (knownObjects dets) $
        return . M.insert sha (LooseCached path len typ now)

writePackToCache :: OdbS3Details -> Text -> ByteString -> ByteString
                 -> IO (FilePath, FilePath)
writePackToCache dets sha packBytes idxBytes = do
    debug $ "writePackToCache: " ++ show sha
    let idxPath  = tempDirectory dets </> fromText ("pack-" <> sha <> ".idx")
        packPath = replaceExtension idxPath "pack"
    debug $ "writeFile: " ++ show packPath
    B.writeFile (pathStr packPath) packBytes
    debug $ "writeFile: " ++ show idxPath
    B.writeFile (pathStr idxPath) idxBytes
    void $ lgWithPackFile idxPath $
        liftIO . observePackObjects dets sha idxPath False
    return (packPath, idxPath)

getObjectFromPack :: OdbS3Details -> FilePath -> FilePath -> Text -> Bool
                  -> IO (Maybe (ObjectLength, ObjectType, ByteString))
getObjectFromPack dets packPath idxPath sha metadataOnly = do
    liftIO $ debug $ "getObjectFromPack "
        ++ show packPath ++ " " ++ show sha
    mresult <- lgReadFromPack idxPath sha metadataOnly
    case (\(typ, len, bytes) -> (fromLength len, fromType typ, bytes))
             <$> mresult of
        x@(Just (len, typ, bytes))
            | B.null bytes -> liftIO $ do
                now <- getCurrentTime
                let obj = PackedCachedMetaKnown len typ
                              (PackedCached packPath idxPath now)
                modifyMVar_ (knownObjects dets) $ return . M.insert sha obj
                return x
            | otherwise -> do
                liftIO $ writeObjectToCache dets sha len typ bytes
                return x
        x -> return x

unpackDetails :: Ptr C'git_odb_backend -> Ptr C'git_oid
              -> IO (OdbS3Details, String, Text)
unpackDetails be oid = do
    odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
    dets   <- deRefStablePtr (details odbS3)
    oidStr <- oidToStr oid
    return (dets, oidStr, T.pack oidStr)

loadFromRemote :: OdbS3Details
               -> Text
               -> (OdbS3Details -> FilePath -> FilePath -> Text -> IO a)
               -> (Maybe ObjectStatus -> IO a)
               -> IO a
loadFromRemote dets sha loader action = do
    location <- wrapLookupObject (lookupObject (callbacks dets)) sha
                    `orElse` return Nothing
    case location of
        Just (ObjectInPack packBase) -> do
            (packPath, idxPath) <- downloadPack dets packBase
            loader dets packPath idxPath sha
        _ -> action location

odbS3BackendReadCallback :: F'git_odb_backend_read_callback
odbS3BackendReadCallback data_p len_p type_p be oid = do
    str <- oidToStr oid
    wrap ("odbS3BackendReadCallback " ++ str) go (return (-1))
  where
    go = do
        (dets, oidStr, sha) <- unpackDetails be oid
        objs <- readMVar (knownObjects dets)
        let deb = M.lookup sha objs
        debug $ "odbS3BackendReadCallback lookup: " ++ show deb
        case M.lookup sha objs of
            Just DoesNotExist -> return c'GIT_ENOTFOUND

            Just (LooseCached path len typ _) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                bytes <- B.readFile (pathStr path)
                pokeByteString bytes len
                return 0

            Just (PackedRemote base) -> do
                (packPath, idxPath) <- downloadPack dets base
                loadFromPack dets packPath idxPath sha

            Just (PackedCached pack idx _) ->
                loadFromPack dets pack idx sha
            Just (PackedCachedMetaKnown _ _ (PackedCached pack idx _)) ->
                loadFromPack dets pack idx sha

            _ -> loadFromRemote dets sha loadFromPack $
                    downloadLoose dets sha

    loadFromPack dets pack idx sha = do
        result <- getObjectFromPack dets pack idx sha False
        case result of
            Just (len,typ,bytes) -> do
                pokeByteString bytes len
                poke len_p (toLength len)
                poke type_p (toType typ)
                return 0
            Nothing -> do
                -- jww (2013-04-25): If we reach this point, first check
                -- whether it's actually a loose object. If not, list the
                -- bucket and see if there are unregistered pack files in
                -- which it might live.  This would close Bug #965.  The same
                -- must be done for ReadPrefix and ReadHeader, so factor this
                -- code out into a common function.
                throwIO (Git.BackendError
                         "Could not find object in pack file")

    pokeByteString bytes (fromIntegral . getObjectLength -> len) = do
        content <- mallocBytes len
        BU.unsafeUseAsCString bytes $ flip (copyBytes content) len
        poke data_p (castPtr content)

    downloadLoose dets sha location = do
        result <- downloadFile dets sha
        case result of
            Just (len,typ,bytes) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                poke data_p (castPtr bytes)
                bs <- curry BU.unsafePackCStringLen bytes
                          (fromIntegral (getObjectLength len))
                when (isNothing location) $
                    wrapRegisterObject (registerObject (callbacks dets))
                        sha (Just (len, typ)) `orElse` return ()
                writeObjectToCache dets sha len typ bs
                return 0
            Nothing -> throwIO (Git.BackendError $
                                "Failed to download object " <> sha)

odbS3BackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
odbS3BackendReadPrefixCallback out_oid oid_p len_p type_p be oid len =
    -- jww (2013-04-22): Not yet implemented.
    wrap "odbS3BackendReadPrefixCallback" (return (-1)) (return (-1))

odbS3BackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
odbS3BackendReadHeaderCallback len_p type_p be oid = do
    str <- oidToStr oid
    wrap ("odbS3BackendReadHeaderCallback " ++ str) go (return (-1))
  where
    go = do
        (dets, oidStr, sha) <- unpackDetails be oid
        objs <- readMVar (knownObjects dets)
        let deb = M.lookup sha objs
        debug $ "odbS3BackendReadHeaderCallback lookup: " ++ show deb
        case M.lookup sha objs of
            Just DoesNotExist -> return c'GIT_ENOTFOUND

            Just (LooseCached _ len typ _) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                return 0
            Just (LooseRemoteMetaKnown len typ) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                return 0

            Just (PackedCached pack idx _) ->
                loadFromPack dets pack idx sha
            Just (PackedCachedMetaKnown len typ _) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                return 0
            Just (PackedRemote base) -> do
                (packPath, idxPath) <- downloadPack dets base
                loadFromPack dets packPath idxPath sha

            _ -> loadFromRemote dets sha loadFromPack $
                    downloadHeader dets sha

    loadFromPack dets pack idx sha = do
        result <- getObjectFromPack dets pack idx sha True
        case result of
            Just (len,typ,_) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                return 0
            Nothing -> throwIO (Git.BackendError
                                "Could not find object in pack file")

    downloadHeader dets sha location = do
        result <- doDownloadHeader dets sha
        case result of
            Just (len,typ) -> do
                poke len_p (toLength len)
                poke type_p (toType typ)
                when (isNothing location) $
                    wrapRegisterObject (registerObject (callbacks dets))
                        sha (Just (len, typ)) `orElse` return ()
                writeObjMetaDataToCache dets sha len typ
                return 0
            Nothing -> return c'GIT_ENOTFOUND

    doDownloadHeader dets sha = do
        bytes <- runResourceT $ do
            let hdrLen = sizeOf (undefined :: Int64) * 2
            result <- getFileS3 dets sha (Just (0,fromIntegral (hdrLen - 1)))
            result $$+- await
        return $ case bytes of
            Nothing -> Nothing
            Just bs ->
                bimap ObjectLength (ObjectType . fromIntegral)
                    <$> (Bin.decode (BL.fromChunks [bs]) :: Maybe (Int64,Int64))

writeObjMetaDataToCache :: OdbS3Details -> Text -> ObjectLength -> ObjectType
                        -> IO ()
writeObjMetaDataToCache dets sha len typ = do
    debug $ "writeObjMetaDataToCache " ++ show sha
    modifyMVar_ (knownObjects dets) $
        return . M.insert sha (LooseRemoteMetaKnown len typ)

odbS3BackendWriteCallback :: F'git_odb_backend_write_callback
odbS3BackendWriteCallback oid be obj_data len obj_type = do
    r <- c'git_odb_hash oid obj_data len obj_type
    case r of
        0 -> do
             str <- oidToStr oid
             wrap ("odbS3BackendWriteCallback " ++ str)
                  (go >> return 0)
                  (return (-1))
        n -> return n
  where
    go = do
        (dets, oidStr, sha) <- unpackDetails be oid
        let hdr = Bin.encode ((fromIntegral len,
                               fromIntegral obj_type) :: (Int64,Int64))
        bytes <- curry BU.unsafePackCStringLen
                      (castPtr obj_data) (fromIntegral len)
        let payload = BL.append hdr (BL.fromChunks [bytes])
        runResourceT $ putFileS3 dets sha (sourceLbs payload)

        wrapRegisterObject (registerObject (callbacks dets))
            sha (Just (fromLength len, fromType obj_type))
                `orElse` return ()

        -- Write a copy to the local cache
        writeObjectToCache dets sha (fromLength len)
            (fromType obj_type) bytes

odbS3BackendExistsCallback :: F'git_odb_backend_exists_callback
odbS3BackendExistsCallback be oid = do
    str <- oidToStr oid
    wrap ("odbS3BackendExistsCallback " ++ str) go (return (-1))
  where
    go = do
        (dets, oidStr, sha) <- unpackDetails be oid
        objs <- readMVar (knownObjects dets)
        let deb = M.lookup sha objs
        debug $ "odbS3BackendExistsCallback lookup: " ++ show deb
        case M.lookup sha objs of
            Just DoesNotExist -> return 0
            Just _            -> return 1
            Nothing -> do
                location <- wrapLookupObject (lookupObject (callbacks dets))
                                sha `orElse` return Nothing
                r <- case location of
                    Just (ObjectInPack base) -> return (PackedRemote base)
                    Just ObjectLoose         -> return LooseRemote
                    _ -> do
                        exists <- wrap "odbS3BackendExistsCallback failed"
                                      (runResourceT $ testFileS3 dets sha)
                                      (return False)
                        if exists
                            then do
                                wrapRegisterObject
                                    (registerObject (callbacks dets))
                                    sha Nothing `orElse` return ()
                                return LooseRemote
                            else return DoesNotExist

                putStrLn $ "Updating object status: " ++ show r
                modifyMVar_ (knownObjects dets) $ return . M.insert sha r
                return $ if r == DoesNotExist then 0 else 1

odbS3BackendRefreshCallback :: F'git_odb_backend_refresh_callback
odbS3BackendRefreshCallback _ = return 0 -- do nothing

odbS3BackendForeachCallback :: F'git_odb_backend_foreach_callback
odbS3BackendForeachCallback be callback payload =
    return (-1)                 -- fallback to the standard method

odbS3WritePackCallback :: F'git_odb_backend_writepack_callback
odbS3WritePackCallback writePackPtr be callback payload =
    wrap "odbS3WritePackCallback" go (return (-1))
  where
    go = do
        odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
        poke writePackPtr (packWriter odbS3)
        return 0

odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
odbS3BackendFreeCallback be = do
    debug "odbS3BackendFreeCallback"
    odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
    dets  <- liftIO $ deRefStablePtr (details odbS3)

    wrapShuttingDown (shuttingDown (callbacks dets)) `orElse` return ()

    exists <- isDirectory (tempDirectory dets)
    when exists $ removeTree (tempDirectory dets)

    backend <- peek be
    freeHaskellFunPtr (c'git_odb_backend'read backend)
    freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
    freeHaskellFunPtr (c'git_odb_backend'read_header backend)
    freeHaskellFunPtr (c'git_odb_backend'write backend)
    freeHaskellFunPtr (c'git_odb_backend'exists backend)

    free (packWriter odbS3)
    freeStablePtr (details odbS3)

foreign export ccall "odbS3BackendFreeCallback"
  odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
foreign import ccall "&odbS3BackendFreeCallback"
  odbS3BackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

observePackObjects :: OdbS3Details -> Text -> FilePath -> Bool -> Ptr C'git_odb
                   -> IO [Text]
observePackObjects dets packSha idxFile alsoWithRemote odbPtr = do
    debug $ "observePackObjects for " ++ show packSha

    -- Iterate the "database", which gives us a list of all the oids contained
    -- within it
    mshas <- newMVar []
    r <- flip (lgForEachObject odbPtr) nullPtr $ \oid _ -> do
        modifyMVar_ mshas $ \shas ->
            (:) <$> oidToSha oid <*> pure shas
        return 0
    checkResult r "lgForEachObject failed"

    -- Update the known objects map with the fact that we've got a local cache
    -- of the pack file.
    debug $ "observePackObjects: update known objects map"
    now  <- getCurrentTime
    shas <- readMVar mshas
    let obj = PackedCached (replaceExtension idxFile "pack") idxFile now
    modifyMVar_ (knownObjects dets) $ \objs ->
        return $ foldr (`M.insert` obj) objs shas

    debug $ "observePackObjects: pack file has been observed"
    return shas

odbS3WritePackAddCallback :: F'git_odb_writepack_add_callback
odbS3WritePackAddCallback wp bytes len progress =
    wrap "odbS3WritePackAddCallback" go (return (-1))
  where
    go = do
        be    <- c'git_odb_writepack'backend <$> peek wp
        odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
        dets  <- deRefStablePtr (details odbS3)
        let dir = tempDirectory dets

        bs <- curry BU.unsafePackCStringLen (castPtr bytes) (fromIntegral len)
        debug $ "odbS3WritePackAddCallback: building index for "
            ++ show (B.length bs) ++ " bytes"
        (packSha, packPath, idxPath) <- lgBuildPackIndex dir bs

        -- Upload the actual files to S3 if it's not already then, and then
        -- register the objects within the pack in the global index.
        packExists <- liftIO $ wrapLookupPackFile
                          (lookupPackFile (callbacks dets))
                          packSha `orElse` return Nothing
        case packExists of
            Just True -> return ()
            _ -> runResourceT $ uploadPackAndIndex dets packPath idxPath packSha
        return 0

odbS3WritePackCommitCallback :: F'git_odb_writepack_commit_callback
odbS3WritePackCommitCallback wp progress = return 0 -- do nothing

odbS3WritePackFreeCallback :: F'git_odb_writepack_free_callback
odbS3WritePackFreeCallback wp = do
    debug "odbS3WritePackFreeCallback"
    writepack <- peek wp
    freeHaskellFunPtr (c'git_odb_writepack'add writepack)
    freeHaskellFunPtr (c'git_odb_writepack'commit writepack)

foreign export ccall "odbS3WritePackFreeCallback"
  odbS3WritePackFreeCallback :: F'git_odb_writepack_free_callback
foreign import ccall "&odbS3WritePackFreeCallback"
  odbS3WritePackFreeCallbackPtr :: FunPtr F'git_odb_writepack_free_callback

odbS3Backend :: Git.MonadGit m
             => S3Configuration NormalQuery
             -> Configuration
             -> Manager
             -> Text
             -> Text
             -> FilePath
             -> BackendCallbacks
             -> m (Ptr C'git_odb_backend)
odbS3Backend s3config config manager bucket prefix dir callbacks = liftIO $ do
  readFun       <- mk'git_odb_backend_read_callback odbS3BackendReadCallback
  readPrefixFun <-
      mk'git_odb_backend_read_prefix_callback odbS3BackendReadPrefixCallback
  readHeaderFun <-
      mk'git_odb_backend_read_header_callback odbS3BackendReadHeaderCallback
  writeFun      <- mk'git_odb_backend_write_callback odbS3BackendWriteCallback
  existsFun     <- mk'git_odb_backend_exists_callback odbS3BackendExistsCallback
  refreshFun    <-
      mk'git_odb_backend_refresh_callback odbS3BackendRefreshCallback
  foreachFun    <-
      mk'git_odb_backend_foreach_callback odbS3BackendForeachCallback
  writepackFun  <-
      mk'git_odb_backend_writepack_callback odbS3WritePackCallback

  writePackAddFun  <-
      mk'git_odb_writepack_add_callback odbS3WritePackAddCallback
  writePackCommitFun  <-
      mk'git_odb_writepack_commit_callback odbS3WritePackCommitCallback

  objects   <- newMVar M.empty
  dirExists <- isDirectory dir
  unless dirExists $ createTree dir

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
          , c'git_odb_backend'free        = odbS3BackendFreeCallbackPtr
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
      , c'git_odb_writepack'free    = odbS3WritePackFreeCallbackPtr
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
            Just addr -> (Aws.s3 HTTP (E.encodeUtf8 addr) False) {
                               Aws.s3Port         = 10001
                             , Aws.s3RequestStyle = PathStyle })
        (Configuration Timestamp Credentials {
              accessKeyID     = E.encodeUtf8 access
            , secretAccessKey = E.encodeUtf8 secret }
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
    { objects :: MVar (HashMap (Text, Text) BL.ByteString)
    }

s3MockService :: IO S3MockService
s3MockService = S3MockService <$> newMVar M.empty

mockHeadObject :: MonadIO m
               => S3MockService -> Text -> Text -> ResourceT m (Maybe Bool)
mockHeadObject svc bucket path = do
    objs <- liftIO $ readMVar (objects svc)
    return $ maybe (Just False) (const (Just True)) $
        M.lookup (bucket, path) objs

mockGetObject :: MonadIO m
              => S3MockService -> Text -> Text -> Maybe (Int64, Int64)
              -> ResourceT m (Maybe (Either Text BL.ByteString))
mockGetObject svc bucket path range = do
    objs <- liftIO $ readMVar (objects svc)
    let obj = maybe (Left $ T.pack $ "Not found: "
                     ++ show bucket ++ "/" ++ show path)
                  Right $
                  M.lookup (bucket, path) objs
    return $ Just $ case range of
        Just (beg,end) -> BL.drop beg <$> BL.take end <$> obj
        Nothing -> obj

mockPutObject :: MonadIO m
              => S3MockService -> Text -> Text -> Int -> BL.ByteString
              -> ResourceT m (Maybe (Either Text ()))
mockPutObject svc bucket path _ bytes = do
    liftIO $ modifyMVar_ (objects svc) $
        return . M.insert (bucket, path) bytes
    return $ Just $ Right ()

-- S3.hs
