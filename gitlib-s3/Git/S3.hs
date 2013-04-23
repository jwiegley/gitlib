{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Git.S3
       ( s3Factory, odbS3Backend, addS3Backend
       , readRefs, writeRefs
       , mirrorRefsFromS3, mirrorRefsToS3
       , odbS3UploadPackAndIndex
       )
       where

import           Aws
import           Aws.Core
import           Aws.S3 hiding (bucketName)
import           Bindings.Libgit2.Indexer
import           Bindings.Libgit2.Odb
import           Bindings.Libgit2.OdbBackend
import           Bindings.Libgit2.Oid
import           Bindings.Libgit2.Refs
import           Bindings.Libgit2.Types
import           Control.Applicative
import           Control.Concurrent (threadDelay)
import           Control.Concurrent.MVar
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Resource
import           Control.Retry
import           Data.Aeson (object, (.=), (.:))
import           Data.Attempt
import           Data.Binary
import           Data.ByteString as B hiding (putStrLn)
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Unsafe as BU
import           Data.Conduit
import           Data.Conduit.Binary
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Foldable (for_)
import           Data.HashMap.Strict as M
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Stringable
import           Data.Tagged
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
import qualified Git
import           Git.Libgit2
import           Git.Libgit2.Backend
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Network.HTTP.Conduit hiding (Response)
import           Prelude hiding (mapM_, catch)
import           System.IO
import           System.IO.Temp
import           System.IO.Unsafe

data OdbS3Callbacks = OdbS3Callbacks
    { registerPackFile :: Text -> [Text] -> IO ()
      -- 'registerPackFile' takes the basename of a pack file, and a list of
      -- SHAs which are contained with the pack.  It must register this in an
      -- index, for the sake of the next function.

    , locateObject     :: Text -> IO (Maybe Text)
      -- 'locateObject' takes a SHA, and returns: Nothing if the object is
      -- "loose", or Just Text identifying the basename of the packfile that
      -- the object is located within.

    , updateReference :: Text -> Text -> IO ()
    , readReference   :: Text   -> Text -> IO Text
    }

instance Default OdbS3Callbacks where
    def = OdbS3Callbacks
        { registerPackFile = \_ _ -> return ()
        , locateObject     = \_   -> return Nothing
        }

data OdbS3Backend = OdbS3Backend
    { odbS3Parent     :: C'git_odb_backend
    , packWriter      :: Ptr C'git_odb_writepack
    , httpManager     :: StablePtr Manager
    , bucketName      :: StablePtr Text
    , objectPrefix    :: StablePtr Text
    , configuration   :: StablePtr Configuration
    , s3configuration :: StablePtr (S3Configuration NormalQuery)
    , callbacks       :: StablePtr OdbS3Callbacks
    }

instance Storable OdbS3Backend where
  sizeOf _ = sizeOf (undefined :: C'git_odb_backend) +
             sizeOf (undefined :: Ptr C'git_odb_writepack) +
             sizeOf (undefined :: StablePtr Manager) +
             sizeOf (undefined :: StablePtr Text) +
             sizeOf (undefined :: StablePtr Text) +
             sizeOf (undefined :: StablePtr Configuration) +
             sizeOf (undefined :: StablePtr (S3Configuration NormalQuery)) +
             sizeOf (undefined :: StablePtr OdbS3Callbacks)
  alignment _ = alignment (undefined :: Ptr C'git_odb_backend)
  peek p = do
    v0 <- peekByteOff p 0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    v1 <- peekByteOff p sizev1
    let sizev2 = sizev1 + sizeOf (undefined :: Ptr C'git_odb_writepack)
    v2 <- peekByteOff p sizev2
    let sizev3 = sizev2 + sizeOf (undefined :: StablePtr Manager)
    v3 <- peekByteOff p sizev3
    let sizev4 = sizev3 + sizeOf (undefined :: StablePtr Text)
    v4 <- peekByteOff p sizev4
    let sizev5 = sizev4 + sizeOf (undefined :: StablePtr Text)
    v5 <- peekByteOff p sizev5
    let sizev6 = sizev5 + sizeOf (undefined :: StablePtr Configuration)
    v6 <- peekByteOff p sizev6
    let sizev7 = sizev6
               + sizeOf (undefined :: StablePtr (S3Configuration NormalQuery))
    v7 <- peekByteOff p sizev7
    return (OdbS3Backend v0 v1 v2 v3 v4 v5 v6 v7)
  poke p (OdbS3Backend v0 v1 v2 v3 v4 v5 v6 v7) = do
    pokeByteOff p 0 v0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    pokeByteOff p sizev1 v1
    let sizev2 = sizev1 + sizeOf (undefined :: Ptr C'git_odb_writepack)
    pokeByteOff p sizev2 v2
    let sizev3 = sizev2 + sizeOf (undefined :: StablePtr Manager)
    pokeByteOff p sizev3 v3
    let sizev4 = sizev3 + sizeOf (undefined :: StablePtr Text)
    pokeByteOff p sizev4 v4
    let sizev5 = sizev4 + sizeOf (undefined :: StablePtr Text)
    pokeByteOff p sizev5 v5
    let sizev6 = sizev5 + sizeOf (undefined :: StablePtr Configuration)
    pokeByteOff p sizev6 v6
    let sizev7 = sizev6
               + sizeOf (undefined :: StablePtr (S3Configuration NormalQuery))
    pokeByteOff p sizev7 v7
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

awsRetry :: Transaction r a
         => Configuration
         -> ServiceConfiguration r NormalQuery
         -> Manager
         -> r
         -> ResourceT IO (Response (ResponseMetadata a) a)
awsRetry = ((((retrying def (isFailure . responseResult) .) .) .) .) aws

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
  res <- awsRetry config s3config manager
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
  lbs <- BL.fromChunks <$> (src $$ CList.consume)
  res <- awsRetry config s3config manager
             (putObject bucket (T.append prefix filepath)
                  (RequestBodyLBS lbs))
  _ <- readResponseIO res
  return lbs

putFileS3 :: OdbS3Backend -> Text -> Source (ResourceT IO) ByteString
             -> ResourceT IO BL.ByteString
putFileS3 = curry . odbS3dispatch putFileS3'

type RefMap m =
    M.HashMap Text (Maybe (Git.Reference (LgRepository m) (Commit m)))

instance Y.FromJSON (Git.Reference (LgRepository m) (Commit m)) where
    parseJSON j = do
        o <- Y.parseJSON j
        let lst = M.toList (o :: Y.Object)
        if isJust (L.lookup "symbolic" lst)
            then Git.Reference
                 <$> o .: "symbolic"
                 <*> (Git.RefSymbolic <$> o .: "target")
            else Git.Reference
                 <$> o .: "name"
                 <*> (Git.RefObj . Git.ByOid . go <$> o .: "target")
      where
        go oidStr =
            return . Oid $ unsafePerformIO $ do
              ptr <- mallocForeignPtr
              withCString oidStr $ \cstr ->
                withForeignPtr ptr $ \ptr' -> do
                  r <- c'git_oid_fromstr ptr' cstr
                  when (r < 0) $ throwIO Git.OidCopyFailed
                  return ptr

coidToJSON :: ForeignPtr C'git_oid -> Y.Value
coidToJSON coid = unsafePerformIO $ withForeignPtr coid $ \oid ->
                    Y.toJSON <$> oidToStr oid

instance Git.MonadGit m => Y.ToJSON (Git.Reference (LgRepository m) (Commit m)) where
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
  exists <- catch (runResourceT $ testFileS3 odbS3 "refs.yml")
                  (\e -> do putStrLn "Failed to check whether 'refs.yml' exists"
                            print (e :: SomeException)
                            throwIO e)
  if exists
    then do
    bytes  <- catch (runResourceT $ do
                        result <- getFileS3 odbS3 "refs.yml" Nothing
                        result $$+- await)
                    (\e -> do putStrLn "Failed to read 'refs.yml'"
                              print (e :: SomeException)
                              throwIO e)
    case bytes of
      Nothing     -> return Nothing
      Just bytes' -> return (Y.decode bytes' :: Maybe (RefMap m))

    else return Nothing

writeRefs :: Git.MonadGit m => Ptr C'git_odb_backend -> RefMap m -> IO ()
writeRefs be refs = do
  let payload = Y.encode refs
  odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
  void $ runResourceT $
    putFileS3 odbS3 "refs.yml" (sourceLbs (BL.fromChunks [payload]))

mirrorRefsFromS3 :: Git.MonadGit m => Ptr C'git_odb_backend -> LgRepository m ()
mirrorRefsFromS3 be = do
    repo <- lgGet
    refs <- liftIO $ readRefs be
    for_ refs $ \refs' ->
        forM_ (M.toList refs') $ \(name, ref) ->
            liftIO $ withForeignPtr (repoObj repo) $ \repoPtr ->
                withCStringable name $ \namePtr ->
                    alloca (go repoPtr namePtr ref)
  where
    go repoPtr namePtr ref ptr = do
        r <- case ref of
            Just (Git.Reference { Git.refTarget = Git.RefSymbolic target }) ->
              withCStringable target $ \targetPtr ->
                c'git_reference_symbolic_create ptr repoPtr namePtr
                                                targetPtr 1
            Just (Git.Reference {
                Git.refTarget = (Git.RefObj x@(Git.ByOid (Tagged coid))) }) ->
              withForeignPtr (getOid coid) $ \coidPtr ->
                c'git_reference_create ptr repoPtr namePtr coidPtr 1
            Nothing -> return 0
        when (r < 0) $ throwIO Git.RepositoryInvalid

mirrorRefsToS3 :: Git.MonadGit m => Ptr C'git_odb_backend -> LgRepository m ()
mirrorRefsToS3 be = do
    odbS3 <- liftIO $ peek (castPtr be :: Ptr OdbS3Backend)
    names <- Git.allRefNames
    refs  <- mapM Git.lookupRef names
    liftIO $ writeRefs be (fromList (L.zip names refs))
  where go name ref =
          case Git.refTarget ref of
            Git.RefSymbolic target     -> (name, Left target)
            Git.RefObj (Git.ByOid oid) -> (name, Right oid)

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

odbS3BackendReadCallback :: F'git_odb_backend_read_callback
odbS3BackendReadCallback data_p len_p type_p be oid = do
  catch go (\e -> do putStrLn "odbS3BackendReadCallback failed"
                     print (e :: SomeException)
                     return (-1))
  where
    go = do
      odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
      oidStr <- oidToStr oid
      blocks <- runResourceT $ do
        result <- getFileS3 odbS3 (T.pack oidStr) Nothing
        result $$+- CList.consume
      case blocks of
        [] -> return (-1)
        bs -> do
          let hdrLen = sizeOf (undefined :: Int64) * 2
              (len,typ) =
                  mapPair fromIntegral $
                  (decode (BL.fromChunks [L.head bs]) :: (Int64,Int64))
          content <- mallocBytes len
          foldM (\offset x -> do
                  let xOffset = if offset == 0 then hdrLen else 0
                      innerLen = B.length x - xOffset
                  BU.unsafeUseAsCString x $ \cstr ->
                      copyBytes (content `plusPtr` offset)
                                (cstr `plusPtr` xOffset) innerLen
                  return (offset + innerLen)) 0 bs
          poke len_p (fromIntegral len)
          poke type_p (fromIntegral typ)
          poke data_p (castPtr content)
          return 0

odbS3BackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
odbS3BackendReadPrefixCallback out_oid oid_p len_p type_p be oid len = do
  return (-1)                   -- jww (2013-04-22): NYI

odbS3BackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
odbS3BackendReadHeaderCallback len_p type_p be oid = do
  catch go (\e -> do putStrLn "odbS3BackendReadHeaderCallback failed"
                     print (e :: SomeException)
                     return (-1))
  where
    go = do
      let hdrLen = sizeOf (undefined :: Int64) * 2
      odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
      oidStr <- oidToStr oid
      bytes  <- runResourceT $ do
        result <- getFileS3 odbS3 (T.pack oidStr) (Just (0,hdrLen - 1))
        result $$+- await
      case bytes of
        Nothing -> return (-1)
        Just bs -> do
          let (len,typ) = decode (BL.fromChunks [bs]) :: (Int64,Int64)
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
      let hdr = encode ((fromIntegral len,
                         fromIntegral obj_type) :: (Int64,Int64))
      bytes <- curry BU.unsafePackCStringLen
                    (castPtr obj_data) (fromIntegral len)
      let payload = BL.append hdr (BL.fromChunks [bytes])
      catch (go odbS3 oidStr payload >> return 0)
            (\e -> do putStrLn "odbS3BackendWriteCallback failed"
                      print (e :: SomeException)
                      return (-1))
    n -> return n
  where
    go odbS3 oidStr payload =
      runResourceT $ putFileS3 odbS3 (T.pack oidStr) (sourceLbs payload)

odbS3BackendExistsCallback :: F'git_odb_backend_exists_callback
odbS3BackendExistsCallback be oid = do
  oidStr <- oidToStr oid
  odbS3  <- peek (castPtr be :: Ptr OdbS3Backend)
  exists <- catch (runResourceT $ testFileS3 odbS3 (T.pack oidStr))
                 (\e -> do putStrLn "odbS3BackendExistsCallback failed"
                           print (e :: SomeException)
                           return False)
  return $ if exists then 1 else 0

odbS3BackendRefreshCallback :: F'git_odb_backend_refresh_callback
odbS3BackendRefreshCallback _ = return 0 -- do nothing

odbS3BackendForeachCallback :: F'git_odb_backend_foreach_callback
odbS3BackendForeachCallback be callback payload = do
    return (-1)                 -- jww (2013-04-22): NYI

odbS3WritePackCallback :: F'git_odb_backend_writepack_callback
odbS3WritePackCallback writePackPtr be callback payload = do
    odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
    poke writePackPtr $ packWriter odbS3
    return 0

odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
odbS3BackendFreeCallback be = do
  backend <- peek be
  freeHaskellFunPtr (c'git_odb_backend'read backend)
  freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
  freeHaskellFunPtr (c'git_odb_backend'read_header backend)
  freeHaskellFunPtr (c'git_odb_backend'write backend)
  freeHaskellFunPtr (c'git_odb_backend'exists backend)

  odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
  free (packWriter odbS3)
  freeStablePtr (httpManager odbS3)
  freeStablePtr (bucketName odbS3)
  freeStablePtr (objectPrefix odbS3)
  freeStablePtr (configuration odbS3)
  freeStablePtr (s3configuration odbS3)
  freeStablePtr (callbacks odbS3)

foreign export ccall "odbS3BackendFreeCallback"
  odbS3BackendFreeCallback :: F'git_odb_backend_free_callback
foreign import ccall "&odbS3BackendFreeCallback"
  odbS3BackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

odbS3WritePackAddCallback :: F'git_odb_writepack_add_callback
odbS3WritePackAddCallback wp bytes len progress =
    withSystemTempDirectory "s3pack" $ \dir -> do
        be    <- c'git_odb_writepack'backend <$> peek wp
        odbS3 <- peek (castPtr be :: Ptr OdbS3Backend)
        receivePack odbS3 dir
  where
    receivePack odbS3 dir =
        withCString dir $ \dirStr ->
        alloca $ \odbPtrPtr ->
        alloca $ \backendPtrPtr ->
        alloca $ \idxPtrPtr ->
        alloca $ \statsPtr -> runResourceT $ do

        -- Allocate a new indexer stream
        r <- liftIO $ c'git_indexer_stream_new idxPtrPtr dirStr
                 nullFunPtr nullPtr
        checkResult r "c'git_indexer_stream_new failed"
        idxPtr <- liftIO $ peek idxPtrPtr
        register $ c'git_indexer_stream_free idxPtr

        -- Add the incoming packfile data to the stream
        r <- liftIO $ c'git_indexer_stream_add idxPtr bytes len statsPtr
        checkResult r "c'git_indexer_stream_add failed"

        -- Finalize the stream, which writes it out to disk
        r <- liftIO $ c'git_indexer_stream_finalize idxPtr statsPtr
        checkResult r "c'git_indexer_stream_finalize failed"

        -- Discover the hash used to identify the pack file
        oidPtr  <- liftIO $ c'git_indexer_stream_hash idxPtr
        packSha <- liftIO $ oidToSha oidPtr

        -- Create a temporary, in-memory object database
        r <- liftIO $ c'git_odb_new odbPtrPtr
        checkResult r "c'git_odb_new failed"
        odbPtr <- liftIO $ peek odbPtrPtr
        register $ c'git_odb_free odbPtr

        -- Load the pack file's index into a temporary objects into a
        -- database, so we can iterate the objects within it
        let basename = "pack-" <> T.unpack packSha <> ".idx"
            idxFile = dir <> "/" <> basename
        r <- liftIO $ withCString idxFile $ \idxFileStr ->
                 c'git_odb_backend_one_pack backendPtrPtr idxFileStr
        checkResult r "c'git_odb_backend_one_pack failed"
        backendPtr <- liftIO $ peek backendPtrPtr
        backend    <- liftIO $ peek backendPtr
        register $ mK'git_odb_backend_free_callback
            (c'git_odb_backend'free backend) backendPtr

        -- Associate the new backend containing our single index file with the
        -- in-memory object database
        r <- liftIO $ c'git_odb_add_backend odbPtr backendPtr 1
        checkResult r "c'git_odb_add_backend failed"

        -- Iterate the "database", which gives us a list of all the oids
        -- contained within it
        shas <- liftIO $ newMVar []
        foreachCallback <- liftIO $ mk'git_odb_foreach_cb $ \oid _ -> do
            modifyMVar_ shas $ \shas -> (:) <$> oidToSha oid <*> pure shas
            return 0
        r <- liftIO $ c'git_odb_foreach odbPtr foreachCallback nullPtr
        checkResult r "c'git_odb_add_foreach failed"

        -- Let whoever is listening know about this pack files and its
        -- contained objects
        cbs <- liftIO $ deRefStablePtr (callbacks odbS3)
        liftIO $ registerPackFile cbs packSha =<< readMVar shas

        -- Upload the actual files to S3
        uploadFile odbS3 dir packSha ".pack"
        uploadFile odbS3 dir packSha ".idx"
        return 0

    oidToSha oidPtr = allocaBytes 42 $ \oidStr ->
        E.decodeUtf8 <$> (B.packCString =<< c'git_oid_tostr oidStr 41 oidPtr)

    checkResult r why = when (r /= 0) $ failure (Git.BackendError why)

    uploadFile odbS3 dir sha ext =
        let basename = "pack-" <> T.unpack sha <> ext
            fullpath = dir <> "/" <> basename
        in liftIO (BL.readFile fullpath)
               >>= putFileS3 odbS3 (T.pack basename) . sourceLbs

odbS3UploadPackAndIndex :: Ptr OdbS3Backend -> Text -> Text -> Text -> IO ()
odbS3UploadPackAndIndex be dir packFile idxFile = do
    odbS3 <- peek be
    void $ runResourceT $ do
        uploadFile odbS3 packFile
        uploadFile odbS3 idxFile
  where
    uploadFile odbS3 file =
        let fullpath = dir <> "/" <> file
        in liftIO (BL.readFile (T.unpack fullpath))
               >>= putFileS3 odbS3 file . sourceLbs

odbS3WritePackCommitCallback :: F'git_odb_writepack_commit_callback
odbS3WritePackCommitCallback wp progress = return 0 -- do nothing

odbS3WritePackFreeCallback :: F'git_odb_writepack_free_callback
odbS3WritePackFreeCallback wp = do
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
             -> OdbS3Callbacks
             -> m (Ptr C'git_odb_backend)
odbS3Backend s3config config manager bucket prefix callbacks = liftIO $ do
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

  manager'   <- newStablePtr manager
  bucket'    <- newStablePtr bucket
  prefix'    <- newStablePtr prefix
  s3config'  <- newStablePtr s3config
  config'    <- newStablePtr config
  callbacks' <- newStablePtr callbacks

  let odbS3Parent = C'git_odb_backend
          { c'git_odb_backend'version     = fromIntegral 1
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
  ptr <- castPtr <$> new OdbS3Backend
      { odbS3Parent     = odbS3Parent
      , packWriter      = nullPtr
      , httpManager     = manager'
      , bucketName      = bucket'
      , objectPrefix    = prefix'
      , configuration   = config'
      , s3configuration = s3config'
      , callbacks       = callbacks'
      }
  packWriterPtr <- new C'git_odb_writepack
      { c'git_odb_writepack'backend = ptr
      , c'git_odb_writepack'add     = writePackAddFun
      , c'git_odb_writepack'commit  = writePackCommitFun
      , c'git_odb_writepack'free    = odbS3WritePackFreeCallbackPtr
      }
  pokeByteOff ptr (sizeOf (undefined :: C'git_odb_backend)) packWriterPtr

  return ptr

addS3Backend :: Git.MonadGit m
             => Repository
             -> Text -- ^ bucket
             -> Text -- ^ prefix
             -> Text -- ^ access key
             -> Text -- ^ secret key
             -> Maybe Manager
             -> Maybe Text -- ^ mock address
             -> LogLevel
             -> OdbS3Callbacks -- ^ callbacks
             -> m Repository
addS3Backend repo bucket prefix access secret
    mmanager mockAddr level callbacks = do
    manager <- maybe (liftIO $ newManager def) return mmanager
    odbS3   <- liftIO $ odbS3Backend
        (case mockAddr of
            Nothing   -> defServiceConfig
            Just addr -> (s3 HTTP (E.encodeUtf8 addr) False) {
                               s3Port         = 10001
                             , s3RequestStyle = PathStyle })
        (Configuration Timestamp Credentials {
              accessKeyID     = E.encodeUtf8 access
            , secretAccessKey = E.encodeUtf8 secret }
         (defaultLog level))
        manager bucket prefix callbacks
    liftIO $ odbBackendAdd repo odbS3 100
    return repo

s3Factory :: Git.MonadGit m
          => Maybe Text -> Text -> Text -> OdbS3Callbacks
          -> Git.RepositoryFactory LgRepository m Repository
s3Factory bucket accessKey secretKey callbacks = lgFactory
    { Git.runRepository = \ctxt -> runLgRepository ctxt . (s3back >>) }
  where
    s3back = do
        repo <- lgGet
        void $ liftIO $ addS3Backend
            repo (fromMaybe "test-bucket" bucket) ""
            accessKey secretKey Nothing
            (if isNothing bucket then Just "127.0.0.1" else Nothing)
            Error callbacks

-- S3.hs
