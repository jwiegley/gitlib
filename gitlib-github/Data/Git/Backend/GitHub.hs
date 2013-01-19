{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Data.Git.Backend.GitHub
       ( odbGitHubBackend
       , createGitHubBackend
       )
       where

import           Bindings.Libgit2.Odb
import           Bindings.Libgit2.OdbBackend
import           Bindings.Libgit2.Oid
import           Bindings.Libgit2.Refs
import           Bindings.Libgit2.Types
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Aeson hiding (Success)
import           Data.Attempt
import           Data.Binary
import           Data.ByteString as B hiding (pack, putStrLn)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import           Data.ByteString.Unsafe
import           Data.Conduit
import           Data.Conduit.Binary
import           Data.Conduit.List hiding (mapM_, foldM, peek, catMaybes,
                                           sequence)
import           Data.Default ( Default(..) )
import           Data.Git hiding (getObject)
import           Data.Git.Backend
import           Data.Git.Backend.Trace
import           Data.Git.Error
import           Data.Git.Oid
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.Map
import           Data.Marshal
import           Data.Marshal.JSON
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Stringable
import           Data.Text as T hiding (drop)
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
import           Network.HTTP.Conduit hiding (Proxy, Response)
import           Network.REST.Client
import           Network.Socket
import           Prelude hiding (mapM_, catch)
import           System.Environment
import           System.IO.Unsafe
import           Text.Shakespeare.Text ( st )

default (Text)

data GitHubBlob = GitHubBlob
    { ghBlobContent  :: ByteString
    , ghBlobEncoding :: Text
    , ghBlobSha      :: Text
    , ghBlobSize     :: Int } deriving Show

-- jww (2012-12-26): If no name mangling scheme is provided, assume it is
-- "type name prefix"
-- jww (2013-01-12): Look into using JsonGrammar to automate JSON encoding and
-- decoding: https://github.com/MedeaMelana/JsonGrammar
instance FromJSON GitHubBlob where
  parseJSON (Object v) = GitHubBlob <$> v .: "content"
                                    <*> v .: "encoding"
                                    <*> v .: "sha"
                                    <*> v .: "size"
  parseJSON _ = mzero

gitHubReadBlob :: Text -> Text -> Text -> RESTfulM (Attempt ByteString)
gitHubReadBlob owner repo sha = do
    -- jww (2013-01-12): Split out GET to its own argument, using StdMethod
    -- from http-types.  Also, use a type class for this argument, to be added
    -- to http-types:
    --     class IsHttpMethod a where asHttpMethod :: a -> ByteString
    blob <- restfulJson ()
        [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/blobs/#{sha}|]
    return $ case dec . ghBlobContent <$> blob of
        Success (Right bs') -> Success bs'
        Success (Left str)  -> Failure (TranslationException str)
        Failure e           -> Failure e
    -- jww (2012-12-26): Handle utf-8 and other encodings
  where dec = B64.decode . B.concat . B.split 10
    -- jww (2012-12-26): Need to add support for passing in a Maybe Text token
    -- in order to read from private repositories

data Content = Content { contentContent  :: ByteString
                       , contentEncoding :: Text } deriving Show

instance FromJSON Content where
  parseJSON (Object v) = Content <$> v .: "content"
                                 <*> v .: "encoding"
  parseJSON _ = mzero

instance ToJSON Content where
  toJSON (Content bs enc) = object ["content" .= bs, "encoding" .= enc]

instance Default Content where
  def = Content B.empty "utf-8"

data Sha = Sha { shaSha :: Text } deriving Show

instance FromJSON Sha where
  parseJSON (Object v) = Sha <$> v .: "sha"
  parseJSON _ = mzero

instance ToJSON Sha where
  toJSON (Sha sha) = object ["sha" .= sha]

gitHubWriteBlob :: Text -> Text -> ByteString -> RESTfulM (Attempt Sha)
gitHubWriteBlob owner repo content =
  restfulJson (Content (B64.encode content) "base64")
    [st|POST https://api.github.com/repos/#{owner}/#{repo}/git/blobs|]

data GitHubTree = GitHubTree { ghTreeSha  :: Text
                             , ghTreeTree :: [GitHubTreeEntry] }
                deriving Show

instance FromJSON GitHubTree where
  parseJSON (Object v) = GitHubTree <$> v .: "sha"
                                    <*> v .: "tree"
  parseJSON _ = mzero

instance ToJSON GitHubTree where
  toJSON (GitHubTree sha tree) = if T.null sha
                                 then object ["tree" .= tree]
                                 else object ["sha" .= sha, "tree" .= tree]

data GitHubTreeEntry = GitHubTreeEntry
    { ghTreeEntryType :: Text
    , ghTreeEntryPath :: Text
    , ghTreeEntryMode :: Text
    , ghTreeEntrySize :: Int
    , ghTreeEntrySha  :: Text } deriving Show

instance FromJSON GitHubTreeEntry where
  parseJSON (Object v) = GitHubTreeEntry <$> v .: "type"
                                         <*> v .: "path"
                                         <*> v .: "mode"
                                         <*> v .:? "size" .!= (-1)
                                         <*> v .: "sha"
  parseJSON _ = mzero

instance ToJSON GitHubTreeEntry where
  toJSON entry = object [ "type" .= ghTreeEntryType entry
                        , "path" .= ghTreeEntryPath entry
                        , "mode" .= ghTreeEntryMode entry
                        , "sha"  .= ghTreeEntrySha entry ]

gitHubReadTree :: Text -> Text -> Text -> RESTfulIO (Attempt GitHubTree)
gitHubReadTree owner repo sha =
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/trees/#{sha}|]

gitHubWriteTree :: Text -> Text -> GitHubTree -> RESTfulIO (Attempt GitHubTree)
gitHubWriteTree owner repo tree =
  restfulJson tree
    [st|POST https://api.github.com/repos/#{owner}/#{repo}/git/trees|]

data GitHubSignature = GitHubSignature
    { ghSignatureDate  :: Text
    , ghSignatureName  :: Text
    , ghSignatureEmail :: Text } deriving Show

instance FromJSON GitHubSignature where
  parseJSON (Object v) = GitHubSignature <$> v .: "date"
                                         <*> v .: "name"
                                         <*> v .: "email"
  parseJSON _ = mzero

instance ToJSON GitHubSignature where
  toJSON (GitHubSignature date name email) =
      object [ "date"  .= date
             , "name"  .= name
             , "email" .= email ]

data GitHubCommit = GitHubCommit
    { ghCommitSha       :: Text
    , ghCommitAuthor    :: GitHubSignature
    , ghCommitCommitter :: Maybe GitHubSignature
    , ghCommitMessage   :: Text
    , ghCommitTree      :: Sha
    , ghCommitParents   :: [Sha] } deriving Show

instance FromJSON GitHubCommit where
  parseJSON (Object v) = GitHubCommit <$> v .: "sha"
                                      <*> v .: "author"
                                      <*> v .:? "committer"
                                      <*> v .: "message"
                                      <*> v .: "tree"
                                      <*> v .: "parents"
  parseJSON _ = mzero

instance ToJSON GitHubCommit where
  toJSON c = object $ [ "sha"       .= ghCommitSha c
                      , "author"    .= ghCommitAuthor c
                      , "message"   .= ghCommitMessage c
                      , "tree"      .= ghCommitTree c
                      , "parents"   .= ghCommitParents c ] <>
                      [ "committer" .= fromJust (ghCommitCommitter c) |
                        isJust (ghCommitCommitter c) ]

gitHubReadCommit ::
    Text -> Text -> Text -> RESTfulEnvT (ResourceT IO) (Attempt GitHubCommit)
gitHubReadCommit owner repo sha =
  -- jww (2012-12-26): Do we want runtime checking of the validity of the
  -- method?  Yes, but allow the user to declare it as OK.
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/commits/#{sha}|]

gitHubWriteCommit :: Text -> Text -> GitHubCommit
                     -> RESTfulIO (Attempt GitHubCommit)
gitHubWriteCommit owner repo commit =
  restfulJson commit
    [st|POST https://api.github.com/repos/#{owner}/#{repo}/git/commits|]

data GitHubObjectRef = GitHubObjectRef
    { objectRefType :: Text
    , objectRefSha  :: Text } deriving Show

instance FromJSON GitHubObjectRef where
  parseJSON (Object v) = GitHubObjectRef <$> v .: "type"
                                         <*> v .: "sha"
  parseJSON _ = mzero

instance ToJSON GitHubObjectRef where
  toJSON c = object $ [ "type" .= objectRefType c
                      , "sha"  .= objectRefSha c ]

data GitHubReference = GitHubReference
    { referenceRef    :: Text
    , referenceObject :: GitHubObjectRef } deriving Show

instance FromJSON GitHubReference where
  parseJSON (Object v) = GitHubReference <$> v .: "ref"
                                         <*> v .: "object"
  parseJSON _ = mzero

instance ToJSON GitHubReference where
  toJSON c = object $ [ "ref"    .= referenceRef c
                      , "object" .= referenceObject c ]

gitHubGetRef :: Text -> Text -> Text -> RESTfulIO (Attempt GitHubReference)
gitHubGetRef owner repo ref =
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/#{ref}|]

gitHubGetAllRefs :: Text -> Text -> Text
                    -> RESTfulIO (Attempt [GitHubReference])
gitHubGetAllRefs owner repo namespace =
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/#{namespace}|]

gitHubCreateRef :: Text -> Text -> GitHubReference
                   -> RESTfulIO (Attempt GitHubReference)
gitHubCreateRef owner repo ref =
  restfulJson ref
    [st|POST https://api.github.com/repos/#{owner}/#{repo}/git/refs|]

gitHubUpdateRef :: Text -> Text -> Text -> Sha
                   -> RESTfulM (Attempt GitHubReference)
gitHubUpdateRef owner repo ref sha =
    -- jww (2013-01-12): restfulEx with a state argument is awkward.  Maybe
    -- have addQueryParam take a third parameter that modifies a RESTfulM's
    -- internal state value, and then do restful ... & addQueryParam, where &
    -- = flip ($)
    restfulJsonEx sha
        [st|PATCH https://api.github.com/repos/#{owner}/#{repo}/git/#{ref}|]
        $ addQueryParam "force" "true"

gitHubDeleteRef :: Text -> Text -> Text -> RESTfulM ()
gitHubDeleteRef owner repo ref =
  restfulJson_ ref
    [st|DELETE https://api.github.com/repos/#{owner}/#{repo}/git/#{ref}|]

data OdbGitHubBackend =
  OdbGitHubBackend { odbGitHubParent     :: C'git_odb_backend
                   , httpManager         :: StablePtr Manager
                   , bucketName          :: StablePtr Text
                   , objectPrefix        :: StablePtr Text }

instance Storable OdbGitHubBackend where
  sizeOf _ = sizeOf (undefined :: C'git_odb_backend) +
             sizeOf (undefined :: StablePtr Manager) +
             sizeOf (undefined :: StablePtr Text) +
             sizeOf (undefined :: StablePtr Text)
  alignment _ = alignment (undefined :: Ptr C'git_odb_backend)
  peek p = do
    v0 <- peekByteOff p 0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    v1 <- peekByteOff p sizev1
    let sizev2 = sizev1 + sizeOf (undefined :: StablePtr Manager)
    v2 <- peekByteOff p sizev2
    let sizev3 = sizev2 + sizeOf (undefined :: StablePtr Text)
    v3 <- peekByteOff p sizev3
    return (OdbGitHubBackend v0 v1 v2 v3)
  poke p (OdbGitHubBackend v0 v1 v2 v3) = do
    pokeByteOff p 0 v0
    let sizev1 = sizeOf (undefined :: C'git_odb_backend)
    pokeByteOff p sizev1 v1
    let sizev2 = sizev1 + sizeOf (undefined :: StablePtr Manager)
    pokeByteOff p sizev2 v2
    let sizev3 = sizev2 + sizeOf (undefined :: StablePtr Text)
    pokeByteOff p sizev3 v3
    return ()

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

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

odbGitHubBackendReadCallback :: F'git_odb_backend_read_callback
odbGitHubBackendReadCallback data_p len_p type_p be oid = undefined
  -- catch go (\e -> print (e :: IOException) >> return (-1))
  -- where
  --   go = do
  --     odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
  --     oidStr <- oidToStr oid
  --     blocks <- runResourceT $ do
  --       result <- getFileGitHub odbGitHub (T.pack oidStr) Nothing
  --       result $$+- consume
  --     case blocks of
  --       [] -> return (-1)
  --       bs -> do
  --         let hdrLen = sizeOf (undefined :: Int64) * 2
  --             (len,typ) =
  --                 mapPair fromIntegral $
  --                 (decode (BL.fromChunks [L.head bs]) :: (Int64,Int64))
  --         content <- mallocBytes len
  --         foldM (\offset x -> do
  --                 let xOffset = if offset == 0 then hdrLen else 0
  --                     innerLen = B.length x - xOffset
  --                 unsafeUseAsCString x $ \cstr ->
  --                     copyBytes (content `plusPtr` offset)
  --                               (cstr `plusPtr` xOffset) innerLen
  --                 return (offset + innerLen)) 0 bs
  --         poke len_p (fromIntegral len)
  --         poke type_p (fromIntegral typ)
  --         poke data_p (castPtr content)
  --         return 0

odbGitHubBackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
odbGitHubBackendReadPrefixCallback out_oid oid_p len_p type_p be oid len =
  return 0

odbGitHubBackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
odbGitHubBackendReadHeaderCallback len_p type_p be oid = undefined
  -- catch go (\e -> print (e :: IOException) >> return (-1))
  -- where
  --   go = do
  --     let hdrLen = sizeOf (undefined :: Int64) * 2
  --     odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
  --     oidStr <- oidToStr oid
  --     bytes  <- runResourceT $ do
  --       result <- getFileGitHub odbGitHub (T.pack oidStr) (Just (0,hdrLen - 1))
  --       result $$+- await
  --     case bytes of
  --       Nothing -> return (-1)
  --       Just bs -> do
  --         let (len,typ) = decode (BL.fromChunks [bs]) :: (Int64,Int64)
  --         poke len_p (fromIntegral len)
  --         poke type_p (fromIntegral typ)
  --         return 0

odbGitHubBackendWriteCallback :: F'git_odb_backend_write_callback
odbGitHubBackendWriteCallback oid be obj_data len obj_type = undefined -- do
  -- r <- c'git_odb_hash oid obj_data len obj_type
  -- case r of
  --   0 -> do
  --     oidStr <- oidToStr oid
  --     odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
  --     let hdr = encode ((fromIntegral len,
  --                        fromIntegral obj_type) :: (Int64,Int64))
  --     bytes <- curry unsafePackCStringLen
  --                   (castPtr obj_data) (fromIntegral len)
  --     let payload = BL.append hdr (BL.fromChunks [bytes])
  --     catch (go odbGitHub oidStr payload >> return 0)
  --           (\e -> print (e :: IOException) >> return (-1))
  --   n -> return n
  -- where
  --   go odbGitHub oidStr payload =
  --     runResourceT $ putFileGitHub odbGitHub (T.pack oidStr) (sourceLbs payload)

odbGitHubBackendExistsCallback :: F'git_odb_backend_exists_callback
odbGitHubBackendExistsCallback be oid = undefined -- do
  -- oidStr <- oidToStr oid
  -- odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
  -- exists <- catch (runResourceT $ testFileGitHub odbGitHub (T.pack oidStr))
  --                (\e -> print (e :: IOException) >> return False)
  -- return $ if exists then 1 else 0

odbGitHubBackendFreeCallback :: F'git_odb_backend_free_callback
odbGitHubBackendFreeCallback be = do
  backend <- peek be
  freeHaskellFunPtr (c'git_odb_backend'read backend)
  freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
  freeHaskellFunPtr (c'git_odb_backend'read_header backend)
  freeHaskellFunPtr (c'git_odb_backend'write backend)
  freeHaskellFunPtr (c'git_odb_backend'exists backend)

  odbGitHub <- peek (castPtr be :: Ptr OdbGitHubBackend)
  freeStablePtr (httpManager odbGitHub)
  freeStablePtr (bucketName odbGitHub)
  freeStablePtr (objectPrefix odbGitHub)

foreign export ccall "odbGitHubBackendFreeCallback"
  odbGitHubBackendFreeCallback :: F'git_odb_backend_free_callback
foreign import ccall "&odbGitHubBackendFreeCallback"
  odbGitHubBackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

odbGitHubBackend :: Manager -> Text -> Text
                 -> IO (Ptr C'git_odb_backend)
odbGitHubBackend manager bucket prefix = do
  readFun       <- mk'git_odb_backend_read_callback odbGitHubBackendReadCallback
  readPrefixFun <-
    mk'git_odb_backend_read_prefix_callback odbGitHubBackendReadPrefixCallback
  readHeaderFun <-
    mk'git_odb_backend_read_header_callback odbGitHubBackendReadHeaderCallback
  writeFun      <- mk'git_odb_backend_write_callback odbGitHubBackendWriteCallback
  existsFun     <- mk'git_odb_backend_exists_callback odbGitHubBackendExistsCallback

  manager'  <- newStablePtr manager
  bucket'   <- newStablePtr bucket
  prefix'   <- newStablePtr prefix

  castPtr <$> new OdbGitHubBackend {
    odbGitHubParent = C'git_odb_backend {
         c'git_odb_backend'odb         = nullPtr
       , c'git_odb_backend'read        = readFun
       , c'git_odb_backend'read_prefix = readPrefixFun
       , c'git_odb_backend'readstream  = nullFunPtr
       , c'git_odb_backend'read_header = readHeaderFun
       , c'git_odb_backend'write       = writeFun
       , c'git_odb_backend'writestream = nullFunPtr
       , c'git_odb_backend'exists      = existsFun
       , c'git_odb_backend'free        = odbGitHubBackendFreeCallbackPtr }
    , httpManager     = manager'
    , bucketName      = bucket'
    , objectPrefix    = prefix' }

createGitHubBackend :: Text -- ^ bucket
                    -> Text -- ^ prefix
                    -> Text -- ^ access key
                    -> Text -- ^ secret key
                    -> Maybe Manager
                    -> Maybe Text -- ^ mock address
                    -> Bool -- ^ tracing?
                    -> Repository
                    -> IO Repository
createGitHubBackend
    bucket prefix access secret mmanager mockAddr tracing repo = undefined -- do
    -- manager <- maybe (newManager def) return mmanager
    -- odbGitHub <-
    --   odbGitHubBackend
    --     (case mockAddr of
    --         Nothing   -> defServiceConfig
    --         Just addr -> (gitHub HTTP (E.encodeUtf8 addr) False) {
    --                            gitHubPort         = 10001
    --                          , gitHubRequestStyle = PathStyle })
    --     (Configuration Timestamp Credentials {
    --           accessKeyID     = E.encodeUtf8 access
    --         , secretAccessKey = E.encodeUtf8 secret }
    --      (defaultLog level))
    --     manager bucket prefix

    -- if tracing
    --   then do backend <- traceBackend odbGitHub
    --           odbBackendAdd repo backend 100
    --   else odbBackendAdd repo odbGitHub 100

    -- -- Start by reading in the known refs from GitHub
    -- mirrorRefsFromGitHub odbGitHub repo

    -- -- Whenever a ref is written, update the refs in GitHub
    -- let onWrite = const $ mirrorRefsToGitHub odbGitHub repo
    -- return repo { repoOnWriteRef = onWrite : repoOnWriteRef repo }

-- GitHub.hs
