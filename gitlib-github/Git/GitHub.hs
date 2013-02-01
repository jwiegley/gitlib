{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# OPTIONS_GHC -Wall #-}

module Git.GitHub
       ( withGitHubBackend
       ) where

import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson hiding (Success)
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
import           Data.Foldable (for_)
import qualified Git
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
import           Text.Shakespeare.Text (st)

type Reference = Git.Reference LgRepository Commit

instance Git.RepositoryBase GitHubRepository where
    data Oid GitHubRepository = Oid Text

    data Tree GitHubRepository = GitHubTree
        { ghTreeSha      :: IORef Oid
        , ghTreeContents :: IORef (HashMap Text TreeEntry)
        } deriving Show

    data Commit GitHubRepository = Commit
        { ghCommitSha       :: Oid
        , ghCommitAuthor    :: GitHubSignature
        , ghCommitCommitter :: Maybe GitHubSignature
        , ghCommitLog       :: Text
        , ghCommitEncoding  :: String
        , ghCommitTree      :: Git.ObjRef GitHubRepository Tree
        , ghCommitParents   :: [Git.ObjRef GitHubRepository Commit]
        } deriving Show

    data Tag GitHubRepository = Tag
        { tagCommit :: Git.ObjRef GitHubRepository Commit }

    parseOid     = ghParseOid
    renderOid    = ghRenderOid
    lookupRef    = ghLookupRef
    updateRef    = ghUpdateRef
    resolveRef   = ghResolveRef
    allRefNames  = ghAllRefNames
    lookupCommit = ghLookupCommit 40
    lookupTree   = ghLookupTree 40
    lookupBlob   = ghLookupBlob
    lookupTag    = undefined
    lookupObject = ghLookupObject
    newTree      = ghNewTree
    createBlob   = ghCreateBlob
    createCommit = ghCreateCommit
    createTag    = undefined

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

ghLookupBlob :: Git.BlobOid LgRepository
             -> GitHubRepository (Git.Blob GitHubRepository)
ghLookupBlob oid = do
    -- jww (2013-01-12): Split out GET to its own argument, using StdMethod
    -- from http-types.  Also, use a type class for this argument, to be added
    -- to http-types:
    --     class IsHttpMethod a where asHttpMethod :: a -> ByteString
    blob <- restfulJson ()
        [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/blobs/#{sha}|]
    return $ case dec . ghBlobContent <$> blob of
        Success (Right bs') -> (\x -> x { ghBlobContent = bs' }) <$> blob
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

instance FromJSON Oid where
  parseJSON (Object v) = Oid <$> v .: "sha"
  parseJSON _ = mzero

instance ToJSON Oid where
  toJSON (Oid sha) = object ["sha" .= sha]

ghWriteBlob :: ByteString -> GitHubRepository Sha
ghWriteBlob content =
  restfulJson (Content (B64.encode content) "base64")
    [st|POST https://api.github.com/repos/#{owner}/#{repo}/git/blobs|]

instance FromJSON Tree where
  parseJSON (Object v) = GitHubTree <$> v .: "sha"
                                    <*> v .: "tree"
  parseJSON _ = mzero

instance ToJSON Tree where
  toJSON (Tree sha tree) = if T.null sha
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

ghReadTree :: Text -> GitHubRepository Tree
ghReadTree sha =
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/trees/#{sha}|]

ghWriteTree :: GitHubTree -> GitHubRepository Tree
ghWriteTree tree =
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

ghReadCommit :: Text -> GitHubRepository Commit
ghReadCommit sha =
  -- jww (2012-12-26): Do we want runtime checking of the validity of the
  -- method?  Yes, but allow the user to declare it as OK.
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/commits/#{sha}|]

ghWriteCommit :: Commit -> GitHubRepository Commit
ghWriteCommit commit =
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

ghGetRef :: Text -> GitHubRepository Reference
ghGetRef ref =
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/#{ref}|]

ghGetAllRefs :: Text -> GitHubRepository [Reference]
ghGetAllRefs namespace =
  restfulJson ()
    [st|GET https://api.github.com/repos/#{owner}/#{repo}/git/#{namespace}|]

ghCreateRef :: Reference -> GitHubRepository Reference
ghCreateRef ref =
  restfulJson ref
    [st|POST https://api.github.com/repos/#{owner}/#{repo}/git/refs|]

ghUpdateRef :: Text -> Sha -> GitHubRepository Reference
ghUpdateRef ref sha =
    -- jww (2013-01-12): restfulEx with a state argument is awkward.  Maybe
    -- have addQueryParam take a third parameter that modifies a RESTfulM's
    -- internal state value, and then do restful ... & addQueryParam, where &
    -- = flip ($)
    restfulJsonEx sha
        [st|PATCH https://api.github.com/repos/#{owner}/#{repo}/git/#{ref}|]
        $ addQueryParam "force" "true"

ghDeleteRef :: Text -> RESTfulM ()
ghDeleteRef ref =
  restfulJson_ ref
    [st|DELETE https://api.github.com/repos/#{owner}/#{repo}/git/#{ref}|]

data Repository = Repository
    { httpManager :: Manager
    , ownerName   :: Text
    , gitHubRepo  :: Text
    , gitHubToken :: Maybe Text }

newtype GitHubRepository a = GitHubRepository
    { runGhRepository :: ReaderT Repository IO a }

instance Functor GitHubRepository where
    fmap f (GitHubRepository x) = GitHubRepository (fmap f x)

instance Applicative GitHubRepository where
    pure = GitHubRepository . pure
    GitHubRepository f <*> GitHubRepository x = GitHubRepository (f <*> x)

instance Monad GitHubRepository where
    return = GitHubRepository . return
    GitHubRepository m >>= f = GitHubRepository (m >>= runGhRepository . f)

instance MonadIO GitHubRepository where
    liftIO m = GitHubRepository (liftIO m)

instance Failure Git.Exception GitHubRepository where
    failure = liftIO . throwIO

ghGet = GitHubRepository ask

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

-- odbGitHubBackendReadCallback :: F'git_odb_backend_read_callback
-- odbGitHubBackendReadCallback data_p len_p type_p be oid =
--     catch go (\e -> print (e :: IOException) >> return (-1))
--   where
--     go = do
--         odbGitHub <- peek (castPtr be :: Ptr OdbGitHubBackend)
--         oidStr    <- oidToStr oid
--         manager   <- liftIO $ deRefStablePtr (httpManager odbGitHub)
--         owner     <- liftIO $ deRefStablePtr (ownerName odbGitHub)
--         repoName  <- liftIO $ deRefStablePtr (gitHubRepo odbGitHub)
--         token     <- liftIO $ deRefStablePtr (gitHubToken odbGitHub)
--         ghBlob    <- runResourceT $ withRestfulEnvAndMgr manager
--                          (for_ token $ \tok ->
--                                addHeader "Authorization" ("token " <> tok))
--                          (gitHubReadBlobName (pack oidStr))
--         case ghBlob of
--           Failure _ -> return (-1)
--           Success blob -> do
--               let len = ghBlobSize blob
--               content <- mallocBytes len
--               unsafeUseAsCString (ghBlobContent blob) $ \cstr ->
--                   copyBytes content cstr len
--               poke len_p (fromIntegral len)
--               poke type_p c'GIT_OBJ_BLOB
--               poke data_p (castPtr content)
--               return 0

-- odbGitHubBackendReadPrefixCallback :: F'git_odb_backend_read_prefix_callback
-- odbGitHubBackendReadPrefixCallback out_oid oid_p len_p type_p be oid len =
--   return 0

-- odbGitHubBackendReadHeaderCallback :: F'git_odb_backend_read_header_callback
-- odbGitHubBackendReadHeaderCallback len_p type_p be oid = undefined
--   -- catch go (\e -> print (e :: IOException) >> return (-1))
--   -- where
--   --   go = do
--   --     let hdrLen = sizeOf (undefined :: Int64) * 2
--   --     odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
--   --     oidStr <- oidToStr oid
--   --     bytes  <- runResourceT $ do
--   --       result <- getFileGitHub odbGitHub (T.pack oidStr) (Just (0,hdrLen - 1))
--   --       result $$+- await
--   --     case bytes of
--   --       Nothing -> return (-1)
--   --       Just bs -> do
--   --         let (len,typ) = decode (BL.fromChunks [bs]) :: (Int64,Int64)
--   --         poke len_p (fromIntegral len)
--   --         poke type_p (fromIntegral typ)
--   --         return 0

-- odbGitHubBackendWriteCallback :: F'git_odb_backend_write_callback
-- odbGitHubBackendWriteCallback oid be obj_data len obj_type = undefined -- do
--   -- r <- c'git_odb_hash oid obj_data len obj_type
--   -- case r of
--   --   0 -> do
--   --     oidStr <- oidToStr oid
--   --     odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
--   --     let hdr = encode ((fromIntegral len,
--   --                        fromIntegral obj_type) :: (Int64,Int64))
--   --     bytes <- curry unsafePackCStringLen
--   --                   (castPtr obj_data) (fromIntegral len)
--   --     let payload = BL.append hdr (BL.fromChunks [bytes])
--   --     catch (go odbGitHub oidStr payload >> return 0)
--   --           (\e -> print (e :: IOException) >> return (-1))
--   --   n -> return n
--   -- where
--   --   go odbGitHub oidStr payload =
--   --     runResourceT $ putFileGitHub odbGitHub (T.pack oidStr) (sourceLbs payload)

-- odbGitHubBackendExistsCallback :: F'git_odb_backend_exists_callback
-- odbGitHubBackendExistsCallback be oid = undefined -- do
--   -- oidStr <- oidToStr oid
--   -- odbGitHub  <- peek (castPtr be :: Ptr OdbGitHubBackend)
--   -- exists <- catch (runResourceT $ testFileGitHub odbGitHub (T.pack oidStr))
--   --                (\e -> print (e :: IOException) >> return False)
--   -- return $ if exists then 1 else 0

-- odbGitHubBackendFreeCallback :: F'git_odb_backend_free_callback
-- odbGitHubBackendFreeCallback be = do
--   backend <- peek be
--   freeHaskellFunPtr (c'git_odb_backend'read backend)
--   freeHaskellFunPtr (c'git_odb_backend'read_prefix backend)
--   freeHaskellFunPtr (c'git_odb_backend'read_header backend)
--   freeHaskellFunPtr (c'git_odb_backend'write backend)
--   freeHaskellFunPtr (c'git_odb_backend'exists backend)

--   odbGitHub <- peek (castPtr be :: Ptr OdbGitHubBackend)
--   freeStablePtr (httpManager odbGitHub)
--   freeStablePtr (ownerName odbGitHub)
--   freeStablePtr (gitHubRepo odbGitHub)
--   freeStablePtr (gitHubToken odbGitHub)

-- foreign export ccall "odbGitHubBackendFreeCallback"
--   odbGitHubBackendFreeCallback :: F'git_odb_backend_free_callback
-- foreign import ccall "&odbGitHubBackendFreeCallback"
--   odbGitHubBackendFreeCallbackPtr :: FunPtr F'git_odb_backend_free_callback

-- odbGitHubBackend :: Manager -> Maybe Text
--                  -> IO (Ptr C'git_odb_backend)
-- odbGitHubBackend managerName token = do
--   readFun <- mk'git_odb_backend_read_callback odbGitHubBackendReadCallback
--   readPrefixFun <-
--     mk'git_odb_backend_read_prefix_callback odbGitHubBackendReadPrefixCallback
--   readHeaderFun <-
--     mk'git_odb_backend_read_header_callback odbGitHubBackendReadHeaderCallback
--   writeFun  <- mk'git_odb_backend_write_callback odbGitHubBackendWriteCallback
--   existsFun <- mk'git_odb_backend_exists_callback odbGitHubBackendExistsCallback

--   manager'  <- newStablePtr manager
--   owner'    <- newStablePtr owner
--   repoName' <- newStablePtr repoName
--   token'    <- newStablePtr token

--   castPtr <$> new OdbGitHubBackend {
--     odbGitHubParent = C'git_odb_backend {
--          c'git_odb_backend'odb         = nullPtr
--        , c'git_odb_backend'read        = readFun
--        , c'git_odb_backend'read_prefix = readPrefixFun
--        , c'git_odb_backend'readstream  = nullFunPtr
--        , c'git_odb_backend'read_header = readHeaderFun
--        , c'git_odb_backend'write       = writeFun
--        , c'git_odb_backend'writestream = nullFunPtr
--        , c'git_odb_backend'exists      = existsFun
--        , c'git_odb_backend'free        = odbGitHubBackendFreeCallbackPtr }
--     , httpManager = manager'
--     , ownerName   = owner'
--     , gitHubRepo  = repoName'
--     , gitHubToken = token' }

-- createGitHubBackend :: Text       -- ^ owner
--                     -> Text       -- ^ repo
--                     -> Maybe Text -- ^ token
--                     -> Maybe Manager
--                     -> Bool       -- ^ tracing?
--                     -> S3Repository ()
-- createGitHubBackendName token mmanager tracing repo = do
--     manager <- maybe (newManager def) return mmanager
--     odbGitHub <-
--       odbGitHubBackend managerName token

--     if tracing
--       then do backend <- traceBackend odbGitHub
--               odbBackendAdd repo backend 100
--       else odbBackendAdd repo odbGitHub 100

--     -- Whenever a ref is written, update the refs in GitHub
--     let beforeRead = undefined
--         onWrite    = undefined
--     return repo { repoBeforeReadRef = beforeRead : repoBeforeReadRef repo
--                 , repoOnWriteRef    = onWrite : repoOnWriteRef repo }

-- GitHub.hs
