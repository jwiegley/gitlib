{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}

module Git.GitHub where

import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Attempt
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
import           Data.Hex
import           Data.IORef
import           Data.Int (Int64)
import qualified Data.List as L
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Marshal
import           Data.Marshal.JSON
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Stringable
import           Data.Tagged
import           Data.Text as T hiding (drop)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
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
import           Network.HTTP.Conduit hiding (Proxy, Response)
import           Network.REST.Client
import           Network.Socket
import           Prelude hiding (mapM_, catch)
import           System.Environment
import           System.IO.Unsafe
import           Text.Shakespeare.Text (st)

type Oid       = Git.Oid GitHubRepository

type BlobOid   = Git.BlobOid GitHubRepository
type TreeOid   = Git.TreeOid GitHubRepository
type CommitOid = Git.CommitOid GitHubRepository

type Blob      = Git.Blob GitHubRepository
type Tree      = Git.Tree GitHubRepository
type TreeEntry = Git.TreeEntry GitHubRepository
type Commit    = Git.Commit GitHubRepository

type TreeRef   = Git.TreeRef GitHubRepository
type CommitRef = Git.CommitRef GitHubRepository

type Reference = Git.Reference GitHubRepository Commit

instance Git.RepositoryBase GitHubRepository where
    data Oid GitHubRepository = Oid ByteString

    data Tree GitHubRepository = GitHubTree
        { ghTreeOid      :: IORef TreeOid
        , ghTreeContents :: IORef (HashMap Text TreeEntry)
        }

    data Commit GitHubRepository = GitHubCommit
        { ghCommitSha       :: Oid
        , ghCommitAuthor    :: GitHubSignature
        , ghCommitCommitter :: Maybe GitHubSignature
        , ghCommitMessage   :: Text
        , ghCommitEncoding  :: String
        , ghCommitTree      :: TreeRef
        , ghCommitParents   :: [CommitRef]
        }

    data Tag GitHubRepository = Tag
        { tagCommit :: CommitRef }

    parseOid x = Oid <$> unhex (T.encodeUtf8 x)
    renderOid (Tagged (Oid x)) = T.pack (hex (BC.unpack x))
    lookupRef    = undefined -- ghLookupRef
    updateRef    = undefined -- ghUpdateRef
    resolveRef   = undefined -- ghResolveRef
    allRefNames  = undefined -- ghAllRefNames
    lookupCommit = undefined -- ghLookupCommit 40
    lookupTree   = undefined -- ghLookupTree 40
    lookupBlob   = ghLookupBlob
    lookupTag    = undefined
    lookupObject = undefined -- ghLookupObject
    newTree      = undefined -- ghNewTree
    createBlob   = ghCreateBlob
    createCommit = undefined -- ghCreateCommit
    createTag    = undefined

data GitHubBlob = GitHubBlob
    { ghBlobContent  :: ByteString
    , ghBlobEncoding :: Text
    , ghBlobSha      :: Text
    , ghBlobSize     :: Int } deriving Show

instance Show (Git.Oid GitHubRepository) where
    show = T.unpack . Git.renderOid . Tagged

instance Ord (Git.Oid GitHubRepository) where
    compare (Oid l) (Oid r) = compare l r

instance Eq (Git.Oid GitHubRepository) where
    Oid l == Oid r = l == r

instance MonadBase IO GitHubRepository
instance MonadBaseControl IO GitHubRepository
instance MonadUnsafeIO GitHubRepository
instance MonadThrow GitHubRepository

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

ghRestfulEx method url arg st = undefined

ghRestful :: (ToJSON a, FromJSON b) => Text -> Text -> a -> GitHubRepository b
ghRestful method url arg = do
    gh        <- ghGet
    urlPrefix <- ghPrefix
    result    <- runResourceT $ withRestfulEnvAndMgr (httpManager gh)
                 (for_ (gitHubToken gh) $ \tok ->
                   addHeader "Authorization" ("token " <> tok))
                 (restfulJson arg [st|#{method} #{urlPrefix}/#{url}|])
    attempt failure return result

ghLookupBlob :: BlobOid -> GitHubRepository Blob
ghLookupBlob oid@(Tagged (Oid sha)) = do
    -- jww (2013-01-12): Split out GET to its own argument, using StdMethod
    -- from http-types.  Also, use a type class for this argument, to be added
    -- to http-types:
    --     class IsHttpMethod a where asHttpMethod :: a -> ByteString
    blob <- ghRestful "GET" ("git/blobs/" <> T.decodeUtf8 sha) ()

    case dec (ghBlobContent blob) of
        Right bs' -> return (Git.BlobString bs')
        Left str  -> failure (Git.TranslationException (T.pack str))
        _ -> failure Git.BlobLookupFailed

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

instance FromJSON (Git.Oid GitHubRepository) where
  parseJSON (Object v) = Oid <$> v .: "sha"
  parseJSON _ = mzero

instance ToJSON (Git.Oid GitHubRepository) where
  toJSON (Oid sha) = object ["sha" .= sha]

ghCreateBlob :: Git.BlobContents GitHubRepository -> GitHubRepository BlobOid
ghCreateBlob (Git.BlobString content) =
    Tagged <$>
        ghRestful "POST" "/git/blobs" (Content (B64.encode content) "base64")
ghCreateBlob _ = error "NYI"

-- instance FromJSON Tree where
--   parseJSON (Object v) = GitHubTree <$> v .: "sha"
--                                     <*> v .: "tree"
--   parseJSON _ = mzero

-- instance ToJSON Tree where
--   toJSON (GitHubTree oid contents) = unsafePerformIO $ do
--       Tagged oid@(Oid sha) <- readIORef oid
--       entries <- readIORef contents
--       if B.null sha
--           then return $ object ["tree" .= entries]
--           else return $ object ["sha"  .= Git.renderOid oid, "tree" .= entries]

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

ghLookupTree :: TreeOid -> GitHubRepository Tree
ghLookupTree (Tagged (Oid sha)) = undefined
    -- ghRestful "GET" ("git/trees/" <> T.decodeUtf8 sha) ()

ghWriteTree :: Tree -> GitHubRepository (Maybe TreeOid)
ghWriteTree tree = undefined -- ghRestful "POST" "git/trees"  tree

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

instance FromJSON Commit where
  parseJSON (Object v) =
      GitHubCommit <$> v .: "sha"
                   <*> v .: "author"
                   <*> v .:? "committer"
                   <*> v .: "message"
                   <*> v .: "encoding"
                   <*> (Git.ByOid . Tagged <$> v .: "tree")
                   <*> (fmap (Git.ByOid . Tagged) <$> v .: "parents")
  parseJSON _ = mzero

instance ToJSON Commit where
  toJSON c = object $ [ "sha"       .= ghCommitSha c
                      , "author"    .= ghCommitAuthor c
                      , "message"   .= ghCommitMessage c
                      , "encoding"  .= ghCommitEncoding c
                      , "tree"      .= ("tree" :: Text) -- ghCommitTree c
                      , "parents"   .= ("parents" :: Text) -- ghCommitParents c
                      ] <>
                      [ "committer" .= fromJust (ghCommitCommitter c) |
                                       isJust (ghCommitCommitter c) ]

ghReadCommit :: CommitOid -> GitHubRepository Commit
ghReadCommit (Tagged (Oid sha)) =
    -- jww (2012-12-26): Do we want runtime checking of the validity of the
    -- method?  Yes, but allow the user to declare it as OK.
    ghRestful "GET" ("git/commits/" <> T.decodeUtf8 sha) ()

ghWriteCommit :: Commit -> GitHubRepository Commit
ghWriteCommit commit = ghRestful "POST" "git/commits" commit

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
ghGetRef ref = undefined -- ghRestful "GET" ("git/" <> ref) ()

ghGetAllRefs :: Text -> GitHubRepository [Reference]
ghGetAllRefs namespace = undefined -- ghRestful "GET" ("git/" <> namespace) ()

ghCreateRef :: Reference -> GitHubRepository Reference
ghCreateRef ref = undefined -- ghRestful "POST" "git/refs" ref

ghUpdateRef :: Text -> CommitOid -> GitHubRepository Reference
ghUpdateRef ref sha = do
    -- jww (2013-01-12): restfulEx with a state argument is awkward.  Maybe
    -- have addQueryParam take a third parameter that modifies a RESTfulM's
    -- internal state value, and then do restful ... & addQueryParam, where &
    -- = flip ($)
    ghRestfulEx "PATCH" ("git/" <> ref) sha
        $ addQueryParam "force" "true"

ghDeleteRef :: Text -> GitHubRepository ()
ghDeleteRef ref = ghRestful "DELETE" ("git/" <> ref) ref

data Repository = Repository
    { httpManager :: Manager
    , ownerName   :: Text
    , gitHubRepo  :: Text
    , gitHubToken :: Maybe Text }

ghPrefix :: GitHubRepository Text
ghPrefix = do
    repo <- ghGet
    let owner    = ownerName repo
        repoName = gitHubRepo repo
        -- token = gitHubToken repo
    return [st|https://api.github.com/repos/#{owner}/#{repoName}|]

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

instance Exception e => Failure e GitHubRepository where
    failure = liftIO . throwIO

ghGet = GitHubRepository ask

instance Git.Treeish Tree where
    type TreeRepository = GitHubRepository
    modifyTree = undefined -- ghModifyTree
    writeTree  = undefined -- ghWriteTree

instance Git.Commitish Commit where
    type CommitRepository = GitHubRepository
    commitOid     = undefined -- fromJust . gitId . ghCommitInfo
    commitParents = undefined -- ghCommitParents
    commitTree    = undefined -- ghCommitTree

instance Git.Treeish Commit where
    type TreeRepository = GitHubRepository
    modifyTree c path createIfNotExist f =
        Git.commitTree' c >>= \t -> Git.modifyTree t path createIfNotExist f
    writeTree c = Git.commitTree' c >>= Git.writeTree

mapPair :: (a -> b) -> (a,a) -> (b,b)
mapPair f (x,y) = (f x, f y)

withOpenGhRepository :: Repository -> GitHubRepository a -> IO a
withOpenGhRepository repo action =
    runReaderT (runGhRepository action) repo

withGitHubRepository :: Text -> Text -> Text -> GitHubRepository a -> IO a
withGitHubRepository owner repoName token action = do
    repo <- openOrCreateGhRepository owner repoName token
    withOpenGhRepository repo action

openGhRepository :: Text -> Text -> Text -> IO Repository
openGhRepository owner repoName token = do
    mgr <- newManager def
    return $ Repository mgr owner repoName Nothing

createGhRepository :: Text -> Text -> Text -> IO Repository
createGhRepository owner repoName token = do
    mgr <- newManager def
    return $ Repository mgr owner repoName Nothing

openOrCreateGhRepository :: Text -> Text -> Text -> IO Repository
openOrCreateGhRepository owner repoName token = do
  p <- liftIO $ return undefined
  if p
    then openGhRepository owner repoName token
    else createGhRepository owner repoName token

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
