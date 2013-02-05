{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Git.GitHub where

import           Control.Applicative
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson hiding (Success)
import           Data.Attempt
import           Data.ByteString as B hiding (pack, putStrLn)
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as BC
import           Data.Conduit
import           Data.Default ( Default(..) )
import           Data.Foldable (for_)
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hex
import           Data.IORef
import           Data.Marshal.JSON ()
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text as T hiding (drop)
import qualified Git
import qualified Github.Repos as Github
import           Network.HTTP.Conduit hiding (Proxy, Response)
import           Network.REST.Client
import           System.IO.Unsafe
import           Text.Shakespeare.Text (st)
import Control.Concurrent

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

    parseOid x = Oid <$> unhex (BC.pack (T.unpack x))
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

instance MonadBase IO GitHubRepository where
    liftBase = liftIO

instance MonadUnsafeIO GitHubRepository where
    unsafeLiftIO = return . unsafePerformIO

instance MonadThrow GitHubRepository where
    -- monadThrow :: Exception e => e -> m a
    monadThrow = throw

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

ghRestfulEx :: (ToJSON a, FromJSON b) => Text -> Text -> a -> RESTful () -> GitHubRepository b
ghRestfulEx method url arg st = undefined

ghRestful :: (ToJSON a, FromJSON b) => Text -> Text -> a -> GitHubRepository b
ghRestful method url arg = do
    liftIO $ putStrLn "ghRestful.1"
    gh        <- ghGet
    liftIO $ putStrLn "ghRestful.2"
    urlPrefix <- ghPrefix
    liftIO $ putStrLn "ghRestful.3"
    let tok = gitHubToken gh
    liftIO $ putStrLn "ghRestful.4"
    result    <- liftIO $
                 catch (runResourceT $
                        withRestfulEnvAndMgr (fromJust (httpManager gh))
                        (for_ tok $ \t -> do
                              addHeader "Authorization" ("token " <> t)
                              addHeader "Content-type" "application/json")
                        (restfulJson arg [st|#{method} #{urlPrefix}/#{url}|]))
                       (\e -> do putStrLn $ "ghRestful Exception: " ++ show (e :: IOException)
                                 throwIO e)
    case result of
        Failure e -> liftIO $ putStrLn $ "ghRestful.5 FAILED: " ++ show e
        Success _ -> liftIO $ putStrLn $ "ghRestful.5 SUCCESS"
    attempt failure return result

ghLookupBlob :: BlobOid -> GitHubRepository Blob
ghLookupBlob oid = do
    -- jww (2013-01-12): Split out GET to its own argument, using StdMethod
    -- from http-types.  Also, use a type class for this argument, to be added
    -- to http-types:
    --     class IsHttpMethod a where asHttpMethod :: a -> ByteString
    -- jww (2012-12-26): Do we want runtime checking of the validity of the
    -- method?  Yes, but allow the user to declare it as OK.
    blob <- ghRestful "GET" ("git/blobs/" <> Git.renderOid oid) ()
    let content = ghBlobContent blob
    case ghBlobEncoding blob of
        "base64" ->
            case dec content of
                Right bs' -> return (Git.BlobString bs')
                Left str  -> failure (Git.TranslationException (T.pack str))
        "utf-8" -> return (Git.BlobString content)
        enc -> failure (Git.BlobEncodingUnknown enc)

  where dec = B64.decode . B.concat . B.split 10

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
ghCreateBlob (Git.BlobString content) = do
    liftIO $ putStrLn $ "ghCreateBlob.1: " ++ show (encode (Content (B64.encode content) "base64"))
    r <- Tagged <$>
        ghRestful "POST" "git/blobs" (Content (B64.encode content) "base64")
    liftIO $ putStrLn $ "ghCreateBlob.2: " ++ show r
    return r
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
ghLookupTree oid = undefined -- ghRestful "GET" ("git/trees/" <> Git.renderOid oid) ()

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
ghReadCommit oid = ghRestful "GET" ("git/commits/" <> Git.renderOid oid) ()

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
    -- ghRestfulEx "PATCH" ("git/" <> ref) sha
    --     $ addQueryParam "force" "true"
    return undefined

ghDeleteRef :: Text -> GitHubRepository ()
ghDeleteRef ref = ghRestful "DELETE" ("git/" <> ref) ref

data GitHubOwner = GitHubUser Text
                 | GitHubOrganization Text
                 deriving (Show, Eq)

data Repository = Repository
    { httpManager :: Maybe Manager
    , gitHubOwner :: GitHubOwner
    , gitHubRepo  :: Github.Repo
    , gitHubToken :: Maybe Text
    }

ghPrefix :: GitHubRepository Text
ghPrefix = do
    repo <- ghGet
    let owner = case gitHubOwner repo of
            GitHubUser name         -> name
            GitHubOrganization name -> name
        name  = Github.repoName (gitHubRepo repo)
    return [st|https://api.github.com/repos/#{owner}/#{name}|]

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

ghGet :: GitHubRepository Repository
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
withOpenGhRepository repo action = do
    putStrLn "withOpenGhRepository.1"
    r <- runReaderT (runGhRepository action) repo
    putStrLn "withOpenGhRepository.2"
    return r

withGitHubRepository :: GitHubOwner -> Text -> Maybe Text -> GitHubRepository a
                     -> IO (Either Github.Error a)
withGitHubRepository owner repoName token action = do
    putStrLn "withGitHubRepository.1"
    bracket
        (openOrCreateGhRepository owner repoName token)
        (\repo -> case repo of
              Left _ -> return ()
              Right _ -> do
                  putStrLn "withGitHubRepository.2"
                  let name = case owner of
                          GitHubUser n -> n
                          GitHubOrganization n -> n
                  putStrLn "withGitHubRepository.3"
                  when (isJust token) $ do
                      putStrLn "withGitHubRepository.4"
                      result <- Github.deleteRepo
                                (Github.GithubOAuth (T.unpack (fromJust token)))
                                (T.unpack name) (T.unpack repoName)
                      case result of
                          Left e -> putStrLn $ "Could not delete repository: "
                                           ++ show e
                          Right _ -> return ())
        (\repo -> case repo of
              Left e -> return (Left e)
              Right r -> Right <$> withOpenGhRepository r action)

openGhRepository :: GitHubOwner -> Github.Repo -> Maybe Text -> IO Repository
openGhRepository owner repo token = do
    mgr <- newManager def
    return Repository { httpManager = Just mgr
                      , gitHubOwner = owner
                      , gitHubRepo  = repo
                      , gitHubToken = token }

createGhRepository ::
    GitHubOwner -> Text -> Text -> IO (Either Github.Error Repository)
createGhRepository owner repoName token = do
    putStrLn "createGhRepository.1"
    result <- case owner of
        GitHubUser _ ->
            Github.createRepo (Github.GithubOAuth (T.unpack token))
                (Github.newRepo (T.unpack repoName))
                    { Github.newRepoHasIssues = Just False
                    , Github.newRepoAutoInit  = Just True }
        GitHubOrganization name ->
            Github.createOrganizationRepo
                (Github.GithubOAuth (T.unpack token)) (T.unpack name)
                (Github.newRepo (T.unpack repoName))
                    { Github.newRepoHasIssues = Just False
                    , Github.newRepoAutoInit  = Just True }
    liftIO $ threadDelay 10000000 -- wait ten seconds
    putStrLn "createGhRepository.2"
    case result of
        Left x -> do putStrLn "createGhRepository.3"
                     return (Left x)
        Right repo -> do
            putStrLn "createGhRepository.4"
            mgr <- newManager def
            return $ Right $ Repository
                { httpManager = Just mgr
                , gitHubOwner = owner
                , gitHubRepo  = repo
                , gitHubToken = Just token }

openOrCreateGhRepository ::
    GitHubOwner -> Text -> Maybe Text -> IO (Either Github.Error Repository)
openOrCreateGhRepository owner repoName token = do
    putStrLn "openOrCreateGhRepository.1"
    result <- case owner of
        GitHubUser name ->
            Github.userRepo (T.unpack name) (T.unpack repoName)
        GitHubOrganization name ->
            Github.organizationRepo (T.unpack name) (T.unpack repoName)
    putStrLn "openOrCreateGhRepository.2"
    case result of
        Left _   -> case token of
            Just tok -> do putStrLn "openOrCreateGhRepository.3"
                           createGhRepository owner repoName tok
            Nothing -> do putStrLn "openOrCreateGhRepository.4"
                          return (Left (Github.UserError
                                     "Authentication token not provided"))
        Right r' -> do putStrLn "openOrCreateGhRepository.5"
                       Right <$> openGhRepository owner r' token

-- GitHub.hs
