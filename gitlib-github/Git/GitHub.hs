{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.GitHub where

import           Control.Applicative
import           Control.Concurrent
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.Aeson hiding (Success)
import           Data.Attempt
import           Data.ByteString as B hiding (pack, putStrLn, map, null)
import qualified Data.ByteString.Base64 as B64
import           Data.Conduit
import           Data.Default ( Default(..) )
import           Data.Foldable (for_)
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.Hex
import           Data.IORef
import           Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text as T hiding (drop, map, null)
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTime)
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import qualified Git
import qualified Github.Repos as Github
import           Network.HTTP.Conduit hiding (Proxy, Response)
import           Network.REST.Client
import           Prelude hiding (FilePath)
import           System.IO.Unsafe
import           System.Locale (defaultTimeLocale)
import           Text.Shakespeare.Text (st)

type Oid m       = Git.Oid (GitHubRepository m)

type BlobOid m   = Git.BlobOid (GitHubRepository m)
type TreeOid m   = Git.TreeOid (GitHubRepository m)
type CommitOid m = Git.CommitOid (GitHubRepository m)

type Blob m      = Git.Blob (GitHubRepository m)
type Tree m      = Git.Tree (GitHubRepository m)
type TreeEntry m = Git.TreeEntry (GitHubRepository m)
type Commit m    = Git.Commit (GitHubRepository m)

type TreeRef m   = Git.TreeRef (GitHubRepository m)
type CommitRef m = Git.CommitRef (GitHubRepository m)

type Reference m = Git.Reference (GitHubRepository m) (Commit m)

data GitHubOptions = GitHubOptions
    { ghRepoOwner :: GitHubOwner
    , ghRepoName  :: Text
    , ghRepoToken :: Maybe Text
    } deriving (Show, Eq)

instance Git.MonadGit m => Git.RepositoryBase (GitHubRepository m) where
    data Oid (GitHubRepository m) = Oid { getOid :: ByteString }

    data Tree (GitHubRepository m) = GitHubTree
        { ghTreeOid      :: IORef (Maybe (TreeOid m))
        , ghTreeContents :: IORef (HashMap Text (TreeEntry m))
        }

    data Commit (GitHubRepository m) = GitHubCommit
        { ghCommitOid       :: Maybe (CommitOid m)
        , ghCommitAuthor    :: Git.Signature
        , ghCommitCommitter :: Maybe Git.Signature
        , ghCommitMessage   :: Text
        , ghCommitTree      :: TreeRef m
        , ghCommitParents   :: [CommitRef m]
        }

    data Tag (GitHubRepository m) = Tag { tagCommit :: CommitRef m }

    data Options (GitHubRepository m) = Options GitHubOptions

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = False }

    parseOid x = Oid <$> unhex (T.encodeUtf8 x)
    renderOid (Oid x) = T.toLower (T.decodeUtf8 (hex x))

    createRef        = ghCreateRef
    lookupRef        = ghLookupRef
    updateRef        = ghUpdateRef
    deleteRef        = ghDeleteRef
    allRefs          = ghAllRefs
    pushRef          = undefined -- ghPushRef
    lookupCommit     = ghLookupCommit
    lookupTree       = ghLookupTree
    lookupBlob       = ghLookupBlob
    lookupTag        = undefined -- ghLookupTag
    lookupObject     = undefined -- ghLookupObject
    existsObject     = undefined -- ghExistsObject
    newTree          = ghNewTree
    hashContents     = undefined        -- jww (2013-03-14): do it locally
    createBlob       = ghCreateBlob
    createCommit     = ghCreateCommit
    createTag        = undefined
    deleteRepository = ghDeleteRepository

data GitHubBlob = GitHubBlob
    { ghBlobContent  :: ByteString
    , ghBlobEncoding :: Text
    , ghBlobSha      :: Text
    , ghBlobSize     :: Int } deriving Show

instance Git.MonadGit m => Show (Git.Oid (GitHubRepository m)) where
    show = T.unpack . Git.renderOid

instance Ord (Git.Oid (GitHubRepository m)) where
    compare (Oid l) (Oid r) = compare l r

instance Eq (Git.Oid (GitHubRepository m)) where
    Oid l == Oid r = l == r

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

ghRestfulEx :: (ToJSON a, FromJSON b, Git.MonadGit m)
            => Text -> Text -> a -> RESTful ()
            -> GitHubRepository m b
ghRestfulEx method url arg act = do
    gh        <- ghGet
    urlPrefix <- ghPrefix
    let tok = ghRepoToken (gitHubOptions gh)
    result <- liftIO $ runResourceT $
              withRestfulEnvAndMgr (fromJust (httpManager gh))
              (for_ tok $ \t -> do
                    addHeader "Authorization" ("token " <> t)
                    addHeader "Content-type" "application/json")
              (restfulJsonEx arg [st|#{method} #{urlPrefix}/#{url}|] act)
    attempt (failure . Git.BackendError . T.pack . show) return result

ghRestful :: (ToJSON a, FromJSON b, Git.MonadGit m)
          => Text -> Text -> a -> GitHubRepository m b
ghRestful method url arg = do
    gh        <- ghGet
    urlPrefix <- ghPrefix
    let tok = ghRepoToken (gitHubOptions gh)
    result <- liftIO $ runResourceT $
              withRestfulEnvAndMgr (fromJust (httpManager gh))
              (for_ tok $ \t -> do
                    addHeader "Authorization" ("token " <> t)
                    addHeader "Content-type" "application/json")
              (restfulJson arg [st|#{method} #{urlPrefix}/#{url}|])
    attempt (failure . Git.BackendError . T.pack . show) return result

ghLookupBlob :: Git.MonadGit m => BlobOid m -> GitHubRepository m (Blob m)
ghLookupBlob oid = do
    -- jww (2013-01-12): Split out GET to its own argument, using StdMethod
    -- from http-types.  Also, use a type class for this argument, to be added
    -- to http-types:
    --     class IsHttpMethod a where asHttpMethod :: a -> ByteString
    -- jww (2012-12-26): Do we want runtime checking of the validity of the
    -- method?  Yes, but allow the user to declare it as OK.
    blob <- ghRestful "GET" ("git/blobs/" <> Git.renderObjOid oid) ()
    let content = ghBlobContent blob
    case ghBlobEncoding blob of
        "base64" ->
            case dec content of
                Right bs' -> return (Git.Blob oid (Git.BlobString bs'))
                Left str  -> failure (Git.TranslationException (T.pack str))
        "utf-8" -> return (Git.Blob oid (Git.BlobString content))
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

data GitHubOidProxy m = GitHubOidProxy
    { runGhpOid :: Oid m
    }

instance FromJSON (GitHubOidProxy m) where
  parseJSON (Object v) =
      GitHubOidProxy . Oid <$>
      (unsafePerformIO . unhex . T.encodeUtf8 <$> v .: "sha")
  parseJSON _ = mzero

instance ToJSON (GitHubOidProxy m) where
  toJSON (GitHubOidProxy (Oid sha)) = object ["sha" .= show sha]

textToOid :: Text -> Oid m
textToOid = Oid . unsafePerformIO . unhex . T.encodeUtf8

oidToText :: Git.MonadGit m => Oid m -> Text
oidToText = T.pack . show

ghCreateBlob :: Git.MonadGit m
             => Git.BlobContents (GitHubRepository m)
             -> GitHubRepository m (BlobOid m)
ghCreateBlob (Git.BlobString content) =
    Tagged . runGhpOid
        <$> ghRestful "POST" "git/blobs"
                      (Content (B64.encode content) "base64")
ghCreateBlob _ = error "NYI"    -- jww (2013-02-06): NYI

data GitHubTreeProxy m = GitHubTreeProxy
    { ghpTreeOid      :: Maybe Text
    , ghpTreeContents :: [GitHubTreeEntryProxy m]
    } deriving Show

instance FromJSON (GitHubTreeProxy m) where
  parseJSON (Object v) =
      -- jww (2013-02-06): The GitHub API supports using the "base_tree"
      -- parameter for doing incremental updates based on existing trees.
      -- This could be a huge efficiency gain, although it would only be an
      -- optimization, as we always know the full contents of every tree.
      GitHubTreeProxy <$> v .: "sha"
                      <*> v .: "tree"
  parseJSON _ = mzero

instance ToJSON (GitHubTreeProxy m) where
  toJSON (GitHubTreeProxy _ contents) = object [ "tree" .= contents ]

data GitHubTreeEntryProxy m = GitHubTreeEntryProxy
    { ghpTreeEntryType    :: Text
    , ghpTreeEntryPath    :: Text
    , ghpTreeEntryMode    :: Text
    , ghpTreeEntrySize    :: Int
    , ghpTreeEntrySha     :: Text
    , ghpTreeEntrySubtree :: Maybe (TreeRef m)
    }

instance Show (GitHubTreeEntryProxy m) where
    show x = Prelude.unlines
        [ "GitHubTreeEntryProxy {"
        , "  ghpTreeEntryType    = " ++ show (ghpTreeEntryType x)
        , "  ghpTreeEntryPath    = " ++ show (ghpTreeEntryPath x)
        , "  ghpTreeEntryMode    = " ++ show (ghpTreeEntryMode x)
        , "  ghpTreeEntrySize    = " ++ show (ghpTreeEntrySize x)
        , "  ghpTreeEntrySha     = " ++ show (ghpTreeEntrySha x)
        , "}"
        ]

treeEntryToProxy :: Git.MonadGit m
                 => Text -> TreeEntry m
                 -> GitHubRepository m (GitHubTreeEntryProxy m)
treeEntryToProxy name (Git.BlobEntry oid kind) =
    return GitHubTreeEntryProxy
        { ghpTreeEntryType    = "blob"
        , ghpTreeEntryPath    = name
        , ghpTreeEntryMode    = case kind of
            Git.PlainBlob      -> "100644"
            Git.ExecutableBlob -> "100755"
            Git.SymlinkBlob    -> "120000"
            Git.UnknownBlob    -> "100000"
        , ghpTreeEntrySize    = (-1)
        , ghpTreeEntrySha     = Git.renderObjOid oid
        , ghpTreeEntrySubtree = Nothing
        }

treeEntryToProxy name (Git.CommitEntry oid) =
    return GitHubTreeEntryProxy
        { ghpTreeEntryType    = "commit"
        , ghpTreeEntryPath    = name
        , ghpTreeEntryMode    = "160000"
        , ghpTreeEntrySize    = (-1)
        , ghpTreeEntrySha     = Git.renderObjOid oid
        , ghpTreeEntrySubtree = Nothing
        }

treeEntryToProxy name (Git.TreeEntry ref@(Git.ByOid oid)) =
    return GitHubTreeEntryProxy
        { ghpTreeEntryType    = "tree"
        , ghpTreeEntryPath    = name
        , ghpTreeEntryMode    = "040000"
        , ghpTreeEntrySize    = (-1)
        , ghpTreeEntrySha     = Git.renderObjOid oid
        , ghpTreeEntrySubtree = Just ref
        }

treeEntryToProxy name (Git.TreeEntry ref@(Git.Known tree)) = do
    oid <- Git.writeTree tree
    return GitHubTreeEntryProxy
        { ghpTreeEntryType    = "tree"
        , ghpTreeEntryPath    = name
        , ghpTreeEntryMode    = "040000"
        , ghpTreeEntrySize    = (-1)
        , ghpTreeEntrySha     = Git.renderObjOid oid
        , ghpTreeEntrySubtree = Just ref
        }

proxyToTreeEntry :: Git.MonadGit m
                 => GitHubTreeEntryProxy m -> GitHubRepository m (TreeEntry m)
proxyToTreeEntry entry@(GitHubTreeEntryProxy { ghpTreeEntryType = "blob" }) = do
    oid <- Git.parseOid (ghpTreeEntrySha entry)
    return $ Git.BlobEntry (Tagged oid) $
        case ghpTreeEntryMode entry of
            "100644" -> Git.PlainBlob
            "100755" -> Git.ExecutableBlob
            "120000" -> Git.SymlinkBlob
            _        -> Git.UnknownBlob

proxyToTreeEntry entry@(GitHubTreeEntryProxy { ghpTreeEntryType = "commit" }) = do
    oid <- Git.parseOid (ghpTreeEntrySha entry)
    return $ Git.CommitEntry  (Tagged oid)

proxyToTreeEntry entry@(GitHubTreeEntryProxy { ghpTreeEntryType = "tree" }) = do
    oid <- Git.parseOid (ghpTreeEntrySha entry)
    return $ Git.TreeEntry (Git.ByOid (Tagged oid))

proxyToTreeEntry _ = error "Unexpected tree entry type from GitHub"

instance FromJSON (GitHubTreeEntryProxy m) where
  parseJSON (Object v) =
      GitHubTreeEntryProxy <$> v .: "type"
                           <*> v .: "path"
                           <*> v .: "mode"
                           <*> v .:? "size" .!= (-1)
                           <*> v .: "sha"
                           <*> pure Nothing
  parseJSON _ = mzero

instance ToJSON (GitHubTreeEntryProxy m) where
  toJSON entry = object [ "type" .= ghpTreeEntryType entry
                        , "path" .= ghpTreeEntryPath entry
                        , "mode" .= ghpTreeEntryMode entry
                        , "sha"  .= ghpTreeEntrySha entry ]

ghNewTree :: Git.MonadGit m => GitHubRepository m (Tree m)
ghNewTree = GitHubTree <$> (liftIO $ newIORef Nothing)
                       <*> (liftIO $ newIORef HashMap.empty)

ghLookupTree :: Git.MonadGit m
             => TreeOid m -> GitHubRepository m (Tree m)
ghLookupTree oid = do
    treeProxy <- ghRestful "GET" ("git/trees/" <> Git.renderObjOid oid) ()
    oid' <- Git.parseOid (fromJust (ghpTreeOid treeProxy))
    subtree' <- subtree treeProxy
    GitHubTree <$> (liftIO $ newIORef (Just (Tagged oid')))
               <*> (liftIO $ newIORef subtree')
  where
    subtree tp =
        HashMap.fromList <$>
        mapM (\entry -> (,) <$> pure (ghpTreeEntryPath entry)
                           <*> proxyToTreeEntry entry)
             (ghpTreeContents tp)

doLookupTreeEntry :: Git.MonadGit m
                  => Tree m -> [Text]
                  -> GitHubRepository m (Maybe (TreeEntry m))
doLookupTreeEntry t [] = return (Just (Git.treeEntry t))
doLookupTreeEntry t (name:names) = do
  -- Lookup the current name in this tree.  If it doesn't exist, and there are
  -- more names in the path and 'createIfNotExist' is True, create a new Tree
  -- and descend into it.  Otherwise, if it exists we'll have @Just (TreeEntry
  -- {})@, and if not we'll have Nothing.

  y <- liftIO $ HashMap.lookup name <$> readIORef (ghTreeContents t)
  if null names
      then return y
      else case y of
      Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
      Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
      Just (Git.TreeEntry t')   -> do t'' <- Git.resolveTreeRef t'
                                      doLookupTreeEntry t'' names
      _ -> return Nothing

doModifyTree :: Git.MonadGit m
             => Tree m
             -> [Text]
             -> Bool
             -> (Maybe (TreeEntry m)
                 -> GitHubRepository m (Maybe (TreeEntry m)))
             -> GitHubRepository m (Maybe (TreeEntry m))
doModifyTree t [] _ _ = return . Just . Git.TreeEntry . Git.Known $ t
doModifyTree t (name:names) createIfNotExist f = do
    -- Lookup the current name in this tree.  If it doesn't exist, and there
    -- are more names in the path and 'createIfNotExist' is True, create a new
    -- Tree and descend into it.  Otherwise, if it exists we'll have @Just
    -- (TreeEntry {})@, and if not we'll have Nothing.
    y' <- doLookupTreeEntry t [name]
    y  <- if isNothing y' && createIfNotExist && not (null names)
          then Just . Git.TreeEntry . Git.Known <$> Git.newTree
          else return y'

    if null names
        then do
        -- If there are no further names in the path, call the transformer
        -- function, f.  It receives a @Maybe TreeEntry@ to indicate if there
        -- was a previous entry at this path.  It should return a 'Left' value
        -- to propagate out a user-defined error, or a @Maybe TreeEntry@ to
        -- indicate whether the entry at this path should be deleted or
        -- replaced with something new.
        --
        -- NOTE: There is no provision for leaving the entry unchanged!  It is
        -- assumed to always be changed, as we have no reliable method of
        -- testing object equality that is not O(n).
        ze <- f y
        liftIO $ modifyIORef (ghTreeContents t) $
            case ze of
                Nothing -> HashMap.delete name
                Just z' -> HashMap.insert name z'
        return ze

        else
        -- If there are further names in the path, descend them now.  If
        -- 'createIfNotExist' was False and there is no 'Tree' under the
        -- current name, or if we encountered a 'Blob' when a 'Tree' was
        -- required, throw an exception to avoid colliding with user-defined
        -- 'Left' values.
        case y of
            Nothing -> return Nothing
            Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
            Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
            Just (Git.TreeEntry st')  -> do
                st <- Git.resolveTreeRef st'
                ze <- doModifyTree st names createIfNotExist f
                liftIO $ do
                    modifyIORef (ghTreeOid t) (const Nothing)
                    stc <- readIORef (ghTreeContents st)
                    modifyIORef (ghTreeContents t) $
                        if HashMap.null stc
                        then HashMap.delete name
                        else HashMap.insert name (Git.treeEntry st)
                return ze

ghModifyTree :: Git.MonadGit m
             => Tree m -> FilePath -> Bool
             -> (Maybe (TreeEntry m)
                 -> GitHubRepository m (Maybe (TreeEntry m)))
             -> GitHubRepository m (Maybe (TreeEntry m))
ghModifyTree tree = doModifyTree tree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

ghWriteTree :: Git.MonadGit m => Tree m -> GitHubRepository m (TreeOid m)
ghWriteTree tree = do
    contents <- liftIO $ readIORef (ghTreeContents tree)
    if HashMap.size contents > 0
        then do
        contents'  <- HashMap.traverseWithKey treeEntryToProxy contents
        treeProxy' <- ghRestful "POST" "git/trees"
                      (GitHubTreeProxy Nothing (HashMap.elems contents'))
        oid <- Git.parseOid (fromJust (ghpTreeOid treeProxy'))
        return (Tagged oid)

        else failure (Git.TreeCreateFailed "Attempt to create an empty tree")

ghTraverseEntries :: Git.MonadGit m
                  => Tree m
                  -> (FilePath -> TreeEntry m -> GitHubRepository m b)
                  -> GitHubRepository m [b]
ghTraverseEntries tree f = do
    oid <- Git.writeTree tree
    treeProxy <- ghRestful "GET" ("git/trees/" <> Git.renderObjOid oid
                                              <> "?recursive=1") ()
    mapM (\entry -> f (F.fromText (ghpTreeEntryPath entry))
                        =<< proxyToTreeEntry entry)
         (ghpTreeContents treeProxy)

-- data GitHubSignature = GitHubSignature
--     { ghSignatureDate  :: Text
--     , ghSignatureName  :: Text
--     , ghSignatureEmail :: Text } deriving Show

-- parseGhTime :: Text -> UTCTime
-- parseGhTime = fromJust . parseTime defaultTimeLocale "%Y-%m-%dT%H%M%S%z"
--               . T.unpack . T.filter (/= ':')

-- formatGhTime :: UTCTime -> Text
-- formatGhTime t =
--     let fmt   = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S%z" t
--         (b,a) = L.splitAt (L.length fmt - 2) fmt
--     in T.pack (b ++ ":" ++ a)
parseGhTime :: Text -> UTCTime
parseGhTime =
    fromJust . parseTime defaultTimeLocale "%Y-%m-%dT%H%M%SZ" . T.unpack

formatGhTime :: UTCTime -> Text
formatGhTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

instance FromJSON Git.Signature where
  parseJSON (Object v) = Git.Signature <$> v .: "name"
                                       <*> v .: "email"
                                       <*> (parseGhTime <$> v .: "date")
  parseJSON _ = mzero

instance ToJSON Git.Signature where
  toJSON (Git.Signature name email date) =
      object [ "name"  .= name
             , "email" .= email
             , "date"  .= formatGhTime date ]

data GitHubCommitProxy m = GitHubCommitProxy
    { ghpCommitOid       :: Text
    , ghpCommitAuthor    :: Git.Signature
    , ghpCommitCommitter :: Maybe Git.Signature
    , ghpCommitMessage   :: Text
    , ghpCommitTree      :: GitHubOidProxy m
    , ghpCommitParents   :: [GitHubOidProxy m]
    }

-- The strange thing about commits is that converting them to JSON does not use
-- the "sha" key for the trees and parents:
--
--   { "parents": ["7d1b31e74ee336d15cbd21741bc88a537ed063a0"],
--      "tree": "827efc6d56897b048c772eb4087f854f46256132" }
--
-- But when converting from JSON, it does:
--
-- { "tree": { "sha": "827efc6d56897b048c772eb4087f854f46256132" },
--   "parents": [
--     { "sha": "7d1b31e74ee336d15cbd21741bc88a537ed063a0" }
--   ] }
instance FromJSON (GitHubCommitProxy m) where
  parseJSON (Object v) =
      GitHubCommitProxy <$> v .: "sha"
                        <*> v .: "author"
                        <*> v .:? "committer"
                        <*> v .: "message"
                        <*> v .: "tree"
                        <*> v .: "parents"
  parseJSON _ = mzero

instance Git.MonadGit m => ToJSON (GitHubCommitProxy m) where
  toJSON c = object $ [ "author"    .= ghpCommitAuthor c
                      , "message"   .= ghpCommitMessage c
                      , "tree"      .= oidToText (runGhpOid (ghpCommitTree c))
                      , "parents"   .= map (oidToText . runGhpOid)
                                           (ghpCommitParents c)
                      ] <>
                      [ "committer" .= fromJust (ghpCommitCommitter c) |
                                       isJust (ghpCommitCommitter c) ]

proxyToCommit :: GitHubCommitProxy m -> Commit m
proxyToCommit cp = GitHubCommit
    { ghCommitOid       = Just (Tagged (textToOid (ghpCommitOid cp)))
    , ghCommitAuthor    = ghpCommitAuthor cp
    , ghCommitCommitter = ghpCommitCommitter cp
    , ghCommitMessage   = ghpCommitMessage cp
    , ghCommitTree      = Git.ByOid (Tagged (runGhpOid (ghpCommitTree cp)))
    , ghCommitParents   = map (Git.ByOid . Tagged . runGhpOid)
                              (ghpCommitParents cp)
    }

ghLookupCommit :: Git.MonadGit m => CommitOid m -> GitHubRepository m (Commit m)
ghLookupCommit oid = do
    cp <- ghRestful "GET" ("git/commits/" <> Git.renderObjOid oid) ()
    return (proxyToCommit cp)

ghCreateCommit :: Git.MonadGit m
               => [CommitRef m] -> TreeRef m
               -> Git.Signature -> Git.Signature -> Text -> Maybe Text
               -> GitHubRepository m (Commit m)
ghCreateCommit parents tree author committer message ref = do
    treeOid <- Git.treeRefOid tree
    commit' <- ghRestful "POST" "git/commits" $ GitHubCommitProxy
                { ghpCommitOid       = ""
                , ghpCommitAuthor    = author
                , ghpCommitCommitter = Just committer
                , ghpCommitMessage   = message
                , ghpCommitTree      = GitHubOidProxy (unTagged treeOid)
                , ghpCommitParents   =
                    map (GitHubOidProxy . unTagged . Git.commitRefOid) parents
                }

    let commit = proxyToCommit commit'
    when (isJust ref) $
        void (ghUpdateRef (fromJust ref)
              (Git.RefObj (Git.ByOid (fromJust (ghCommitOid commit)))))

    return commit

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
    { referenceName   :: Text
    , referenceObject :: GitHubObjectRef } deriving Show

instance FromJSON GitHubReference where
  parseJSON (Object v) = GitHubReference <$> v .: "ref"
                                         <*> v .: "object"
  parseJSON _ = mzero

instance ToJSON GitHubReference where
  toJSON c = object $ [ "ref"    .= referenceName c
                      , "object" .= referenceObject c ]

data GitHubDirectRef = GitHubDirectRef
    { directRefName  :: Maybe Text
    , directRefSha   :: Text
    , directRefForce :: Maybe Bool
    } deriving Show

instance FromJSON GitHubDirectRef where
  parseJSON (Object v) = GitHubDirectRef <$> v .:? "ref"
                                         <*> v .: "sha"
                                         <*> v .:? "force"
  parseJSON _ = mzero

instance ToJSON GitHubDirectRef where
  toJSON c = object $ [ "ref"   .= directRefName c
                      , "sha"   .= directRefSha c
                      , "force" .= directRefForce c ]

ghRefToReference :: Git.MonadGit m
                 => GitHubReference -> GitHubRepository m (Reference m)
ghRefToReference ref = do
    oid <- Git.parseOid (objectRefSha (referenceObject ref))
    return (Git.Reference (referenceName ref)
                          (Git.RefObj (Git.ByOid (Tagged oid))))

ghLookupRef :: Git.MonadGit m
            => Text -> GitHubRepository m (Maybe (Reference m))
ghLookupRef refName =
    -- jww (2013-02-14): Need to test whether or not the ref exists
    Just <$> (ghRefToReference =<< ghRestful "GET" ("git/" <> refName) ())

ghAllRefs :: Git.MonadGit m
          => GitHubRepository m [Reference m]
ghAllRefs =
    mapM ghRefToReference =<< ghRestful "GET" "git/refs" ()

ghCreateRef :: Git.MonadGit m
            => Text -> Git.RefTarget (GitHubRepository m) (Commit m)
            -> GitHubRepository m (Reference m)
ghCreateRef refName (Git.RefObj commitRef) = do
    let oid = Git.commitRefOid commitRef
    ghRefToReference
        =<< ghRestful "POST" "git/refs"
                     (GitHubDirectRef (Just refName) (Git.renderObjOid oid)
                                      Nothing)

ghCreateRef _ (Git.RefSymbolic _) =
    error "Not supported"

ghUpdateRef :: Git.MonadGit m
            => Text -> Git.RefTarget (GitHubRepository m) (Commit m)
            -> GitHubRepository m (Reference m)
ghUpdateRef refName (Git.RefObj commitRef) = do
    let oid = Git.commitRefOid commitRef
    ghRefToReference =<<
        (ghRestful "PATCH" ("git/" <> refName)
                   (GitHubDirectRef Nothing (Git.renderObjOid oid) (Just True)))

ghUpdateRef _ (Git.RefSymbolic _) =
    error "Not supported"

ghDeleteRef :: Git.MonadGit m => Text -> GitHubRepository m ()
ghDeleteRef ref = ghRestful "DELETE" ("git/" <> ref) ref

data GitHubOwner = GitHubUser Text
                 | GitHubOrganization Text
                 deriving (Show, Eq)

data Repository = Repository
    { httpManager   :: Maybe Manager
    , gitHubRepo    :: Github.Repo
    , gitHubOptions :: GitHubOptions
    }

ghPrefix :: Git.MonadGit m => GitHubRepository m Text
ghPrefix = do
    repo <- ghGet
    let owner = case ghRepoOwner (gitHubOptions repo) of
            GitHubUser name         -> name
            GitHubOrganization name -> name
        name  = Github.repoName (gitHubRepo repo)
    return [st|https://api.github.com/repos/#{owner}/#{name}|]

newtype GitHubRepository m a = GitHubRepository
    { ghRepositoryReaderT :: ReaderT Repository m a }

instance Functor m => Functor (GitHubRepository m) where
    fmap f (GitHubRepository x) = GitHubRepository (fmap f x)

instance Applicative m => Applicative (GitHubRepository m) where
    pure = GitHubRepository . pure
    GitHubRepository f <*> GitHubRepository x = GitHubRepository (f <*> x)

instance Monad m => Monad (GitHubRepository m) where
    return = GitHubRepository . return
    GitHubRepository m >>= f = GitHubRepository (m >>= ghRepositoryReaderT . f)

instance MonadIO m => MonadIO (GitHubRepository m) where
    liftIO m = GitHubRepository (liftIO m)

instance (Monad m, MonadIO m, Applicative m)
         => MonadBase IO (GitHubRepository m) where
    liftBase = liftIO

instance Monad m => MonadUnsafeIO (GitHubRepository m) where
    unsafeLiftIO = return . unsafePerformIO

instance Monad m => MonadThrow (GitHubRepository m) where
    -- monadThrow :: Exception e => e -> m a
    monadThrow = throw

instance MonadTrans GitHubRepository where
    lift = GitHubRepository . ReaderT . const

ghGet :: Git.MonadGit m => GitHubRepository m Repository
ghGet = GitHubRepository ask

instance Git.MonadGit m => Git.Treeish (Tree m) where
    type TreeRepository (Tree m) = GitHubRepository m
    modifyTree      = ghModifyTree
    writeTree       = ghWriteTree
    traverseEntries = ghTraverseEntries

instance Git.MonadGit m => Git.Commitish (Commit m) where
    type CommitRepository (Commit m) = GitHubRepository m
    commitOid       = fromJust . ghCommitOid
    commitParents   = ghCommitParents
    commitTree      = ghCommitTree
    commitAuthor    = ghCommitAuthor
    commitCommitter = \c -> fromMaybe (ghCommitAuthor c) (ghCommitCommitter c)
    commitLog       = ghCommitMessage
    commitEncoding  = const "utf-8"

instance Git.MonadGit m => Git.Treeish (Commit m) where
    type TreeRepository (Commit m) = GitHubRepository m

    modifyTree      = Git.defaultCommitModifyTree
    writeTree       = Git.defaultCommitWriteTree
    traverseEntries = Git.defaultCommitTraverseEntries

-- dropRepository :: GitHubOwner -> Text -> Maybe Text -> GitHubRepository m a
--                -> IO (Either Github.Error a)
-- dropRepository owner repoName token action =
--     let repoName' = if ".git" `T.isSuffixOf` repoName
--                     then T.take (T.length repoName - 4) repoName
--                     else repoName
--     in bracket
--        (openOrCreateGhRepository owner repoName' token)
--        (\repo -> case repo of
--              Left _ -> return ()
--              Right _ -> when (isJust token) $ do
--              let name = case owner of
--                      GitHubUser n -> n
--                      GitHubOrganization n -> n
--              _ <- Github.deleteRepo
--                   (Github.GithubOAuth (T.unpack (fromJust token)))
--                   (T.unpack name) (T.unpack repoName')
--              return ())
--        (\repo -> case repo of
--              Left e -> return (Left e)
--              Right r -> Right <$> withOpenGhRepository r action)

ghDeleteRepository :: Git.MonadGit m => GitHubRepository m ()
ghDeleteRepository = do
    repo <- ghGet
    let ghOpts    = gitHubOptions repo
        owner     = ghRepoOwner ghOpts
        repoName  = ghRepoName ghOpts
        token     = ghRepoToken ghOpts
        repoName' = if ".git" `T.isSuffixOf` repoName
                    then T.take (T.length repoName - 4) repoName
                    else repoName
        name = case owner of
            GitHubUser n -> n
            GitHubOrganization n -> n
    result <- liftIO $ Github.deleteRepo
                  (Github.GithubOAuth (T.unpack (fromJust token)))
                  (T.unpack name) (T.unpack repoName')
    either (failure . Git.BackendError . T.pack . show) return result

doOpenGhRepository :: Git.RepositoryOptions
                   -> GitHubOwner
                   -> Text
                   -> Maybe Text
                   -> Github.Repo
                   -> IO Repository
doOpenGhRepository _ owner repoName token repo = do
    mgr <- newManager def
    return Repository { httpManager   = Just mgr
                      , gitHubRepo    = repo
                      , gitHubOptions = GitHubOptions
                          { ghRepoOwner = owner
                          , ghRepoName  = repoName
                          , ghRepoToken = token
                          }
                      }

createGhRepository :: Git.RepositoryOptions
                   -> GitHubOwner
                   -> Text
                   -> Text
                   -> IO (Either Github.Error Repository)
createGhRepository _ owner repoName token =
    let auth     = Github.GithubOAuth (T.unpack token)
        newr     = (Github.newRepo (T.unpack repoName))
                   { Github.newRepoHasIssues = Just False
                   , Github.newRepoAutoInit  = Just True }
    in either (return . Left) confirmCreation =<<
       case owner of
           GitHubUser _ -> Github.createRepo auth newr
           GitHubOrganization name ->
               Github.createOrganizationRepo auth (T.unpack name) newr
  where
    confirmCreation _ = do
        repo <- query owner repoName (20 :: Int)
        flip (either (return . Left)) repo $ \r -> do
            mgr <- newManager def
            return $ Right $ Repository
                { httpManager   = Just mgr
                , gitHubRepo    = r
                , gitHubOptions = GitHubOptions
                    { ghRepoOwner = owner
                    , ghRepoName  = repoName
                    , ghRepoToken = Just token
                    }
                }

    query owner repoName count = do
        -- Poll every five seconds for 100 seconds, waiting for the repository
        -- to be created, since this happens asynchronously on the GitHub
        -- servers.
        repo <- liftIO $ threadDelay 5000000
                >> doesRepoExist owner repoName
        case repo of
            Left l
                | count < 0 -> return (Left l)
                | otherwise -> query owner repoName (count - 1)
            Right r -> return (Right r)

doesRepoExist :: GitHubOwner -> Text -> IO (Either Github.Error Github.Repo)
doesRepoExist owner repoName =
    case owner of
        GitHubUser name -> do
            Github.userRepo (T.unpack name) (T.unpack repoName)
        GitHubOrganization name -> do
            Github.organizationRepo (T.unpack name) (T.unpack repoName)

openOrCreateGhRepository :: Git.RepositoryOptions
                         -> GitHubOwner
                         -> Text
                         -> Maybe Text
                         -> IO (Either Github.Error Repository)
openOrCreateGhRepository opts owner repoName token = do
    exists <- doesRepoExist owner repoName
    case exists of
        Left _ -> case token of
            Just tok -> createGhRepository opts owner repoName tok
            Nothing  -> return (Left (Github.UserError
                                      "Authentication token not provided"))
        Right r -> Right <$> doOpenGhRepository opts owner repoName token r

ghFactory :: Git.MonadGit m
          => GitHubOwner
          -> Text
          -> Maybe Text
          -> Git.RepositoryFactory (GitHubRepository m) m Repository
ghFactory owner repoName token = Git.RepositoryFactory
    { Git.openRepository  = \opts -> openGhRepository opts owner repoName token
    , Git.runRepository   = runGhRepository
    , Git.closeRepository = closeGhRepository
    , Git.defaultOptions  = defaultGhOptions
    }

openGhRepository :: Git.MonadGit m
                 => Git.RepositoryOptions
                 -> GitHubOwner
                 -> Text
                 -> Maybe Text
                 -> m Repository
openGhRepository opts owner repoName token = do
    liftIO $ openOrCreateGhRepository opts owner repoName token
        >>= either (throwIO . userError . show) return

runGhRepository :: Repository -> GitHubRepository m a -> m a
runGhRepository repo action =
    runReaderT (ghRepositoryReaderT action) repo

closeGhRepository :: Git.MonadGit m => Repository -> m ()
closeGhRepository = const (return ())

defaultGhOptions :: Git.RepositoryOptions
defaultGhOptions = Git.RepositoryOptions "" False False

-- GitHub.hs
