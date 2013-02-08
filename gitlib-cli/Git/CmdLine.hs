{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.CmdLine where

import           Control.Applicative hiding (many)
import           Control.Exception hiding (try)
import           Control.Failure
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
-- import           Data.Attempt
import           Data.ByteString as B hiding (pack, putStrLn, map, null)
import           Data.Conduit
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text as T hiding (drop, map, null)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTime)
-- import           Debug.Trace
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Git
import           Prelude hiding (FilePath)
import           Shelly
import           System.IO.Unsafe
import           System.Locale (defaultTimeLocale)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.Text.Lazy

type Oid       = Git.Oid CmdLineRepository

type BlobOid   = Git.BlobOid CmdLineRepository
type TreeOid   = Git.TreeOid CmdLineRepository
type CommitOid = Git.CommitOid CmdLineRepository

type Blob      = Git.Blob CmdLineRepository
type Tree      = Git.Tree CmdLineRepository
type TreeEntry = Git.TreeEntry CmdLineRepository
type Commit    = Git.Commit CmdLineRepository

type TreeRef   = Git.TreeRef CmdLineRepository
type CommitRef = Git.CommitRef CmdLineRepository

type Reference = Git.Reference CmdLineRepository Commit

instance Git.RepositoryBase CmdLineRepository where
    data Oid CmdLineRepository = Oid { getOid :: TL.Text }

    data Tree CmdLineRepository = CmdLineTree
        { cliTreeOid      :: IORef (Maybe TreeOid)
        , cliTreeContents :: IORef (HashMap Text TreeEntry)
        }

    data Commit CmdLineRepository = CmdLineCommit
        { cliCommitOid       :: Maybe CommitOid
        , cliCommitAuthor    :: Git.Signature
        , cliCommitCommitter :: Maybe Git.Signature
        , cliCommitMessage   :: Text
        , cliCommitTree      :: TreeRef
        , cliCommitParents   :: [CommitRef]
        }

    data Tag CmdLineRepository = Tag { tagCommit :: CommitRef }

    parseOid = return . Oid . TL.fromStrict
    renderOid (Tagged (Oid x)) = TL.toStrict x

    lookupRef    = undefined -- cliLookupRef
    updateRef    = undefined -- cliUpdateRef
    resolveRef   = undefined -- cliResolveRef
    allRefNames  = undefined -- cliAllRefNames
    lookupCommit = cliLookupCommit
    lookupTree   = cliLookupTree
    lookupBlob   = cliLookupBlob
    lookupTag    = undefined
    lookupObject = undefined -- cliLookupObject
    newTree      = cliNewTree
    createBlob   = cliCreateBlob
    createCommit = cliCreateCommit
    createTag    = undefined

data CliBlob = CliBlob
    { cliBlobContent  :: ByteString
    , cliBlobEncoding :: Text
    , cliBlobSha      :: Text
    , cliBlobSize     :: Int } deriving Show

instance Show (Git.Oid CmdLineRepository) where
    show = T.unpack . Git.renderOid . Tagged

instance Ord (Git.Oid CmdLineRepository) where
    compare (Oid l) (Oid r) = compare l r

instance Eq (Git.Oid CmdLineRepository) where
    Oid l == Oid r = l == r

instance MonadBase IO CmdLineRepository where
    liftBase = liftIO

instance MonadUnsafeIO CmdLineRepository where
    unsafeLiftIO = return . unsafePerformIO

instance MonadThrow CmdLineRepository where
    -- monadThrow :: Exception e => e -> m a
    monadThrow = throw

cliLookupBlob :: BlobOid -> CmdLineRepository Blob
cliLookupBlob (Tagged (Oid sha)) = do
    repo <- cliGet
    out  <- shelly $ silently $
            run "git" [ "--git-dir", repoPath repo, "cat-file", "-p", sha ]
    return (Git.BlobString (T.encodeUtf8 (TL.toStrict out)))

cliCreateBlob :: Git.BlobContents CmdLineRepository -> CmdLineRepository BlobOid
cliCreateBlob (Git.BlobString content) = do
    repo <- cliGet
    oid  <- shelly $ silently $ do
        setStdin (TL.fromStrict (T.decodeUtf8 content))
        run "git" ["--git-dir", repoPath repo, "hash-object", "-w", "--stdin"]
    return (Tagged (Oid (TL.init oid)))

cliCreateBlob _ = error "NYI"    -- jww (2013-02-06): NYI

cliNewTree :: CmdLineRepository Tree
cliNewTree = CmdLineTree <$> (liftIO $ newIORef Nothing)
                         <*> (liftIO $ newIORef HashMap.empty)

cliLookupTree :: TreeOid -> CmdLineRepository Tree
cliLookupTree oid@(Tagged (Oid sha)) = do
    repo        <- cliGet
    contents    <- shelly $ silently $ do
        run "git" ["--git-dir", repoPath repo, "ls-tree", "-z", sha]
    oidRef      <- liftIO $ newIORef (Just oid)
    contentsRef <- liftIO $ newIORef $ HashMap.fromList $
                   map parseLine (TL.splitOn "\NUL" contents)
    return (CmdLineTree oidRef contentsRef)
  where
    parseLine line =
        let [prefix,path] = TL.splitOn "\t" line
            [mode,kind,sha] = TL.words prefix
        in (TL.toStrict path,
            case kind of
            "blob" -> Git.BlobEntry (Tagged (Oid sha)) (mode == "100755")
            "tree" -> Git.TreeEntry (Git.ByOid (Tagged (Oid sha)))
            _ -> error "This cannot happen")

doLookupTreeEntry :: Tree -> [Text] -> CmdLineRepository (Maybe TreeEntry)
doLookupTreeEntry t [] = return (Just (Git.treeEntry t))
doLookupTreeEntry t (name:names) = do
  -- Lookup the current name in this tree.  If it doesn't exist, and there are
  -- more names in the path and 'createIfNotExist' is True, create a new Tree
  -- and descend into it.  Otherwise, if it exists we'll have @Just (TreeEntry
  -- {})@, and if not we'll have Nothing.

  y <- liftIO $ HashMap.lookup name <$> readIORef (cliTreeContents t)
  if null names
      then return y
      else case y of
      Just (Git.BlobEntry {}) -> failure Git.TreeCannotTraverseBlob
      Just (Git.TreeEntry t') -> do t'' <- Git.resolveTree t'
                                    doLookupTreeEntry t'' names
      _ -> return Nothing

doModifyTree :: Tree
             -> [Text]
             -> Bool
             -> (Maybe TreeEntry -> CmdLineRepository (Maybe TreeEntry))
             -> CmdLineRepository (Maybe TreeEntry)
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
        liftIO $ modifyIORef (cliTreeContents t) $
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
            Just (Git.BlobEntry {}) -> failure Git.TreeCannotTraverseBlob
            Just (Git.TreeEntry st') -> do
                st <- Git.resolveTree st'
                ze <- doModifyTree st names createIfNotExist f
                liftIO $ do
                    modifyIORef (cliTreeOid t) (const Nothing)
                    stc <- readIORef (cliTreeContents st)
                    modifyIORef (cliTreeContents t) $
                        if HashMap.null stc
                        then HashMap.delete name
                        else HashMap.insert name (Git.treeEntry st)
                return ze

cliModifyTree :: Tree -> FilePath -> Bool
             -> (Maybe TreeEntry -> CmdLineRepository (Maybe TreeEntry))
             -> CmdLineRepository (Maybe TreeEntry)
cliModifyTree tree = doModifyTree tree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

cliWriteTree :: Tree -> CmdLineRepository TreeOid
cliWriteTree tree = do
    repo     <- cliGet
    contents <- liftIO $ readIORef (cliTreeContents tree)
    rendered <- mapM renderLine (HashMap.toList contents)
    oid      <- shelly $ silently $ do
        setStdin $ TL.append (TL.intercalate "\NUL" rendered) "\NUL"
        run "git" ["--git-dir", repoPath repo, "mktree", "-z", "--missing"]
    return (Tagged (Oid (TL.init oid)))
  where
    renderLine (path, Git.BlobEntry (Tagged (Oid sha)) exe) =
        return $ TL.concat [ if exe then "100755" else "100644"
                           , " blob ", sha, "\t", TL.fromStrict path ]
    renderLine (path, Git.TreeEntry tref) = do
        treeOid <- Git.treeRefOid tref
        return $ TL.concat [ "040000 tree "
                            , TL.fromStrict (Git.renderOid treeOid), "\t"
                            , TL.fromStrict path ]

parseCliTime :: Text -> UTCTime
parseCliTime =
    fromJust . parseTime defaultTimeLocale "%Y-%m-%dT%H%M%SZ" . T.unpack

formatCliTime :: UTCTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

cliLookupCommit :: CommitOid -> CmdLineRepository Commit
cliLookupCommit oid@(Tagged (Oid sha)) = do
    repo <- cliGet
    output <- shelly $ silently $ do
        run "git" $ ["--git-dir", repoPath repo, "cat-file", "p", sha]
    case parse parseOutput "" output of
        Left e -> failure Git.CommitLookupFailed
        Right c -> return c
  where
    parseOutput = do
        tree <- string "tree " *>
                (Git.ByOid . Tagged . Oid . TL.pack
                 <$> manyTill anyChar newline)
        parents <- many (string "parent " *>
                         (Git.ByOid . Tagged . Oid . TL.pack
                          <$> manyTill anyChar newline))
        CmdLineCommit
            <$> pure (Just oid)
            <*> parseSignature "author"
            <*> (Just <$> parseSignature "committer ")
            <*> (T.pack <$> many anyChar)
            <*> pure tree
            <*> pure parents

    parseSignature txt =
        Git.Signature
            <$> (string (T.unpack txt ++ " ")
                 *> (T.pack <$> manyTill anyChar (try (string " <"))))
            <*> (T.pack <$> manyTill anyChar (try (string "> ")))
            <*> (fromJust . parseTime defaultTimeLocale "%s %z"
                 <$> manyTill anyChar (try (string "> ")))

cliCreateCommit :: [CommitRef] -> TreeRef
               -> Git.Signature -> Git.Signature -> Text -> Maybe Text
               -> CmdLineRepository Commit
cliCreateCommit parents tree author committer message ref = do
    repo <- cliGet
    treeOid <- Git.treeRefOid tree
    let parentOids = map Git.commitRefOid parents

    oid <- shelly $ silently $ do
        setenv "GIT_AUTHOR_NAME"  (TL.fromStrict (Git.signatureName author))
        setenv "GIT_AUTHOR_EMAIL" (TL.fromStrict (Git.signatureEmail author))
        setenv "GIT_AUTHOR_DATE"
            (TL.fromStrict (formatCliTime (Git.signatureWhen author)))
        setenv "GIT_COMMITTER_NAME"
            (TL.fromStrict (Git.signatureName committer))
        setenv "GIT_COMMITTER_EMAIL"
            (TL.fromStrict (Git.signatureEmail committer))
        setenv "GIT_COMMITTER_DATE"
            (TL.fromStrict (formatCliTime (Git.signatureWhen committer)))

        setStdin $ TL.fromStrict message
        run "git" $ ["--git-dir", repoPath repo, "commit-tree"]
                 <> L.concat [["-p", TL.fromStrict (Git.renderOid poid)] |
                              poid <- parentOids]
                 <> [TL.fromStrict (Git.renderOid treeOid)]

    return CmdLineCommit
        { cliCommitOid       = Just (Tagged (Oid (TL.init oid)))
        , cliCommitAuthor    = author
        , cliCommitCommitter = Just committer
        , cliCommitMessage   = message
        , cliCommitTree      = Git.ByOid treeOid
        , cliCommitParents   = map Git.ByOid parentOids
        }

data CliObjectRef = CliObjectRef
    { objectRefType :: Text
    , objectRefSha  :: Text } deriving Show

data CliReference = CliReference
    { referenceRef    :: Text
    , referenceObject :: CliObjectRef } deriving Show

cliGetRef :: Text -> CmdLineRepository Reference
cliGetRef ref = undefined -- cliRestful "GET" ("git/" <> ref) ()

cliGetAllRefs :: Text -> CmdLineRepository [Reference]
cliGetAllRefs namespace = undefined -- cliRestful "GET" ("git/" <> namespace) ()

cliCreateRef :: Reference -> CmdLineRepository Reference
cliCreateRef ref = undefined -- cliRestful "POST" "git/refs" ref

cliUpdateRef :: Text -> CommitOid -> CmdLineRepository Reference
cliUpdateRef ref sha = do
    -- jww (2013-01-12): restfulEx with a state argument is awkward.  Maybe
    -- have addQueryParam take a third parameter that modifies a RESTfulM's
    -- internal state value, and then do restful ... & addQueryParam, where &
    -- = flip ($)
    -- cliRestfulEx "PATCH" ("git/" <> ref) sha
    --     $ addQueryParam "force" "true"
    return undefined

cliDeleteRef :: Text -> CmdLineRepository ()
cliDeleteRef ref = undefined

data Repository = Repository { repoPath :: TL.Text } deriving Show

newtype CmdLineRepository a = CmdLineRepository
    { runCmdLineRepository :: ReaderT Repository IO a }

instance Functor CmdLineRepository where
    fmap f (CmdLineRepository x) = CmdLineRepository (fmap f x)

instance Applicative CmdLineRepository where
    pure = CmdLineRepository . pure
    CmdLineRepository f <*> CmdLineRepository x = CmdLineRepository (f <*> x)

instance Monad CmdLineRepository where
    return = CmdLineRepository . return
    CmdLineRepository m >>= f = CmdLineRepository (m >>= runCmdLineRepository . f)

instance MonadIO CmdLineRepository where
    liftIO m = CmdLineRepository (liftIO m)

instance Exception e => Failure e CmdLineRepository where
    failure = liftIO . throwIO

cliGet :: CmdLineRepository Repository
cliGet = CmdLineRepository ask

instance Git.Treeish Tree where
    type TreeRepository = CmdLineRepository
    modifyTree = cliModifyTree
    writeTree  = cliWriteTree

instance Git.Commitish Commit where
    type CommitRepository = CmdLineRepository
    commitOid     = fromJust . cliCommitOid
    commitParents = cliCommitParents
    commitTree    = cliCommitTree

instance Git.Treeish Commit where
    type TreeRepository = CmdLineRepository
    modifyTree c path createIfNotExist f =
        Git.commitTree' c >>= \t -> Git.modifyTree t path createIfNotExist f
    writeTree c = Git.commitTree' c >>= Git.writeTree

withOpenCmdLineRepository :: Repository -> CmdLineRepository a -> IO a
withOpenCmdLineRepository repo action =
    runReaderT (runCmdLineRepository action) repo

withCmdLineRepository ::
    FilePath -> Bool -> CmdLineRepository a -> IO (Either Text a)
withCmdLineRepository path bar action =
    bracket
    (openOrCreateCmdLineRepository path bar)
    (\repo -> case repo of
          Left _ -> return ()
          Right _ -> return ())
    (\repo -> case repo of
          Left e -> return (Left e)
          Right r -> Right <$> withOpenCmdLineRepository r action)

openCmdLineRepository :: FilePath -> IO Repository
openCmdLineRepository path =
    case F.toText path of
        Left _ -> error $ "Cannot convert path: " ++ show path
        Right p -> return Repository { repoPath = TL.fromStrict p }

createCmdLineRepository :: FilePath -> Bool -> IO (Either Text Repository)
createCmdLineRepository path bare = do
    case F.toText path of
        Left e -> return (Left e)
        Right p -> do
            shelly $ silently $
                run_ "git" $ ["--git-dir", TL.fromStrict p]
                          <> ["--bare" | bare] <> ["init"]
            return (Right (Repository (TL.fromStrict p)))

openOrCreateCmdLineRepository :: FilePath -> Bool -> IO (Either Text Repository)
openOrCreateCmdLineRepository path bare = do
    exists <- F.isDirectory path
    if exists
        then Right <$> openCmdLineRepository path
        else createCmdLineRepository path bare

-- CmdLine.hs
