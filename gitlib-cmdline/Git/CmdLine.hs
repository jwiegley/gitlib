{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Git.CmdLine where

import           Control.Applicative hiding (many)
import           Control.Exception hiding (try)
import           Control.Failure
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Reader
import           Data.Conduit
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text as T hiding (map, null)
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format (formatTime, parseTime)
import           Data.Tuple
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Git
import qualified Git.Utils as Git
import           Prelude hiding (FilePath)
import           Shelly hiding (trace)
import           System.IO.Unsafe
import           System.Locale (defaultTimeLocale)
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Prim
import           Text.Parsec.Text.Lazy ()

type Oid       = Git.Oid CmdLineRepository

type BlobOid   = Git.BlobOid CmdLineRepository
type TreeOid   = Git.TreeOid CmdLineRepository
type CommitOid = Git.CommitOid CmdLineRepository
type TagOid    = Git.TagOid CmdLineRepository

type Blob      = Git.Blob CmdLineRepository
type Tree      = Git.Tree CmdLineRepository
type TreeEntry = Git.TreeEntry CmdLineRepository
type Commit    = Git.Commit CmdLineRepository
type Tag       = Git.Tag CmdLineRepository

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

    data Tag CmdLineRepository = CmdLineTag
        { cliTagCommit :: CommitRef }

    data Options CmdLineRepository = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    parseOid = return . Oid . TL.fromStrict
    renderOid (Oid x) = TL.toStrict x

    lookupRef    = cliLookupRef
    createRef    = cliUpdateRef
    updateRef    = cliUpdateRef
    deleteRef    = cliDeleteRef
    resolveRef   = cliResolveRef
    pushRef      = cliPushRef
    allRefs      = cliAllRefs
    lookupCommit = cliLookupCommit
    lookupTree   = cliLookupTree
    lookupBlob   = cliLookupBlob
    lookupTag    = undefined -- cliLookupTag
    lookupObject = undefined -- cliLookupObject
    existsObject = undefined -- cliExistsObject
    newTree      = cliNewTree
    hashContents = cliHashContents
    createBlob   = cliCreateBlob
    createCommit = cliCreateCommit
    createTag    = cliCreateTag

    deleteRepository =
        cliGet >>= liftIO . F.removeTree . Git.repoPath . repoOptions

instance Show (Git.Oid CmdLineRepository) where
    show (Oid x) = show x

instance Ord (Git.Oid CmdLineRepository) where
    compare (Oid l) (Oid r) = compare l r

instance Eq (Git.Oid CmdLineRepository) where
    Oid l == Oid r = l == r

instance MonadBase IO CmdLineRepository where
    liftBase = liftIO

instance MonadUnsafeIO CmdLineRepository where
    unsafeLiftIO = return . unsafePerformIO

instance MonadThrow CmdLineRepository where
    monadThrow = throw

doRunGit :: (FilePath -> [TL.Text] -> Sh a) -> [TL.Text] -> Sh ()
         -> CmdLineRepository a
doRunGit f args act = do
    repo <- cliGet
    shellyNoDir $ silently $ do
        act
        f "git" $ ["--git-dir", repoPath repo] <> args

runGit :: [TL.Text] -> CmdLineRepository TL.Text
runGit = flip (doRunGit run) (return ())

runGit_ :: [TL.Text] -> CmdLineRepository ()
runGit_ = flip (doRunGit run_) (return ())

cliPushRefDirectly ::
    Git.Repository r
    => Reference -> Text -> Text
    -> CmdLineRepository (r (Maybe (Git.Reference r (Git.Commit r))))
cliPushRefDirectly ref remoteNameOrURI remoteRefName = do
    repo <- cliGet
    r <- shellyNoDir $ silently $ errExit False $ do
        run_ "git" $ [ "--git-dir", repoPath repo ]
                  <> [ "push", TL.fromStrict remoteNameOrURI
                     , TL.concat [ TL.fromStrict (Git.refName ref)
                                 , ":", TL.fromStrict remoteRefName ] ]
        lastExitCode
    return $ if r == 0
             then Git.lookupRef (Git.refName ref)
             else return Nothing

cliPushRef :: Git.Repository r
           => Reference -> Maybe Text -> Text
           -> CmdLineRepository (r (Maybe (Git.Reference r (Git.Commit r))))
cliPushRef ref remoteNameOrURI remoteRefName =
    maybe (Git.genericPushRef ref remoteRefName)
        (flip (cliPushRefDirectly ref) remoteRefName) remoteNameOrURI

cliLookupBlob :: BlobOid -> CmdLineRepository Blob
cliLookupBlob oid@(Tagged (Oid sha)) =
    Git.Blob oid . Git.BlobString . T.encodeUtf8 . TL.toStrict <$>
        runGit ["cat-file", "-p", sha]

cliDoCreateBlob :: Git.BlobContents CmdLineRepository -> Bool
                -> CmdLineRepository BlobOid
cliDoCreateBlob b persist = do
    Tagged . Oid . TL.init <$>
        (Git.blobContentsToByteString b
         >>= \bs ->
             doRunGit run (["hash-object"] <> ["-w" | persist] <> ["--stdin"]) $
                 setStdin (TL.fromStrict (T.decodeUtf8 (bs))))

cliHashContents :: Git.BlobContents CmdLineRepository
                -> CmdLineRepository BlobOid
cliHashContents b = cliDoCreateBlob b False

cliCreateBlob :: Git.BlobContents CmdLineRepository -> CmdLineRepository BlobOid
cliCreateBlob b = cliDoCreateBlob b True

cliNewTree :: CmdLineRepository Tree
cliNewTree = CmdLineTree <$> (liftIO $ newIORef Nothing)
                         <*> (liftIO $ newIORef HashMap.empty)

cliLookupTree :: TreeOid -> CmdLineRepository Tree
cliLookupTree oid@(Tagged (Oid sha)) = do
    contents <- runGit ["ls-tree", "-z", sha]
    oidRef   <- liftIO $ newIORef (Just oid)
    -- Even though the tree entries are separated by \NUL, for whatever reason
    -- @git ls-tree@ also outputs a newline at the end.
    contentsRef <- liftIO $ newIORef $ HashMap.fromList $
                   map parseLine (L.init (TL.splitOn "\NUL" contents))
    return (CmdLineTree oidRef contentsRef)
  where
    parseLine line =
        let [prefix,path] = TL.splitOn "\t" line
            [mode,kind,sha] = TL.words prefix
        in (TL.toStrict path,
            case kind of
            "blob"   -> Git.BlobEntry (Tagged (Oid sha)) $
                        case mode of
                            "100644" -> Git.PlainBlob
                            "100755" -> Git.ExecutableBlob
                            "120000" -> Git.SymlinkBlob
                            _        -> Git.UnknownBlob
            "commit" -> Git.CommitEntry (Tagged (Oid sha))
            "tree"   -> Git.TreeEntry (Git.ByOid (Tagged (Oid sha)))
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
      Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
      Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
      Just (Git.TreeEntry t')   -> do t'' <- Git.resolveTreeRef t'
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
            Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
            Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
            Just (Git.TreeEntry st')  -> do
                st <- Git.resolveTreeRef st'
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
    contents <- liftIO $ readIORef (cliTreeContents tree)
    rendered <- mapM renderLine (HashMap.toList contents)
    oid      <- doRunGit run ["mktree", "-z", "--missing"]
                $ setStdin $ TL.append (TL.intercalate "\NUL" rendered) "\NUL"
    return (Tagged (Oid (TL.init oid)))
  where
    renderLine (path, Git.BlobEntry (Tagged (Oid sha)) kind) =
        return $ TL.concat [ case kind of
                                  Git.PlainBlob      -> "100644"
                                  Git.ExecutableBlob -> "100755"
                                  Git.SymlinkBlob    -> "120000"
                                  Git.UnknownBlob    -> "100000"
                           , " blob ", sha, "\t", TL.fromStrict path ]
    renderLine (path, Git.CommitEntry coid) = do
        return $ TL.concat [ "160000 commit "
                           , TL.fromStrict (Git.renderObjOid coid), "\t"
                           , TL.fromStrict path ]
    renderLine (path, Git.TreeEntry tref) = do
        treeOid <- Git.treeRefOid tref
        return $ TL.concat
            [ "040000 tree "
            , TL.fromStrict (Git.renderObjOid treeOid), "\t"
            , TL.fromStrict path ]

cliTraverseEntries :: Tree -> (FilePath -> TreeEntry -> CmdLineRepository b)
                   -> CmdLineRepository [b]
cliTraverseEntries tree f = do
    Tagged (Oid sha) <- Git.writeTree tree
    contents <- runGit ["ls-tree", "-t", "-r", "-z", sha]
    -- Even though the tree entries are separated by \NUL, for whatever reason
    -- @git ls-tree@ also outputs a newline at the end.
    mapM (uncurry f) $ map parseLine (L.init (TL.splitOn "\NUL" contents))
  where
    parseLine line =
        let [prefix,path] = TL.splitOn "\t" line
            [mode,kind,sha] = TL.words prefix
        in (fromText path,
            case kind of
            "blob"   -> Git.BlobEntry (Tagged (Oid sha)) $
                        case mode of
                            "100644" -> Git.PlainBlob
                            "100755" -> Git.ExecutableBlob
                            "120000" -> Git.SymlinkBlob
                            _        -> Git.UnknownBlob
            "commit" -> Git.CommitEntry (Tagged (Oid sha))
            "tree"   -> Git.TreeEntry (Git.ByOid (Tagged (Oid sha)))
            _ -> error "This cannot happen")

parseCliTime :: Text -> UTCTime
parseCliTime =
    fromJust . parseTime defaultTimeLocale "%Y-%m-%dT%H%M%SZ" . T.unpack

formatCliTime :: UTCTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"

cliLookupCommit :: CommitOid -> CmdLineRepository Commit
cliLookupCommit oid@(Tagged (Oid sha)) = do
    output <- runGit ["cat-file", "-p", sha]
    case parse parseOutput "" output of
        Left e  -> failure $ Git.CommitLookupFailed (T.pack (show e))
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
            <*> (Just <$> parseSignature "committer")
            <*> (T.pack <$> many anyChar)
            <*> pure tree
            <*> pure parents

    parseSignature txt =
        Git.Signature
            <$> (string (T.unpack txt ++ " ")
                 *> (T.pack <$> manyTill anyChar (try (string " <"))))
            <*> (T.pack <$> manyTill anyChar (try (string "> ")))
            <*> (fromJust . parseTime defaultTimeLocale "%s %z"
                 <$> manyTill anyChar newline)

cliCreateCommit :: [CommitRef] -> TreeRef
               -> Git.Signature -> Git.Signature -> Text -> Maybe Text
               -> CmdLineRepository Commit
cliCreateCommit parents tree author committer message ref = do
    treeOid <- Git.treeRefOid tree
    let parentOids = map Git.commitRefOid parents
    oid <- doRunGit run
           (["commit-tree"]
            <> [TL.fromStrict (Git.renderObjOid treeOid)]
            <> L.concat [["-p", TL.fromStrict (Git.renderObjOid poid)] |
                         poid <- parentOids])
           $ do mapM_ (\(var,f,val) -> setenv var (TL.fromStrict (f val)))
                      [ ("GIT_AUTHOR_NAME",  Git.signatureName,  author)
                      , ("GIT_AUTHOR_EMAIL", Git.signatureEmail, author)
                      , ("GIT_AUTHOR_DATE",
                         formatCliTime . Git.signatureWhen, author)
                      , ("GIT_COMMITTER_NAME",  Git.signatureName,  committer)
                      , ("GIT_COMMITTER_EMAIL", Git.signatureEmail, committer)
                      , ("GIT_COMMITTER_DATE",
                         formatCliTime . Git.signatureWhen, committer)
                      ]
                setStdin $ TL.fromStrict message

    let commit = CmdLineCommit
            { cliCommitOid       = Just (Tagged (Oid (TL.init oid)))
            , cliCommitAuthor    = author
            , cliCommitCommitter = Just committer
            , cliCommitMessage   = message
            , cliCommitTree      = Git.ByOid treeOid
            , cliCommitParents   = map Git.ByOid parentOids
            }
    when (isJust ref) $
        void (cliUpdateRef (fromJust ref)
              (Git.RefObj (Git.ByOid (fromJust (cliCommitOid commit)))))

    return commit

data CliObjectRef = CliObjectRef
    { objectRefType :: Text
    , objectRefSha  :: Text } deriving Show

data CliReference = CliReference
    { referenceRef    :: Text
    , referenceObject :: CliObjectRef } deriving Show

cliShowRef :: Maybe Text -> CmdLineRepository (Maybe [(TL.Text,TL.Text)])
cliShowRef mrefName = do
    repo <- cliGet
    shellyNoDir $ silently $ errExit False $ do
        rev <- run "git" $ [ "--git-dir", repoPath repo
                           , "show-ref" ]
                       <> [ TL.fromStrict (fromJust mrefName)
                          | isJust mrefName ]
        ec  <- lastExitCode
        return $ if ec == 0
                 then Just $ map ((\(x:y:[]) -> (y,x)) . TL.words)
                           $ TL.lines rev
                 else Nothing

nameAndShaToRef :: TL.Text -> TL.Text -> Reference
nameAndShaToRef name sha =
    Git.Reference (TL.toStrict name)
                  (Git.RefObj (Git.ByOid (Tagged (Oid sha))))

cliLookupRef :: Text -> CmdLineRepository (Maybe Reference)
cliLookupRef refName = do
    xs <- cliShowRef (Just refName)
    let name = TL.fromStrict refName
        ref  = maybe Nothing (lookup name) xs
    return $ maybe Nothing (Just . uncurry nameAndShaToRef .
                            \sha -> (name,sha)) ref

cliUpdateRef :: Text -> Git.RefTarget CmdLineRepository Commit
             -> CmdLineRepository Reference
cliUpdateRef refName refObj@(Git.RefObj commitRef) = do
    let Tagged (Oid sha) = Git.commitRefOid commitRef
    runGit_ ["update-ref", TL.fromStrict refName, sha]
    return (Git.Reference refName refObj)

cliUpdateRef refName refObj@(Git.RefSymbolic targetName) = do
    runGit_ ["symbolic-ref", TL.fromStrict refName, TL.fromStrict targetName]
    return (Git.Reference refName refObj)

cliDeleteRef :: Text -> CmdLineRepository ()
cliDeleteRef refName = runGit_ ["update-ref", "-d", TL.fromStrict refName]

cliAllRefs :: CmdLineRepository [Reference]
cliAllRefs = do
    mxs <- cliShowRef Nothing
    return $ case mxs of
        Nothing -> []
        Just xs -> map (uncurry nameAndShaToRef) xs

cliResolveRef :: Text -> CmdLineRepository (Maybe CommitRef)
cliResolveRef refName = do
    repo <- cliGet
    shellyNoDir $ silently $ errExit False $ do
        rev <- run "git" $ [ "--git-dir", repoPath repo
                           , "rev-parse", TL.fromStrict refName ]
        ec  <- lastExitCode
        return $ if ec == 0
            then Just (Git.ByOid (Tagged (Oid (TL.init rev))))
            else Nothing

-- cliLookupTag :: TagOid -> CmdLineRepository Tag
-- cliLookupTag oid = undefined

cliCreateTag :: CommitOid -> Git.Signature -> Text -> Text
             -> CmdLineRepository Tag
cliCreateTag oid@(Tagged (Oid sha)) tagger msg name = do
    doRunGit run_ ["mktag"] $ setStdin $ TL.unlines $
        [ "object " <> sha
        , "type commit"
        , "tag " <> TL.fromStrict name
        , "tagger " <> TL.fromStrict (Git.signatureName tagger)
          <> " <" <> TL.fromStrict (Git.signatureEmail tagger) <> "> "
          <> TL.pack (formatTime defaultTimeLocale "%s %z"
                      (Git.signatureWhen tagger))
        , ""] <> TL.lines (TL.fromStrict msg)
    return $ CmdLineTag (Git.ByOid oid)

data Repository = Repository
    { repoOptions :: Git.RepositoryOptions
    }

repoPath :: Repository -> TL.Text
repoPath = toTextIgnore . Git.repoPath . repoOptions

newtype CmdLineRepository a = CmdLineRepository
    { cmdLineRepositoryReaderT :: ReaderT Repository IO a }

instance Functor CmdLineRepository where
    fmap f (CmdLineRepository x) = CmdLineRepository (fmap f x)

instance Applicative CmdLineRepository where
    pure = CmdLineRepository . pure
    CmdLineRepository f <*> CmdLineRepository x = CmdLineRepository (f <*> x)

instance Monad CmdLineRepository where
    return = CmdLineRepository . return
    CmdLineRepository m >>= f =
        CmdLineRepository (m >>= cmdLineRepositoryReaderT . f)

instance MonadIO CmdLineRepository where
    liftIO m = CmdLineRepository (liftIO m)

instance Exception e => Failure e CmdLineRepository where
    failure = liftIO . throwIO

cliGet :: CmdLineRepository Repository
cliGet = CmdLineRepository ask

instance Git.Treeish Tree where
    type TreeRepository Tree = CmdLineRepository
    modifyTree      = cliModifyTree
    writeTree       = cliWriteTree
    traverseEntries = cliTraverseEntries

instance Git.Commitish Commit where
    type CommitRepository Commit = CmdLineRepository
    commitOid       = fromJust . cliCommitOid
    commitParents   = cliCommitParents
    commitTree      = cliCommitTree
    commitAuthor    = cliCommitAuthor
    commitCommitter = \c -> fromMaybe (cliCommitAuthor c) (cliCommitCommitter c)
    commitLog       = cliCommitMessage
    commitEncoding  = const "utf-8"

instance Git.Treeish Commit where
    type TreeRepository Commit = CmdLineRepository
    modifyTree      = Git.defaultCommitModifyTree
    writeTree       = Git.defaultCommitWriteTree
    traverseEntries = Git.defaultCommitTraverseEntries

cliFactory :: Git.RepositoryFactory CmdLineRepository IO Repository
cliFactory = Git.RepositoryFactory
    { Git.openRepository  = openCmdLineRepository
    , Git.runRepository   = runCmdLineRepository
    , Git.closeRepository = closeCmdLineRepository
    , Git.defaultOptions  = defaultCmdLineOptions
    }

openCmdLineRepository :: Git.RepositoryOptions -> IO Repository
openCmdLineRepository opts = do
    let path = Git.repoPath opts
    exists <- F.isDirectory path
    case F.toText path of
        Left e -> failure (Git.BackendError e)
        Right p -> do
            when (not exists && Git.repoAutoCreate opts) $
                shellyNoDir $ silently $
                    run_ "git" $ ["--git-dir", TL.fromStrict p]
                              <> ["--bare" | Git.repoIsBare opts]
                              <> ["init"]
            return Repository { repoOptions = opts }

runCmdLineRepository :: Repository -> CmdLineRepository a -> IO a
runCmdLineRepository repo action =
    runReaderT (cmdLineRepositoryReaderT action) repo

closeCmdLineRepository :: Repository -> IO ()
closeCmdLineRepository = const (return ())

defaultCmdLineOptions :: Git.RepositoryOptions
defaultCmdLineOptions = Git.RepositoryOptions "" False False

-- CmdLine.hs
