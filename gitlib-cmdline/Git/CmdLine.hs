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
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader
import qualified Data.ByteString as B
import           Data.Conduit
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List as L
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import           Data.Time
import           Data.Tuple
-- import           Debug.Trace
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Git
import qualified Git.Utils as Git
import           Prelude hiding (FilePath)
import           Shelly hiding (trace)
import           System.Exit
import           System.IO.Unsafe
import           System.Locale (defaultTimeLocale)
import           System.Process.ByteString
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.Prim
import           Text.Parsec.Token

type Oid m       = Git.Oid (CmdLineRepository m)

type BlobOid m   = Git.BlobOid (CmdLineRepository m)
type TreeOid m   = Git.TreeOid (CmdLineRepository m)
type CommitOid m = Git.CommitOid (CmdLineRepository m)
type TagOid m    = Git.TagOid (CmdLineRepository m)

type Blob m      = Git.Blob (CmdLineRepository m)
type Tree m      = Git.Tree (CmdLineRepository m)
type TreeEntry m = Git.TreeEntry (CmdLineRepository m)
type Commit m    = Git.Commit (CmdLineRepository m)
type Tag m       = Git.Tag (CmdLineRepository m)

type TreeRef m   = Git.TreeRef (CmdLineRepository m)
type CommitRef m = Git.CommitRef (CmdLineRepository m)

type Reference m = Git.Reference (CmdLineRepository m) (Commit m)

instance Git.MonadGit m => Git.Repository (CmdLineRepository m) where
    data Oid (CmdLineRepository m) = Oid { getOid :: TL.Text }

    data TreeData (CmdLineRepository m) = TreeData
        { cliTreeOid      :: IORef (Maybe (TreeOid m))
        , cliTreeContents :: IORef (HashMap Text (TreeEntry m))
        }

    data Options (CmdLineRepository m) = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    parseOid = return . Oid . TL.fromStrict
    renderOid (Oid x) = TL.toStrict x

    lookupRef       = cliLookupRef
    createRef       = cliUpdateRef
    updateRef       = cliUpdateRef
    deleteRef       = cliDeleteRef
    resolveRef      = cliResolveRef
    pushRef         = cliPushRef
    allRefs         = cliAllRefs
    lookupCommit    = cliLookupCommit
    lookupTree      = cliLookupTree
    lookupBlob      = cliLookupBlob
    lookupTag       = error "Not defined CmdLineRepository.cliLookupTag"
    lookupObject    = error "Not defined CmdLineRepository.cliLookupObject"
    existsObject    = cliExistsObject
    traverseCommits = cliTraverseCommits
    missingObjects  = cliMissingObjects
    newTree         = cliNewTree
    hashContents    = cliHashContents
    createBlob      = cliCreateBlob
    createCommit    = cliCreateCommit
    createTag       = cliCreateTag

    deleteRepository =
        cliGet >>= liftIO . F.removeTree . Git.repoPath . repoOptions

instance Show (Git.Oid (CmdLineRepository m)) where
    show (Oid x) = show x

instance Ord (Git.Oid (CmdLineRepository m)) where
    compare (Oid l) (Oid r) = compare l r

instance Eq (Git.Oid (CmdLineRepository m)) where
    Oid l == Oid r = l == r

doRunGit :: Git.MonadGit m
         => (FilePath -> [TL.Text] -> Sh a) -> [TL.Text] -> Sh ()
         -> CmdLineRepository m a
doRunGit f args act = do
    repo <- cliGet
    shellyNoDir $ silently $ do
        act
        f "git" $ ["--git-dir", repoPath repo] <> args

runGit :: Git.MonadGit m
       => [TL.Text] -> CmdLineRepository m TL.Text
runGit = flip (doRunGit run) (return ())

runGit_ :: Git.MonadGit m
        => [TL.Text] -> CmdLineRepository m ()
runGit_ = flip (doRunGit run_) (return ())

cliPushRefDirectly :: Git.MonadGit m
                   => Text -> Text -> Text -> Maybe FilePath
                   -> CmdLineRepository m Bool
cliPushRefDirectly refName remoteNameOrURI remoteRefName msshCmd = do
    repo <- cliGet
    r <- shellyNoDir $ silently $ errExit False $ do
        case msshCmd of
            Nothing -> return ()
            Just sshCmd -> setenv "GIT_SSH" . toTextIgnore
                               =<< liftIO (F.canonicalizePath sshCmd)
        run_ "git" $ [ "--git-dir", repoPath repo ]
                  <> [ "push", TL.fromStrict remoteNameOrURI
                     , TL.concat [ TL.fromStrict refName
                                 , ":", TL.fromStrict remoteRefName ] ]
        lastExitCode
    return (r == 0)

cliPushRef :: (Git.MonadGit m, Git.MonadGit (t (CmdLineRepository m)),
               Git.Repository (t (CmdLineRepository m)), MonadTrans t)
           => Reference m -> Maybe Text -> Text
           -> t (CmdLineRepository m) Bool
cliPushRef ref remoteNameOrURI remoteRefName =
    case remoteNameOrURI of
        Nothing  -> Git.genericPushRef ref remoteRefName
        Just uri -> lift $ cliPushRefDirectly (Git.refName ref) uri
                        remoteRefName Nothing

cliLookupBlob :: Git.MonadGit m
              => BlobOid m -> CmdLineRepository m (Blob m)
cliLookupBlob oid@(Tagged (Oid sha)) = do
    repo <- cliGet
    (r,out,_) <-
        liftIO $ readProcessWithExitCode "git"
            [ "--git-dir", TL.unpack (repoPath repo)
            , "cat-file", "-p", TL.unpack sha ]
            B.empty
    if r == ExitSuccess
        then return (Git.Blob oid (Git.BlobString out))
        else failure Git.BlobLookupFailed

cliDoCreateBlob :: Git.MonadGit m
                => Git.BlobContents (CmdLineRepository m) -> Bool
                -> CmdLineRepository m (BlobOid m)
cliDoCreateBlob b persist = do
    repo      <- cliGet
    bs        <- Git.blobContentsToByteString b
    (r,out,_) <-
        liftIO $ readProcessWithExitCode "git"
            ([ "--git-dir", TL.unpack (repoPath repo), "hash-object" ]
             <> ["-w" | persist] <> ["--stdin"])
            bs
    if r == ExitSuccess
        then return . Tagged . Oid . TL.fromStrict . T.init . T.decodeUtf8 $ out
        else failure Git.BlobCreateFailed

cliHashContents :: Git.MonadGit m
                => Git.BlobContents (CmdLineRepository m)
                -> CmdLineRepository m (BlobOid m)
cliHashContents b = cliDoCreateBlob b False

cliCreateBlob :: Git.MonadGit m
              => Git.BlobContents (CmdLineRepository m)
              -> CmdLineRepository m (BlobOid m)
cliCreateBlob b = cliDoCreateBlob b True

cliExistsObject :: Git.MonadGit m => Oid m -> CmdLineRepository m Bool
cliExistsObject (Oid sha) = do
    repo <- cliGet
    shellyNoDir $ silently $ errExit False $ do
        run_ "git" $ [ "--git-dir", repoPath repo
                     , "cat-file", "-e", sha ]
        ec <- lastExitCode
        return (ec == 0)

cliTraverseCommits :: Git.MonadGit m
                   => (CommitRef m -> CmdLineRepository m a)
                   -> Reference m
                   -> CmdLineRepository m [a]
cliTraverseCommits f ref = do
    shas <- doRunGit run [ "--no-pager", "log"
                         , "--format=%H", TL.fromStrict (Git.refName ref) ]
            $ return ()
    mapM (\sha -> f =<< (Git.ByOid . Tagged
                         <$> Git.parseOid (TL.toStrict sha)))
        (TL.lines shas)

cliMissingObjects :: Git.MonadGit m
                  => Maybe (Reference m) -> Reference m
                  -> CmdLineRepository m [Oid m]
cliMissingObjects mhave need = do
    shas <- doRunGit run
            ([ "--no-pager", "rev-list", "--objects"]
             <> (case mhave of
                      Nothing ->
                          [TL.fromStrict (Git.refName need)]
                      Just have ->
                          [ TL.fromStrict (Git.refName have)
                          , TL.fromStrict (T.append "^" (Git.refName need)) ]))
            $ return ()
    mapM (Git.parseOid . T.take 40 . TL.toStrict) (TL.lines shas)

cliMakeTree :: Git.MonadGit m
            => IORef (Maybe (TreeOid m))
            -> IORef (HashMap Text (TreeEntry m))
            -> Tree m
cliMakeTree oid contents =
    Git.mkTree cliModifyTree cliWriteTree cliTraverseEntries $
        TreeData oid contents

cliNewTree :: Git.MonadGit m => CmdLineRepository m (Tree m)
cliNewTree = cliMakeTree <$> liftIO (newIORef Nothing)
                         <*> liftIO (newIORef HashMap.empty)

cliLookupTree :: Git.MonadGit m => TreeOid m -> CmdLineRepository m (Tree m)
cliLookupTree oid@(Tagged (Oid sha)) = do
    contents <- runGit ["ls-tree", "-z", sha]
    oidRef   <- liftIO $ newIORef (Just oid)
    -- Even though the tree entries are separated by \NUL, for whatever reason
    -- @git ls-tree@ also outputs a newline at the end.
    contentsRef <- liftIO $ newIORef $ HashMap.fromList $
                   map parseLine (L.init (TL.splitOn "\NUL" contents))
    return $ cliMakeTree oidRef contentsRef
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

doLookupTreeEntry :: Git.MonadGit m => Tree m -> [Text]
                  -> CmdLineRepository m (Maybe (TreeEntry m))
doLookupTreeEntry t [] = return (Just (Git.treeEntry t))
doLookupTreeEntry t (name:names) = do
  -- Lookup the current name in this tree.  If it doesn't exist, and there are
  -- more names in the path and 'createIfNotExist' is True, create a new Tree
  -- and descend into it.  Otherwise, if it exists we'll have @Just (TreeEntry
  -- {})@, and if not we'll have Nothing.

  y <- liftIO $ HashMap.lookup name
           <$> readIORef (cliTreeContents (Git.getTreeData t))
  if null names
      then return y
      else case y of
          Just (Git.BlobEntry {})   ->
              failure Git.TreeCannotTraverseBlob
          Just (Git.CommitEntry {}) ->
              failure Git.TreeCannotTraverseCommit
          Just (Git.TreeEntry t')   -> do
              t'' <- Git.resolveTreeRef t'
              doLookupTreeEntry t'' names
          _ -> return Nothing

doModifyTree :: Git.MonadGit m
             => Tree m
             -> [Text]
             -> Bool
             -> (Maybe (TreeEntry m)
                 -> CmdLineRepository m (Maybe (TreeEntry m)))
             -> CmdLineRepository m (Maybe (TreeEntry m))
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
        liftIO $ modifyIORef (cliTreeContents (Git.getTreeData t)) $
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
                    modifyIORef (cliTreeOid (Git.getTreeData t)) (const Nothing)
                    stc <- readIORef (cliTreeContents (Git.getTreeData st))
                    modifyIORef (cliTreeContents (Git.getTreeData t)) $
                        if HashMap.null stc
                        then HashMap.delete name
                        else HashMap.insert name (Git.treeEntry st)
                return ze

cliModifyTree :: Git.MonadGit m
              => Tree m -> FilePath -> Bool
              -> (Maybe (TreeEntry m)
                  -> CmdLineRepository m (Maybe (TreeEntry m)))
              -> CmdLineRepository m (Maybe (TreeEntry m))
cliModifyTree tree = doModifyTree tree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

cliWriteTree :: Git.MonadGit m
             => Tree m -> CmdLineRepository m (TreeOid m)
cliWriteTree tree = do
    contents <- liftIO $ readIORef (cliTreeContents (Git.getTreeData tree))
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

cliTraverseEntries :: Git.MonadGit m
                   => Tree m
                   -> (FilePath -> TreeEntry m -> CmdLineRepository m b)
                   -> CmdLineRepository m [b]
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

parseCliTime :: String -> ZonedTime
parseCliTime = fromJust . parseTime defaultTimeLocale "%s %z"

formatCliTime :: ZonedTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%s %z"

lexer :: TokenParser u
lexer = makeTokenParser haskellDef

cliLookupCommit :: Git.MonadGit m
                => CommitOid m -> CmdLineRepository m (Commit m)
cliLookupCommit (Tagged (Oid sha)) = do
    output <- doRunGit run ["cat-file", "--batch"]
                  $ setStdin (TL.append sha "\n")
    case parse parseOutput "" (TL.unpack output) of
        Left e  -> failure $ Git.CommitLookupFailed (T.pack (show e))
        Right c -> return c
  where
    parseOutput = do
        coid       <- Tagged . Oid . TL.pack
                      <$> manyTill alphaNum space
        _          <- string "commit " *> manyTill digit newline
        treeOid    <- string "tree " *>
                      (Tagged . Oid . TL.pack
                       <$> manyTill anyChar newline)
        parentOids <- many (string "parent " *>
                            (Tagged . Oid . TL.pack
                             <$> manyTill anyChar newline))
        author     <- parseSignature "author"
        committer  <- parseSignature "committer"
        message    <- newline *> many anyChar
        return Git.Commit
            { Git.commitOid       = coid
            , Git.commitAuthor    = author
            , Git.commitCommitter = committer
            , Git.commitLog       = T.pack (init message)
            , Git.commitTree      = Git.ByOid treeOid
            , Git.commitParents   = map Git.ByOid (Prelude.reverse parentOids)
            , Git.commitEncoding  = "utf-8"
            }

    parseSignature txt =
        Git.Signature
            <$> (string (T.unpack txt ++ " ")
                 *> (T.pack <$> manyTill anyChar (try (string " <"))))
            <*> (T.pack <$> manyTill anyChar (try (string "> ")))
            <*> (parseCliTime <$> manyTill anyChar newline)

cliCreateCommit :: Git.MonadGit m
                => [CommitRef m] -> TreeRef m
                -> Git.Signature -> Git.Signature -> Text -> Maybe Text
                -> CmdLineRepository m (Commit m)
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
                setStdin (TL.fromStrict message)

    let commit = Git.Commit
            { Git.commitOid       = Tagged (Oid (TL.init oid))
            , Git.commitAuthor    = author
            , Git.commitCommitter = committer
            , Git.commitLog       = message
            , Git.commitTree      = Git.ByOid treeOid
            , Git.commitParents   = map Git.ByOid parentOids
            , Git.commitEncoding  = "utf-8"
            }
    when (isJust ref) $
        void $ cliUpdateRef (fromJust ref) (Git.RefObj (Git.Known commit))

    return commit

data CliObjectRef = CliObjectRef
    { objectRefType :: Text
    , objectRefSha  :: Text } deriving Show

data CliReference = CliReference
    { referenceRef    :: Text
    , referenceObject :: CliObjectRef } deriving Show

cliShowRef :: Git.MonadGit m
           => Maybe Text -> CmdLineRepository m (Maybe [(TL.Text,TL.Text)])
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

nameAndShaToRef :: TL.Text -> TL.Text -> Reference m
nameAndShaToRef name sha =
    Git.Reference (TL.toStrict name)
                  (Git.RefObj (Git.ByOid (Tagged (Oid sha))))

cliLookupRef :: Git.MonadGit m
             => Text -> CmdLineRepository m (Maybe (Reference m))
cliLookupRef refName = do
    xs <- cliShowRef (Just refName)
    let name = TL.fromStrict refName
        ref  = maybe Nothing (lookup name) xs
    return $ maybe Nothing (Just . uncurry nameAndShaToRef .
                            \sha -> (name,sha)) ref

cliUpdateRef :: Git.MonadGit m
             => Text -> Git.RefTarget (CmdLineRepository m) (Commit m)
             -> CmdLineRepository m (Reference m)
cliUpdateRef refName refObj@(Git.RefObj commitRef) = do
    let Tagged (Oid sha) = Git.commitRefOid commitRef
    runGit_ ["update-ref", TL.fromStrict refName, sha]
    return (Git.Reference refName refObj)

cliUpdateRef refName refObj@(Git.RefSymbolic targetName) = do
    runGit_ ["symbolic-ref", TL.fromStrict refName, TL.fromStrict targetName]
    return (Git.Reference refName refObj)

cliDeleteRef :: Git.MonadGit m => Text -> CmdLineRepository m ()
cliDeleteRef refName = runGit_ ["update-ref", "-d", TL.fromStrict refName]

cliAllRefs :: Git.MonadGit m
           => CmdLineRepository m [Reference m]
cliAllRefs = do
    mxs <- cliShowRef Nothing
    return $ case mxs of
        Nothing -> []
        Just xs -> map (uncurry nameAndShaToRef) xs

cliResolveRef :: Git.MonadGit m
              => Text -> CmdLineRepository m (Maybe (CommitRef m))
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

cliCreateTag :: Git.MonadGit m
             => CommitOid m -> Git.Signature -> Text -> Text
             -> CmdLineRepository m (Tag m)
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
    return $ Git.Tag (Git.ByOid oid)

data Repository = Repository
    { repoOptions :: Git.RepositoryOptions
    }

repoPath :: Repository -> TL.Text
repoPath = toTextIgnore . Git.repoPath . repoOptions

newtype CmdLineRepository m a = CmdLineRepository
    { cmdLineRepositoryReaderT :: ReaderT Repository m a }

instance Functor m => Functor (CmdLineRepository m) where
    fmap f (CmdLineRepository x) = CmdLineRepository (fmap f x)

instance Applicative m => Applicative (CmdLineRepository m) where
    pure = CmdLineRepository . pure
    CmdLineRepository f <*> CmdLineRepository x = CmdLineRepository (f <*> x)

instance Monad m => Monad (CmdLineRepository m) where
    return = CmdLineRepository . return
    CmdLineRepository m >>= f =
        CmdLineRepository (m >>= cmdLineRepositoryReaderT . f)

instance MonadIO m => MonadIO (CmdLineRepository m) where
    liftIO m = CmdLineRepository (liftIO m)

instance (Monad m, MonadIO m, Applicative m)
         => MonadBase IO (CmdLineRepository m) where
    liftBase = liftIO

instance Monad m => MonadUnsafeIO (CmdLineRepository m) where
    unsafeLiftIO = return . unsafePerformIO

instance Monad m => MonadThrow (CmdLineRepository m) where
    monadThrow = throw

instance MonadTrans CmdLineRepository where
    lift = CmdLineRepository . ReaderT . const

instance MonadTransControl CmdLineRepository where
    newtype StT CmdLineRepository a = StCmdLineRepository
        { unCmdLineRepository :: StT (ReaderT Repository) a
        }
    liftWith = defaultLiftWith CmdLineRepository
                   cmdLineRepositoryReaderT StCmdLineRepository
    restoreT = defaultRestoreT CmdLineRepository unCmdLineRepository

instance (MonadIO m, MonadBaseControl IO m)
         => MonadBaseControl IO (CmdLineRepository m) where
    newtype StM (CmdLineRepository m) a = StMT
        { unStMT :: ComposeSt CmdLineRepository m a
        }
    liftBaseWith = defaultLiftBaseWith StMT
    restoreM     = defaultRestoreM unStMT

cliGet :: Monad m => CmdLineRepository m Repository
cliGet = CmdLineRepository ask

cliFactory :: Git.MonadGit m
           => Git.RepositoryFactory (CmdLineRepository m) m Repository
cliFactory = Git.RepositoryFactory
    { Git.openRepository  = openCliRepository
    , Git.runRepository   = runCliRepository
    , Git.closeRepository = closeCliRepository
    , Git.defaultOptions  = defaultCliOptions
    }

openCliRepository :: Git.MonadGit m => Git.RepositoryOptions -> m Repository
openCliRepository opts = do
    let path = Git.repoPath opts
    exists <- liftIO $ F.isDirectory path
    case F.toText path of
        Left e -> failure (Git.BackendError e)
        Right p -> do
            when (not exists && Git.repoAutoCreate opts) $
                shellyNoDir $ silently $
                    run_ "git" $ ["--git-dir", TL.fromStrict p]
                              <> ["--bare" | Git.repoIsBare opts]
                              <> ["init"]
            return Repository { repoOptions = opts }

runCliRepository :: Git.MonadGit m => Repository -> CmdLineRepository m a -> m a
runCliRepository repo action =
    runReaderT (cmdLineRepositoryReaderT action) repo

closeCliRepository :: Git.MonadGit m => Repository -> m ()
closeCliRepository = const (return ())

defaultCliOptions :: Git.RepositoryOptions
defaultCliOptions = Git.RepositoryOptions "" False False

-- Cli.hs
