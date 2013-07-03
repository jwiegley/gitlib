{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}
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
import           Data.Conduit hiding (MonadBaseControl)
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.IORef
import           Data.List as L
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
#if MIN_VERSION_shelly(1, 0, 0)
import qualified Data.Text as TL
#else
import qualified Data.Text.Lazy as TL
#endif
import           Data.Time
import           Data.Tuple
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

toStrict :: TL.Text -> T.Text
#if MIN_VERSION_shelly(1, 0, 0)
toStrict = id
#else
toStrict = TL.toStrict
#endif

fromStrict :: T.Text -> TL.Text
#if MIN_VERSION_shelly(1, 0, 0)
fromStrict = id
#else
fromStrict = TL.fromStrict
#endif

type BlobOid m    = Git.BlobOid (CmdLineRepository m)
type TreeOid m    = Git.TreeOid (CmdLineRepository m)
type CommitOid m  = Git.CommitOid (CmdLineRepository m)
type TagOid m     = Git.TagOid (CmdLineRepository m)

type Blob m       = Git.Blob (CmdLineRepository m)
type Tree m       = Git.Tree (CmdLineRepository m)
type TreeEntry m  = Git.TreeEntry (CmdLineRepository m)
type Commit m     = Git.Commit (CmdLineRepository m)
type Tag m        = Git.Tag (CmdLineRepository m)

type TreeRef m    = Git.TreeRef (CmdLineRepository m)
type CommitRef m  = Git.CommitRef (CmdLineRepository m)
type CommitName m = Git.CommitName (CmdLineRepository m)

type Reference m  = Git.Reference (CmdLineRepository m) (Commit m)
type Object m     = Git.Object (CmdLineRepository m)

instance Git.MonadGit m => Git.Repository (CmdLineRepository m) where
    type Oid (CmdLineRepository m)  =
#if MIN_VERSION_shelly(1, 0, 0)
        Git.OidText
#else
        Git.OidTextL
#endif
    type Tree (CmdLineRepository m) = MutableTree m

    data Options (CmdLineRepository m) = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

#if MIN_VERSION_shelly(1, 0, 0)
    parseOid = Git.parseOidText
#else
    parseOid = Git.parseOidTextL
#endif

    lookupRef        = cliLookupRef
    createRef        = cliUpdateRef
    updateRef        = cliUpdateRef
    deleteRef        = cliDeleteRef
    resolveRef       = cliResolveRef
    allRefs          = cliAllRefs
    lookupCommit     = cliLookupCommit
    lookupTree       = cliLookupTree
    lookupBlob       = cliLookupBlob
    lookupTag        = error "Not defined CmdLineRepository.cliLookupTag"
    lookupObject     = error "Not defined CmdLineRepository.cliLookupObject"
    existsObject     = cliExistsObject
    pushCommit       = \name _ rrefname -> Git.genericPushCommit name rrefname
    traverseCommits  = cliTraverseCommits
    missingObjects   = cliMissingObjects
    traverseObjects  = error "Not defined: CmdLineRepository.traverseObjects"
    newTree          = cliNewTree
    cloneTree        = cliCloneTree
    traverseEntries  = cliTraverseEntries
    unsafeUpdateTree = cliModifyTree
    writeTree        = cliWriteTree
    hashContents     = cliHashContents
    createBlob       = cliCreateBlob
    createCommit     = cliCreateCommit
    createTag        = cliCreateTag

    remoteFetch     = error "Not defined: CmdLineRepository.remoteFetch"

    deleteRepository =
        cliGet >>= liftIO . F.removeTree . Git.repoPath . repoOptions

git :: [TL.Text] -> Sh TL.Text
git args = do
    -- liftIO $ putStrLn $ "CmdLine: git " ++ show args
    run "git" args

git_ :: [TL.Text] -> Sh ()
git_ args = do
    -- liftIO $ putStrLn $ "CmdLine: git " ++ show args
    run_ "git" args

doRunGit :: Git.MonadGit m
         => (FilePath -> [TL.Text] -> Sh a) -> [TL.Text] -> Sh ()
         -> CmdLineRepository m a
doRunGit f args act = do
    repo <- cliGet
    shellyNoDir $ silently $ do
        act
        -- liftIO $ putStrLn $ "CmdLine: git "
        --     ++ show (["--git-dir", repoPath repo] <> args)
        f "git" $ ["--git-dir", repoPath repo] <> args

runGit :: Git.MonadGit m
       => [TL.Text] -> CmdLineRepository m TL.Text
runGit = flip (doRunGit run) (return ())

runGit_ :: Git.MonadGit m
        => [TL.Text] -> CmdLineRepository m ()
runGit_ = flip (doRunGit run_) (return ())

cliRepoDoesExist :: Text -> Sh (Either Git.GitException ())
cliRepoDoesExist remoteURI = do
    setenv "SSH_ASKPASS" "echo"
    setenv "GIT_ASKPASS" "echo"
    git_ [ "ls-remote", fromStrict remoteURI ]
    ec <- lastExitCode
    return $ if ec == 0
             then Right ()
             else Left $ Git.RepositoryCannotAccess remoteURI

cliFilePathToURI :: Git.MonadGit m => FilePath -> m Text
cliFilePathToURI =
    fmap (T.append "file://localhost" . toStrict . toTextIgnore)
        . liftIO
        . F.canonicalizePath

cliPushCommitDirectly :: Git.MonadGit m
                      => CommitName m -> Text -> Text -> Maybe FilePath
                      -> CmdLineRepository m (CommitRef m)
cliPushCommitDirectly cname remoteNameOrURI remoteRefName msshCmd = do
    repo <- cliGet
    merr <- shellyNoDir $ silently $ errExit False $ do
        case msshCmd of
            Nothing -> return ()
            Just sshCmd -> setenv "GIT_SSH" . toTextIgnore
                               =<< liftIO (F.canonicalizePath sshCmd)

        eres <- cliRepoDoesExist remoteNameOrURI
        case eres of
            Left e -> return (Just e)
            Right () -> do
                git_ $ [ "--git-dir", repoPath repo ]
                    <> [ "push", fromStrict remoteNameOrURI
                       , TL.concat [ fromStrict (Git.renderCommitName cname)
                                   , ":", fromStrict remoteRefName ] ]
                r <- lastExitCode
                if r == 0
                    then return Nothing
                    else Just
                         . (\x -> if "non-fast-forward" `T.isInfixOf` x ||
                                    "Note about fast-forwards" `T.isInfixOf` x
                                 then Git.PushNotFastForward x
                                 else (Git.BackendError $
                                       "git push failed:\n" <> x))
                         . toStrict <$> lastStderr
    case merr of
        Nothing  -> do
            mcref <- Git.resolveRef remoteRefName
            case mcref of
                Nothing   -> failure (Git.BackendError $ "git push failed")
                Just cref -> return cref
        Just err -> failure err

cliResetHard :: Git.MonadGit m => Text -> CmdLineRepository m ()
cliResetHard refname =
    doRunGit run_ [ "reset", "--hard", fromStrict refname ] $ return ()

cliPullCommitDirectly :: Git.MonadGit m
                      => Text
                      -> Text
                      -> Text
                      -> Text
                      -> Maybe FilePath
                      -> CmdLineRepository m
                          (Git.MergeResult (CmdLineRepository m))
cliPullCommitDirectly remoteNameOrURI remoteRefName user email msshCmd = do
    repo     <- cliGet
    leftHead <- Git.resolveRef "HEAD"

    eres <- shellyNoDir $ silently $ errExit False $ do
        case msshCmd of
            Nothing     -> return ()
            Just sshCmd -> setenv "GIT_SSH" . toTextIgnore
                               =<< liftIO (F.canonicalizePath sshCmd)

        eres <- cliRepoDoesExist remoteNameOrURI
        case eres of
            Left e -> return (Left e)
            Right () -> do
                git_ $ [ "--git-dir", repoPath repo
                       , "config", "user.name", fromStrict user
                       ]
                git_ $ [ "--git-dir", repoPath repo
                       , "config", "user.email", fromStrict email
                       ]
                git_ $ [ "--git-dir", repoPath repo
                       , "-c", "merge.conflictstyle=merge"
                       ]
                    <> [ "pull", "--quiet"
                       , fromStrict remoteNameOrURI
                       , fromStrict remoteRefName ]
                Right <$> lastExitCode
    case eres of
        Left err -> failure err
        Right r  ->
            if r == 0
                then Git.MergeSuccess <$> getOid "HEAD"
                else case leftHead of
                    Nothing ->
                        failure (Git.BackendError
                                 "Reference missing: HEAD (left)")
                    Just lh -> recordMerge repo (Git.commitRefOid lh)
  where
    -- jww (2013-05-15): This function should not overwrite head, but simply
    -- create a detached commit and return its id.
    recordMerge repo leftHead = do
        rightHead <- getOid "MERGE_HEAD"
        xs <- shellyNoDir $ silently $ errExit False $ do
            xs <- returnConflict . TL.init
                  <$> git [ "--git-dir", repoPath repo
                          , "status", "-z", "--porcelain" ]
            forM_ (Map.assocs xs) $ uncurry (handleFile repo)
            git_ [ "--git-dir", repoPath repo
                 , "commit", "-F", ".git/MERGE_MSG" ]
            return xs
        Git.MergeConflicted
            <$> getOid "HEAD"
            <*> pure leftHead
            <*> pure rightHead
            <*> pure (Map.fromList . filter (isConflict . snd)
                                   . Map.toList $ xs)

    isConflict (Git.Deleted, Git.Deleted) = False
    isConflict (_, Git.Unchanged)         = False
    isConflict (Git.Unchanged, _)         = False
    isConflict _                          = True

    handleFile repo fp (Git.Deleted, Git.Deleted) =
        git_ [ "--git-dir", repoPath repo, "rm", "--cached", toTextIgnore fp ]
    handleFile repo fp (Git.Unchanged, Git.Deleted) =
        git_ [ "--git-dir", repoPath repo, "rm", "--cached", toTextIgnore fp ]
    handleFile repo fp (_, _) =
        git_ [ "--git-dir", repoPath repo, "add", toTextIgnore fp ]

    getOid name = do
        mref <- Git.resolveRef name
        case mref of
            Nothing  -> failure (Git.BackendError $
                                 T.append "Reference missing: " name)
            Just ref -> return (Git.commitRefOid ref)

    charToModKind 'M' = Just Git.Modified
    charToModKind 'U' = Just Git.Unchanged
    charToModKind 'A' = Just Git.Added
    charToModKind 'D' = Just Git.Deleted
    charToModKind _   = Nothing

    returnConflict xs =
        Map.fromList
            . map (\(f, (l, r)) -> (f, getModKinds l r))
            . filter (\(_, (l, r)) -> ((&&) `on` isJust) l r)
            . map (\l -> (fromText $ TL.drop 3 l,
                          (charToModKind (TL.index l 0),
                           charToModKind (TL.index l 1))))
            . init
            . TL.splitOn "\NUL" $ xs

    getModKinds l r = case (l, r) of
        (Nothing, Just x)    -> (Git.Unchanged, x)
        (Just x, Nothing)    -> (x, Git.Unchanged)
        -- 'U' really means unmerged, but it can mean both modified and
        -- unmodified as a result.  Example: UU means both sides have modified
        -- a file, but AU means that the left side added the file and the
        -- right side knows nothing about the file.
        (Just Git.Unchanged,
         Just Git.Unchanged) -> (Git.Modified, Git.Modified)
        (Just x, Just y)     -> (x, y)
        (Nothing, Nothing)   -> error "Both merge items cannot be Unchanged"

#if MIN_VERSION_shelly(1, 0, 0)
type Oid = Git.OidText

mkOid :: T.Text -> Git.OidText
mkOid = Git.OidText

getOid :: Git.OidText -> T.Text
getOid = Git.getOidT
#else
type Oid = Git.OidTextL

mkOid :: TL.Text -> Git.OidTextL
mkOid = Git.OidTextL

getOid :: Git.OidTextL -> TL.Text
getOid = Git.getOidTL
#endif

cliLookupBlob :: Git.MonadGit m
              => BlobOid m -> CmdLineRepository m (Blob m)
cliLookupBlob oid@(Tagged (getOid -> sha)) = do
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
        then return . Tagged
                    . mkOid
                    . fromStrict
                    . T.init
                    . T.decodeUtf8 $ out
        else failure Git.BlobCreateFailed

cliHashContents :: Git.MonadGit m
                => Git.BlobContents (CmdLineRepository m)
                -> CmdLineRepository m (BlobOid m)
cliHashContents b = cliDoCreateBlob b False

cliCreateBlob :: Git.MonadGit m
              => Git.BlobContents (CmdLineRepository m)
              -> CmdLineRepository m (BlobOid m)
cliCreateBlob b = cliDoCreateBlob b True

cliExistsObject :: Git.MonadGit m => Oid -> CmdLineRepository m Bool
cliExistsObject (getOid -> sha) = do
    repo <- cliGet
    shellyNoDir $ silently $ errExit False $ do
        git_ [ "--git-dir", repoPath repo, "cat-file", "-e", sha ]
        ec <- lastExitCode
        return (ec == 0)

cliTraverseCommits :: Git.MonadGit m
                   => (CommitRef m -> CmdLineRepository m a)
                   -> CommitName m
                   -> CmdLineRepository m [a]
cliTraverseCommits f name = do
    shas <- doRunGit run [ "--no-pager", "log", "--format=%H"
                         , fromStrict (Git.renderCommitName name) ]
            $ return ()
    mapM (\sha -> f =<< (Git.ByOid . Tagged <$> Git.parseOid (toStrict sha)))
        (TL.lines shas)

cliMissingObjects :: Git.MonadGit m
                  => Maybe (CommitName m) -> CommitName m
                  -> CmdLineRepository m [Object m]
cliMissingObjects mhave need = do
    shas <- doRunGit run
            ([ "--no-pager", "log", "--format=%H %T", "-z"]
             <> (case mhave of
                      Nothing   -> [ fromStrict (Git.renderCommitName need) ]
                      Just have ->
                          [ fromStrict (Git.renderCommitName have)
                          , TL.append "^"
                            (fromStrict (Git.renderCommitName need)) ]))
            $ return ()
    concat <$> mapM (go . T.words . toStrict) (TL.lines shas)
  where
    go [csha,tsha] = do
        coid <- Git.parseOid csha
        toid <- Git.parseOid tsha
        return [ Git.CommitObj (Git.ByOid (Tagged coid))
               , Git.TreeObj (Git.ByOid (Tagged toid))
               ]
    go x = failure (Git.BackendError $
                    "Unexpected output from git-log: " <> T.pack (show x))

data MutableTree m = MutableTree
    { cliTreeOid      :: IORef (Maybe (TreeOid m))
    , cliTreeContents :: IORef (HashMap Text (TreeEntry m))
    }

cliMakeTree :: IORef (Maybe (TreeOid m))
            -> IORef (HashMap Text (TreeEntry m))
            -> Tree m
cliMakeTree oid contents = MutableTree oid contents

cliNewTree :: Git.MonadGit m
           => CmdLineRepository m (Tree m)
cliNewTree = cliMakeTree <$> liftIO (newIORef Nothing)
                         <*> liftIO (newIORef HashMap.empty)

cliCloneTree :: Git.MonadGit m
             => Tree m
             -> CmdLineRepository m (Tree m)
cliCloneTree (MutableTree oid contents) =
    cliMakeTree <$> liftIO (newIORef =<< readIORef oid)
                <*> liftIO (newIORef =<< readIORef contents)

cliModifyTree :: Git.MonadGit m
              => Tree m
              -> FilePath
              -> Bool
              -> (Maybe (TreeEntry m)
                  -> Git.ModifyTreeResult (CmdLineRepository m))
              -> CmdLineRepository m (Tree m, Maybe (TreeEntry m))
cliModifyTree t path createIfNotExist f =
    fmap Git.fromModifyTreeResult
        <$> doModifyTree t (Git.splitPath path) createIfNotExist
  where
    -- Lookup the current name in this tree.  If it doesn't exist, and there
    -- are more names in the path and 'createIfNotExist' is True, create a new
    -- Tree and descend into it.  Otherwise, if it exists we'll have @Just
    -- (TreeEntry {})@, and if not we'll have Nothing.
    doModifyTree tr [] _ =
        return (tr, Git.TreeEntryPersistent . Git.TreeEntry . Git.Known $ tr)
    doModifyTree tr (name:names) createIfNotExist = do
        y' <- doLookupTreeEntry tr [name]
        y  <- if isNothing y' && createIfNotExist && not (null names)
              then Just . Git.TreeEntry . Git.Known <$> Git.newTree
              else return y'
        go tr name names y

    -- If there are no further names in the path, call the transformer
    -- function, f.  It receives a @Maybe TreeEntry@ to indicate if there was
    -- a previous entry at this path.  It should return a 'Left' value to
    -- propagate out a user-defined error, or a @Maybe TreeEntry@ to indicate
    -- whether the entry at this path should be deleted or replaced with
    -- something new.
    --
    -- NOTE: There is no provision for leaving the entry unchanged!  It is
    -- assumed to always be changed, as we have no reliable method of testing
    -- object equality that is not O(n).
    go tr name [] y = do
        let ze = f y
        liftIO $ modifyIORef (cliTreeContents tr) $ case ze of
            Git.TreeEntryNotFound     -> id
            Git.TreeEntryPersistent _ -> id
            Git.TreeEntryDeleted      -> HashMap.delete name
            Git.TreeEntryMutated z'   -> HashMap.insert name z'
        return (tr, ze)

    go tr _ _ Nothing                  = return (tr, Git.TreeEntryNotFound)
    go _ _ _ (Just Git.BlobEntry {})   = failure Git.TreeCannotTraverseBlob
    go _ _ _ (Just Git.CommitEntry {}) = failure Git.TreeCannotTraverseCommit

    -- If there are further names in the path, descend them now.  If
    -- 'createIfNotExist' was False and there is no 'Tree' under the current
    -- name, or if we encountered a 'Blob' when a 'Tree' was required, throw
    -- an exception to avoid colliding with user-defined 'Left' values.
    go tr name names (Just (Git.TreeEntry st')) = do
        st <- Git.resolveTreeRef st'
        (st'', ze) <- doModifyTree st names createIfNotExist
        case ze of
            Git.TreeEntryNotFound     -> return ()
            Git.TreeEntryPersistent _ -> return ()
            Git.TreeEntryDeleted      -> postUpdate tr st'' name
            Git.TreeEntryMutated _    -> postUpdate tr st'' name
        return (tr, ze)

    -- Lookup the current name in this tree.  If it doesn't exist, and there
    -- are more names in the path and 'createIfNotExist' is True, create a new
    -- Tree and descend into it.  Otherwise, if it exists we'll have @Just
    -- (TreeEntry {})@, and if not we'll have Nothing.
    doLookupTreeEntry tr [] = return (Just (Git.treeEntry tr))
    doLookupTreeEntry tr (name:names) = do
      y <- liftIO $ HashMap.lookup name <$> readIORef (cliTreeContents tr)
      if null names
          then return y
          else case y of
              Just (Git.BlobEntry {})   -> failure Git.TreeCannotTraverseBlob
              Just (Git.CommitEntry {}) -> failure Git.TreeCannotTraverseCommit
              Just (Git.TreeEntry st)   -> do
                  st' <- Git.resolveTreeRef st
                  doLookupTreeEntry st' names
              _ -> return Nothing

    postUpdate tr st name = liftIO $ do
        modifyIORef (cliTreeOid tr) (const Nothing)
        stc <- readIORef (cliTreeContents st)
        modifyIORef (cliTreeContents tr) $
            if HashMap.null stc
            then HashMap.delete name
            else HashMap.insert name (Git.treeEntry st)

cliLookupTree :: Git.MonadGit m => TreeOid m -> CmdLineRepository m (Tree m)
cliLookupTree oid@(Tagged (getOid -> sha)) = do
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
        in (toStrict path,
            case kind of
            "blob"   -> Git.BlobEntry (Tagged (mkOid sha)) $
                        case mode of
                            "100644" -> Git.PlainBlob
                            "100755" -> Git.ExecutableBlob
                            "120000" -> Git.SymlinkBlob
                            _        -> Git.UnknownBlob
            "commit" -> Git.CommitEntry (Tagged (mkOid sha))
            "tree"   -> Git.TreeEntry (Git.ByOid (Tagged (mkOid sha)))
            _ -> error "This cannot happen")

cliWriteTree :: Git.MonadGit m
             => Tree m -> CmdLineRepository m (TreeOid m)
cliWriteTree tree = do
    contents <- liftIO $ readIORef (cliTreeContents tree)
    rendered <- mapM renderLine (HashMap.toList contents)
    oid      <- doRunGit run ["mktree", "-z", "--missing"]
                $ setStdin $ TL.append (TL.intercalate "\NUL" rendered) "\NUL"
    return (Tagged (mkOid (TL.init oid)))
  where
    renderLine (path, Git.BlobEntry (Tagged (getOid -> sha)) kind) =
        return $ TL.concat [ case kind of
                                  Git.PlainBlob      -> "100644"
                                  Git.ExecutableBlob -> "100755"
                                  Git.SymlinkBlob    -> "120000"
                                  Git.UnknownBlob    -> "100000"
                           , " blob ", sha, "\t", fromStrict path ]
    renderLine (path, Git.CommitEntry coid) = do
        return $ TL.concat [ "160000 commit "
                           , fromStrict (Git.renderObjOid coid), "\t"
                           , fromStrict path ]
    renderLine (path, Git.TreeEntry tref) = do
        treeOid <- Git.treeRefOid tref
        return $ TL.concat
            [ "040000 tree "
            , fromStrict (Git.renderObjOid treeOid), "\t"
            , fromStrict path ]

cliTraverseEntries :: Git.MonadGit m
                   => (FilePath -> TreeEntry m -> CmdLineRepository m b)
                   -> Tree m
                   -> CmdLineRepository m [b]
cliTraverseEntries f tree = do
    Tagged (getOid -> sha) <- Git.writeTree tree
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
            "blob"   -> Git.BlobEntry (Tagged (mkOid sha)) $
                        case mode of
                            "100644" -> Git.PlainBlob
                            "100755" -> Git.ExecutableBlob
                            "120000" -> Git.SymlinkBlob
                            _        -> Git.UnknownBlob
            "commit" -> Git.CommitEntry (Tagged (mkOid sha))
            "tree"   -> Git.TreeEntry (Git.ByOid (Tagged (mkOid sha)))
            _ -> error "This cannot happen")

parseCliTime :: String -> ZonedTime
parseCliTime = fromJust . parseTime defaultTimeLocale "%s %z"

formatCliTime :: ZonedTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%s %z"

lexer :: TokenParser u
lexer = makeTokenParser haskellDef

cliLookupCommit :: Git.MonadGit m
                => CommitOid m -> CmdLineRepository m (Commit m)
cliLookupCommit (Tagged (getOid -> sha)) = do
    output <- doRunGit run ["cat-file", "--batch"]
                  $ setStdin (TL.append sha "\n")
    case parse parseOutput "" (TL.unpack output) of
        Left e  -> failure $ Git.CommitLookupFailed (T.pack (show e))
        Right c -> return c
  where
    parseOutput = do
        coid       <- Tagged . mkOid . TL.pack
                      <$> manyTill alphaNum space
        _          <- string "commit " *> manyTill digit newline
        treeOid    <- string "tree " *>
                      (Tagged . mkOid . TL.pack
                       <$> manyTill anyChar newline)
        parentOids <- many (string "parent " *>
                            (Tagged . mkOid . TL.pack
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
            , Git.commitParents   = map Git.ByOid parentOids
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
            <> [fromStrict (Git.renderObjOid treeOid)]
            <> L.concat [["-p", fromStrict (Git.renderObjOid poid)] |
                         poid <- parentOids])
           $ do mapM_ (\(var,f,val) -> setenv var (fromStrict (f val)))
                      [ ("GIT_AUTHOR_NAME",  Git.signatureName,  author)
                      , ("GIT_AUTHOR_EMAIL", Git.signatureEmail, author)
                      , ("GIT_AUTHOR_DATE",
                         formatCliTime . Git.signatureWhen, author)
                      , ("GIT_COMMITTER_NAME",  Git.signatureName,  committer)
                      , ("GIT_COMMITTER_EMAIL", Git.signatureEmail, committer)
                      , ("GIT_COMMITTER_DATE",
                         formatCliTime . Git.signatureWhen, committer)
                      ]
                setStdin (fromStrict message)

    let commit = Git.Commit
            { Git.commitOid       = Tagged (mkOid (TL.init oid))
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
        rev <- git $ [ "--git-dir", repoPath repo, "show-ref" ]
                 <> [ fromStrict (fromJust mrefName) | isJust mrefName ]
        ec  <- lastExitCode
        return $ if ec == 0
                 then Just $ map ((\(x:y:[]) -> (y,x)) . TL.words)
                           $ TL.lines rev
                 else Nothing

nameAndShaToRef :: TL.Text -> TL.Text -> Reference m
nameAndShaToRef name sha =
    Git.Reference (toStrict name)
                  (Git.RefObj (Git.ByOid (Tagged (mkOid sha))))

cliLookupRef :: Git.MonadGit m
             => Text -> CmdLineRepository m (Maybe (Reference m))
cliLookupRef refName = do
    xs <- cliShowRef (Just refName)
    let name = fromStrict refName
        ref  = maybe Nothing (lookup name) xs
    return $ maybe Nothing (Just . uncurry nameAndShaToRef .
                            \sha -> (name,sha)) ref

cliUpdateRef :: Git.MonadGit m
             => Text -> Git.RefTarget (CmdLineRepository m) (Commit m)
             -> CmdLineRepository m (Reference m)
cliUpdateRef refName refObj@(Git.RefObj commitRef) = do
    let Tagged (getOid -> sha) = Git.commitRefOid commitRef
    runGit_ ["update-ref", fromStrict refName, sha]
    return (Git.Reference refName refObj)

cliUpdateRef refName refObj@(Git.RefSymbolic targetName) = do
    runGit_ ["symbolic-ref", fromStrict refName, fromStrict targetName]
    return (Git.Reference refName refObj)

cliDeleteRef :: Git.MonadGit m => Text -> CmdLineRepository m ()
cliDeleteRef refName = runGit_ ["update-ref", "-d", fromStrict refName]

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
        rev <- git [ "--git-dir", repoPath repo
                   , "rev-parse", "--quiet", "--verify"
                   , fromStrict refName ]
        ec  <- lastExitCode
        return $ if ec == 0
            then Just (Git.ByOid (Tagged (mkOid (TL.init rev))))
            else Nothing

-- cliLookupTag :: TagOid -> CmdLineRepository Tag
-- cliLookupTag oid = undefined

cliCreateTag :: Git.MonadGit m
             => CommitOid m -> Git.Signature -> Text -> Text
             -> CmdLineRepository m (Tag m)
cliCreateTag oid@(Tagged (getOid -> sha)) tagger msg name = do
    tsha <- doRunGit run ["mktag"] $ setStdin $ TL.unlines $
        [ "object " <> sha
        , "type commit"
        , "tag " <> fromStrict name
        , "tagger " <> fromStrict (Git.signatureName tagger)
          <> " <" <> fromStrict (Git.signatureEmail tagger) <> "> "
          <> TL.pack (formatTime defaultTimeLocale "%s %z"
                      (Git.signatureWhen tagger))
        , ""] <> TL.lines (fromStrict msg)
    return $ Git.Tag (Tagged (mkOid (TL.init tsha))) (Git.ByOid oid)

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
           => Git.RepositoryFactory CmdLineRepository m Repository
cliFactory = Git.RepositoryFactory
    { Git.openRepository  = openCliRepository
    , Git.runRepository   = runCliRepository
    , Git.closeRepository = closeCliRepository
    , Git.getRepository   = cliGet
    , Git.defaultOptions  = defaultCliOptions
    , Git.startupBackend  = return ()
    , Git.shutdownBackend = return ()
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
                    git_ $ ["--git-dir", fromStrict p]
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
