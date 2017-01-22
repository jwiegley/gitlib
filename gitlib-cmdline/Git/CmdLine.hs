{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.CmdLine where

import           Conduit
import           Control.Applicative hiding (many)
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Data.ByteString as B
import           Data.Foldable (for_)
import           Data.Function
import qualified Data.HashMap.Strict as HashMap
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
import qualified Filesystem.Path.CurrentOS as F
import           Data.Time.Locale.Compat (defaultTimeLocale)
import           Git
import qualified Git.Tree.Builder.Pure as Pure
import           Shelly hiding (FilePath, trace)
import           System.Directory
import           System.Exit
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

newtype CliRepo = CliRepo RepositoryOptions

cliRepoPath :: CliRepo -> TL.Text
cliRepoPath (CliRepo options) = TL.pack $ repoPath options

cliWorkingDir :: CliRepo -> Maybe TL.Text
cliWorkingDir (CliRepo options) = TL.pack <$> repoWorkingDir options

-- class HasCliRepo env where
--     getCliRepo :: env -> CliRepo

-- instance HasCliRepo CliRepo where
--     getCliRepo = id

-- instance HasCliRepo (env, CliRepo) where
--     getCliRepo = snd

instance (Applicative m, MonadThrow m, MonadIO m)
         => MonadGit CliRepo (ReaderT CliRepo m) where
    type Oid CliRepo     = SHA
    data Tree CliRepo    = CmdLineTree (TreeOid CliRepo)
    data Options CliRepo = Options

    facts = return RepositoryFacts
        { hasSymbolicReferences = True }

    getRepository    = ask
    closeRepository  = return ()
    deleteRepository = getRepository >>=
        liftIO . removeDirectoryRecursive . TL.unpack . cliRepoPath

    parseOid = textToSha

    lookupReference   = cliLookupRef
    createReference   = cliUpdateRef
    updateReference   = cliUpdateRef
    deleteReference   = cliDeleteRef
    sourceReferences  = cliSourceRefs
    lookupCommit      = cliLookupCommit
    lookupTree        = cliLookupTree
    lookupBlob        = cliLookupBlob
    lookupTag         = cliLookupTag
    lookupObject      = error "Not defined cliLookupObject"
    existsObject      = cliExistsObject
    sourceObjects     = cliSourceObjects
    newTreeBuilder    = Pure.newPureTreeBuilder cliReadTree cliWriteTree
    treeOid (CmdLineTree toid) = return toid
    treeEntry         = cliTreeEntry
    sourceTreeEntries = cliSourceTreeEntries
    hashContents      = cliHashContents
    createBlob        = cliCreateBlob
    createCommit      = cliCreateCommit
    createTag         = cliCreateTag

    diffContentsWithTree = error "Not defined cliDiffContentsWithTree"

type MonadCli m = (Applicative m, MonadThrow m, MonadIO m)

mkOid :: MonadCli m => forall o. TL.Text -> ReaderT CliRepo m (Tagged o SHA)
mkOid = fmap Tagged <$> textToSha . toStrict

shaToRef :: MonadCli m => TL.Text -> ReaderT CliRepo m (RefTarget CliRepo)
shaToRef = fmap (RefObj . untag) . mkOid

parseCliTime :: String -> ZonedTime
parseCliTime = fromJust . parseTime defaultTimeLocale "%s %z"

formatCliTime :: ZonedTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%s %z"

lexer :: TokenParser u
lexer = makeTokenParser haskellDef

gitStdOpts :: CliRepo -> [TL.Text]
gitStdOpts repo = [ "--git-dir", cliRepoPath repo ]
    ++ maybe [] (\w -> [ "--work-tree", w ]) (cliWorkingDir repo)

git :: CliRepo -> [TL.Text] -> Sh TL.Text
git repo args = run "git" $ gitStdOpts repo ++ args

git_ :: CliRepo -> [TL.Text] -> Sh ()
git_ repo args = run_ "git" $ gitStdOpts repo ++ args

doRunGit :: MonadCli m
         => (F.FilePath -> [TL.Text] -> Sh a) -> [TL.Text] -> Sh ()
         -> ReaderT CliRepo m a
doRunGit f args act = do
    repo <- getRepository
    shellyNoDir $ silently $
        act >> f "git" (gitStdOpts repo <> args)

runGit :: MonadCli m => [TL.Text] -> ReaderT CliRepo m TL.Text
runGit = flip (doRunGit run) (return ())

runGit_ :: MonadCli m => [TL.Text] -> ReaderT CliRepo m ()
runGit_ = flip (doRunGit run_) (return ())

cliRepoDoesExist :: CliRepo -> Text -> Sh (Either GitException ())
cliRepoDoesExist repo remoteURI = do
    setenv "SSH_ASKPASS" "echo"
    setenv "GIT_ASKPASS" "echo"
    git_ repo [ "ls-remote", fromStrict remoteURI ]
    ec <- lastExitCode
    return $ if ec == 0
             then Right ()
             else Left $ RepositoryCannotAccess remoteURI

cliFilePathToURI :: (Functor m, MonadIO m) => FilePath -> m FilePath
cliFilePathToURI = fmap ("file://localhost" <>) . liftIO . canonicalizePath

cliPushCommit :: MonadCli m
              => CommitOid CliRepo -> Text -> Text -> Maybe FilePath
              -> ReaderT CliRepo m (CommitOid CliRepo)
cliPushCommit cname remoteNameOrURI remoteRefName msshCmd = do
    repo <- getRepository
    merr <- shellyNoDir $ silently $ errExit False $ do
        for_ msshCmd $ \sshCmd ->
            setenv "GIT_SSH" . TL.pack =<< liftIO (canonicalizePath sshCmd)

        eres <- cliRepoDoesExist repo remoteNameOrURI
        case eres of
            Left e -> return $ Just e
            Right () -> do
                git_ repo [ "push", fromStrict remoteNameOrURI
                          , TL.concat [ fromStrict (renderObjOid cname)
                                      , ":", fromStrict remoteRefName
                                      ]
                          ]
                r <- lastExitCode
                if r == 0
                    then return Nothing
                    else Just
                         . (\x -> if "non-fast-forward" `T.isInfixOf` x ||
                                    "Note about fast-forwards" `T.isInfixOf` x
                                 then PushNotFastForward x
                                 else BackendError $
                                          "git push failed:\n" <> x)
                         . toStrict <$> lastStderr
    case merr of
        Nothing  -> do
            mcref <- resolveReference remoteRefName
            case mcref of
                Nothing   -> throwM $ BackendError "git push failed"
                Just cref -> return $ Tagged cref
        Just err -> throwM err

cliResetHard :: MonadCli m => Text -> ReaderT CliRepo m ()
cliResetHard refname =
    doRunGit run_ [ "reset", "--hard", fromStrict refname ] $ return ()

cliPullCommit :: MonadCli m
              => Text -> Text -> Text -> Text -> Maybe FilePath
              -> ReaderT CliRepo m (MergeResult CliRepo)
cliPullCommit remoteNameOrURI remoteRefName user email msshCmd = do
    repo     <- getRepository
    leftHead <- fmap Tagged <$> cliResolveRef "HEAD"
    eres     <- shellyNoDir $ silently $ errExit False $ do
        for_ msshCmd $ \sshCmd ->
            setenv "GIT_SSH" . TL.pack =<< liftIO (canonicalizePath sshCmd)
        eres <- cliRepoDoesExist repo remoteNameOrURI
        case eres of
            Left e -> return (Left e)
            Right () -> do
                git_ repo [ "config", "user.name", fromStrict user ]
                git_ repo [ "config", "user.email", fromStrict email ]
                git_ repo $
                       [ "-c", "merge.conflictstyle=merge" ]
                    <> [ "pull", "--quiet"
                       , fromStrict remoteNameOrURI
                       , fromStrict remoteRefName
                       ]
                Right <$> lastExitCode
    case eres of
        Left err -> throwM err
        Right r  ->
            if r == 0
                then MergeSuccess <$> (Tagged <$> getOid "HEAD")
                else case leftHead of
                    Nothing ->
                        throwM (BackendError
                                 "Reference missing: HEAD (left)")
                    Just lh -> recordMerge lh
  where
    -- jww (2013-05-15): This function should not overwrite head, but simply
    -- create a detached commit and return its id.
    recordMerge :: MonadCli m
                => CommitOid CliRepo -> ReaderT CliRepo m (MergeResult CliRepo)
    recordMerge leftHead = do
        repo <- getRepository
        rightHead <- Tagged <$> getOid "MERGE_HEAD"
        xs <- shellyNoDir $ silently $ errExit False $ do
            xs <- returnConflict . TL.init
                  <$> git repo [ "status", "-z", "--porcelain" ]
            forM_ (Map.assocs xs) $ uncurry (handleFile repo)
            git_ repo [ "commit", "-F", ".git/MERGE_MSG" ]
            return xs
        MergeConflicted
            <$> (Tagged <$> getOid "HEAD")
            <*> pure leftHead
            <*> pure rightHead
            <*> pure (Map.filter isConflict xs)

    isConflict (Deleted, Deleted) = False
    isConflict (_, Unchanged)         = False
    isConflict (Unchanged, _)         = False
    isConflict _                          = True

    handleFile repo fp (Deleted, Deleted) =
        git_ repo [ "rm", "--cached", fromStrict . T.decodeUtf8 $ fp ]
    handleFile repo fp (Unchanged, Deleted) =
        git_ repo [ "rm", "--cached", fromStrict . T.decodeUtf8 $ fp ]
    handleFile repo fp (_, _) =
        git_ repo [ "add", fromStrict . T.decodeUtf8 $ fp ]

    getOid :: MonadCli m => Text -> ReaderT CliRepo m (Oid CliRepo)
    getOid name = do
        mref <- cliResolveRef name
        case mref of
            Nothing  -> throwM $ BackendError
                                $ T.append "Reference missing: " name
            Just ref -> return ref

    charToModKind 'M' = Just Modified
    charToModKind 'U' = Just Unchanged
    charToModKind 'A' = Just Added
    charToModKind 'D' = Just Deleted
    charToModKind _   = Nothing

    returnConflict xs =
        Map.fromList
            . map (\(f, (l, r)) -> (f, getModKinds l r))
            . filter (\(_, (l, r)) -> ((&&) `on` isJust) l r)
            -- jww (2013-08-25): What is the correct way to interpret the
            -- output from "git status"?
            . map (\l -> (T.encodeUtf8 . toStrict . TL.drop 3 $ l,
                          (charToModKind (TL.index l 0),
                           charToModKind (TL.index l 1))))
            . init
            . TL.splitOn "\NUL" $ xs

    getModKinds l r = case (l, r) of
        (Nothing, Just x)    -> (Unchanged, x)
        (Just x, Nothing)    -> (x, Unchanged)
        -- 'U' really means unmerged, but it can mean both modified and
        -- unmodified as a result.  Example: UU means both sides have modified
        -- a file, but AU means that the left side added the file and the
        -- right side knows nothing about the file.
        (Just Unchanged,
         Just Unchanged) -> (Modified, Modified)
        (Just x, Just y)     -> (x, y)
        (Nothing, Nothing)   -> error "Both merge items cannot be Unchanged"

cliLookupBlob :: MonadCli m
              => BlobOid CliRepo
              -> ReaderT CliRepo m (Blob CliRepo (ReaderT CliRepo m))
cliLookupBlob oid@(renderObjOid -> sha) = do
    repo <- getRepository
    (r,out,_) <-
        liftIO $ readProcessWithExitCode "git"
            (map TL.unpack (gitStdOpts repo)
                 ++ [ "cat-file", "-p", TL.unpack (fromStrict sha) ])
            B.empty
    if r == ExitSuccess
        then return (Blob oid (BlobString out))
        else throwM BlobLookupFailed

cliDoCreateBlob :: MonadCli m
                => BlobContents (ReaderT CliRepo m)
                -> Bool
                -> ReaderT CliRepo m (BlobOid CliRepo)
cliDoCreateBlob b persist = do
    repo      <- getRepository
    bs        <- blobContentsToByteString b
    (r,out,_) <-
        liftIO $ readProcessWithExitCode "git"
            (map TL.unpack (gitStdOpts repo)
                 ++ [ "hash-object" ]
                 ++ ["-w" | persist]
                 ++ ["--stdin"])
            bs
    if r == ExitSuccess
        then mkOid . fromStrict . T.init . T.decodeUtf8 $ out
        else throwM $ BlobCreateFailed "Failed to create blob"

cliHashContents :: MonadCli m
                => BlobContents (ReaderT CliRepo m)
                -> ReaderT CliRepo m (BlobOid CliRepo)
cliHashContents b = cliDoCreateBlob b False

cliCreateBlob :: MonadCli m
              => BlobContents (ReaderT CliRepo m)
              -> ReaderT CliRepo m (BlobOid CliRepo)
cliCreateBlob b = cliDoCreateBlob b True

cliExistsObject :: MonadCli m => SHA -> ReaderT CliRepo m Bool
cliExistsObject (shaToText -> sha) = do
    repo <- getRepository
    shellyNoDir $ silently $ errExit False $ do
        git_ repo [ "cat-file", "-e", fromStrict sha ]
        ec <- lastExitCode
        return (ec == 0)

cliSourceObjects :: MonadCli m
                 => Maybe (CommitOid CliRepo) -> CommitOid CliRepo -> Bool
                 -> Producer (ReaderT CliRepo m) (ObjectOid CliRepo)
cliSourceObjects mhave need alsoTrees = do
    shas <- lift $ doRunGit run
            ([ "--no-pager", "log", "--format=%H %T" ]
             <> (case mhave of
                      Nothing   -> [ fromStrict (renderObjOid need) ]
                      Just have ->
                          [ fromStrict (renderObjOid have)
                          , TL.append "^"
                            (fromStrict (renderObjOid need)) ]))
            $ return ()
    mapM_ (go . T.words . toStrict) (TL.lines shas)
  where
    go [csha,tsha] = do
        coid <- lift $ parseObjOid csha
        yield $ CommitObjOid coid
        when alsoTrees $ do
            toid <- lift $ parseObjOid tsha
            yield $ TreeObjOid toid

    go x = throwM (BackendError $
                   "Unexpected output from git-log: " <> T.pack (show x))

cliReadTree :: MonadCli m
            => Tree CliRepo -> ReaderT CliRepo m (Pure.EntryHashMap CliRepo)
cliReadTree (CmdLineTree (renderObjOid -> sha)) = do
    contents <- runGit ["ls-tree", "-z", fromStrict sha]
    -- Even though the tree entries are separated by \NUL, for whatever
    -- reason @git ls-tree@ also outputs a newline at the end.
    HashMap.fromList
        <$> mapM cliParseLsTree (L.init (TL.splitOn "\NUL" contents))

cliParseLsTree :: MonadCli m
               => TL.Text -> ReaderT CliRepo m (TreeFilePath, TreeEntry CliRepo)
cliParseLsTree line =
    let [prefix,path] = TL.splitOn "\t" line
        [mode,kind,sha] = TL.words prefix
    in liftM2 (,) (return (T.encodeUtf8 . toStrict $ path)) $ case kind of
        "blob"   -> do
            oid <- mkOid sha
            BlobEntry oid <$> case mode of
                "100644" -> return PlainBlob
                "100755" -> return ExecutableBlob
                "120000" -> return SymlinkBlob
                _        -> throwM $ BackendError $
                    "Unknown blob mode: " <> T.pack (show mode)
        "commit" -> CommitEntry <$> mkOid sha
        "tree"   -> TreeEntry <$> mkOid sha
        _ -> throwM $ BackendError "This cannot happen"

cliWriteTree :: MonadCli m
             => Pure.EntryHashMap CliRepo -> ReaderT CliRepo m (TreeOid CliRepo)
cliWriteTree entMap = do
    rendered <- mapM renderLine (HashMap.toList entMap)
    when (null rendered) $ throwM TreeEmptyCreateFailed
    oid      <- doRunGit run [ "mktree", "-z", "--missing" ]
                $ setStdin $ TL.append (TL.intercalate "\NUL" rendered) "\NUL"
    mkOid (TL.init oid)
  where
    renderLine (fromStrict . T.decodeUtf8 -> path,
                BlobEntry (renderObjOid -> sha) kind) =
        return $ TL.concat
            [ case kind of
                   PlainBlob      -> "100644"
                   ExecutableBlob -> "100755"
                   SymlinkBlob    -> "120000"
            , " blob ", fromStrict sha, "\t", path
            ]
    renderLine (fromStrict . T.decodeUtf8 -> path, CommitEntry coid) =
        return $ TL.concat
            [ "160000 commit "
            , fromStrict (renderObjOid coid), "\t"
            , path
            ]
    renderLine (fromStrict . T.decodeUtf8 -> path, TreeEntry toid) =
        return $ TL.concat
            [ "040000 tree "
            , fromStrict (renderObjOid toid), "\t"
            , path
            ]

cliLookupTree :: MonadCli m
              => TreeOid CliRepo -> ReaderT CliRepo m (Tree CliRepo)
cliLookupTree oid@(renderObjOid -> sha) = do
    repo <- getRepository
    ec <- shellyNoDir $ silently $ errExit False $ do
        git_ repo [ "cat-file", "-t", fromStrict sha ]
        lastExitCode
        -- res <- git repo [ "cat-file", "-t", fromStrict sha ]
        -- ec <- lastExitCode
        -- return $ if ec == 0
        --          then if res == "tree"
        --               then 0
        --               else (-1)
        --          else ec
    if ec == 0
        then return $ CmdLineTree oid
        else throwM (ObjectLookupFailed sha 40)

cliTreeEntry :: MonadCli m
             => Tree CliRepo -> TreeFilePath
             -> ReaderT CliRepo m (Maybe (TreeEntry CliRepo))
cliTreeEntry tree fp = do
    repo <- getRepository
    toid <- treeOid tree
    mentryLines <- shellyNoDir $ silently $ errExit False $ do
        contents <- git repo [ "ls-tree", "-z"
                             , fromStrict (renderObjOid toid)
                             , "--", fromStrict . T.decodeUtf8 $ fp
                             ]
        ec <- lastExitCode
        return $ if ec == 0
                 then Just $ L.init (TL.splitOn "\NUL" contents)
                 else Nothing
    case mentryLines of
        Nothing -> return Nothing
        Just entryLines -> do
            entries <- mapM cliParseLsTree entryLines
            return $ case entries of
                []        -> Nothing
                ((_,x):_) -> Just x

cliSourceTreeEntries :: MonadCli m
                     => Tree CliRepo
                     -> Producer (ReaderT CliRepo m) (TreeFilePath, TreeEntry CliRepo)
cliSourceTreeEntries tree = do
    contents <- lift $ do
        toid <- treeOid tree
        runGit [ "ls-tree", "-t", "-r", "-z"
               , fromStrict (renderObjOid toid)
               ]
    forM_ (L.init (TL.splitOn "\NUL" contents)) $
        yield <=< lift . cliParseLsTree

cliLookupCommit :: MonadCli m
                => CommitOid CliRepo -> ReaderT CliRepo m (Commit CliRepo)
cliLookupCommit (renderObjOid -> sha) = do
    output <- doRunGit run ["cat-file", "--batch"] $
        setStdin (TL.append (fromStrict sha) "\n")
    result <- runParserT parseOutput () "" (TL.unpack output)
    case result of
        Left e  -> throwM $ CommitLookupFailed (T.pack (show e))
        Right c -> return c
  where
    parseOutput :: (Stream s (ReaderT CliRepo m) Char, MonadCli m)
                => ParsecT s u (ReaderT CliRepo m) (Commit CliRepo)
    parseOutput = do
        coid       <- manyTill alphaNum space
        _          <- string "commit " *> manyTill digit newline
        treeOid    <- string "tree " *> manyTill anyChar newline
        parentOids <- many (string "parent " *> manyTill anyChar newline)
        author     <- parseSignature "author"
        committer  <- parseSignature "committer"
        message    <- newline *> many anyChar

        lift $ do
            coid'  <- mkOid (TL.pack coid)
            toid'  <- mkOid (TL.pack treeOid)
            poids' <- mapM (mkOid . TL.pack) parentOids
            return Commit
                { commitOid       = coid'
                , commitAuthor    = author
                , commitCommitter = committer
                , commitLog       = T.pack (init message)
                , commitTree      = toid'
                , commitParents   = poids'
                , commitEncoding  = "utf-8"
                }

    parseSignature txt =
        Signature
            <$> (string (T.unpack txt ++ " ")
                 *> (T.pack <$> manyTill anyChar (try (string " <"))))
            <*> (T.pack <$> manyTill anyChar (try (string "> ")))
            <*> (parseCliTime <$> manyTill anyChar newline)

cliCreateCommit :: MonadCli m
                => [CommitOid CliRepo]
                -> TreeOid CliRepo
                -> Signature
                -> Signature
                -> Text
                -> Maybe Text
                -> ReaderT CliRepo m (Commit CliRepo)
cliCreateCommit parentOids treeOid author committer message ref = do
    oid <- doRunGit run
           (["commit-tree"]
            <> [fromStrict (renderObjOid treeOid)]
            <> L.concat [["-p", fromStrict (renderObjOid poid)] |
                         poid <- parentOids])
           $ do mapM_ (\(var,f,val) -> setenv var (fromStrict (f val)))
                      [ ("GIT_AUTHOR_NAME",  signatureName,  author)
                      , ("GIT_AUTHOR_EMAIL", signatureEmail, author)
                      , ("GIT_AUTHOR_DATE",
                         formatCliTime . signatureWhen, author)
                      , ("GIT_COMMITTER_NAME",  signatureName,  committer)
                      , ("GIT_COMMITTER_EMAIL", signatureEmail, committer)
                      , ("GIT_COMMITTER_DATE",
                         formatCliTime . signatureWhen, committer)
                      ]
                setStdin (fromStrict message)

    coid <- mkOid (TL.init oid)
    let commit = Commit
            { commitOid       = coid
            , commitAuthor    = author
            , commitCommitter = committer
            , commitLog       = message
            , commitTree      = treeOid
            , commitParents   = parentOids
            , commitEncoding  = "utf-8"
            }
    when (isJust ref) $
        void $ cliUpdateRef (fromJust ref)
            (RefObj (untag (commitOid commit)))

    return commit

data CliObjectRef = CliObjectRef
    { objectRefType :: Text
    , objectRefSha  :: Text } deriving Show

data CliReference = CliReference
    { referenceRef    :: Text
    , referenceObject :: CliObjectRef } deriving Show

cliShowRef :: MonadCli m
           => Maybe Text -> ReaderT CliRepo m (Maybe [(TL.Text,TL.Text)])
cliShowRef mrefName = do
    repo <- getRepository
    shellyNoDir $ silently $ errExit False $ do
        rev <- git repo $ [ "show-ref" ]
                 <> [ fromStrict (fromJust mrefName) | isJust mrefName ]
        ec  <- lastExitCode
        return $ if ec == 0
                 then Just $ map ((\(x:y:[]) -> (y,x)) . TL.words)
                           $ TL.lines rev
                 else Nothing

cliLookupRef :: MonadCli m
             => Text -> ReaderT CliRepo m (Maybe (RefTarget CliRepo))
cliLookupRef refName = do
    repo <- getRepository
    (ec,rev) <- shellyNoDir $ silently $ errExit False $ do
        rev <- git repo [ "symbolic-ref", fromStrict refName ]
        ec  <- lastExitCode
        return (ec,rev)
    if ec == 0
        then return . Just . RefSymbolic . toStrict . TL.init $ rev
        else fmap RefObj <$> cliResolveRef refName

cliUpdateRef :: MonadCli m => Text -> RefTarget CliRepo -> ReaderT CliRepo m ()
cliUpdateRef refName (RefObj (renderOid -> sha)) =
    runGit_ ["update-ref", fromStrict refName, fromStrict sha]

cliUpdateRef refName (RefSymbolic targetName) =
    runGit_ ["symbolic-ref", fromStrict refName, fromStrict targetName]

cliDeleteRef :: MonadCli m => Text -> ReaderT CliRepo m ()
cliDeleteRef refName = runGit_ ["update-ref", "-d", fromStrict refName]

cliSourceRefs :: MonadCli m => Producer (ReaderT CliRepo m) Text
cliSourceRefs = do
    mxs <- lift $ cliShowRef Nothing
    yieldMany $ case mxs of
        Nothing -> []
        Just xs -> map (toStrict . fst) xs

cliResolveRef :: MonadCli m => Text -> ReaderT CliRepo m (Maybe (Oid CliRepo))
cliResolveRef refName = do
    repo <- getRepository
    (rev, ec) <- shellyNoDir $ silently $ errExit False $ do
        rev <- git repo [ "rev-parse", "--quiet", "--verify"
                        , fromStrict refName
                        ]
        ec <- lastExitCode
        return (rev, ec)
    if ec == 0
        then Just <$> textToSha (toStrict (TL.init rev))
        else return Nothing

cliLookupTag :: MonadCli m
             => TagOid CliRepo -> ReaderT CliRepo m (Tag CliRepo)
cliLookupTag tag@(renderObjOid -> sha) = do
    repo <- getRepository
    (r,out,_) <-
        liftIO $ readProcessWithExitCode  "git"
            (map TL.unpack (gitStdOpts repo)
                ++ ["cat-file", "tag", TL.unpack (fromStrict sha)])
            B.empty
    if r == ExitSuccess
        then do
            p <- runParserT parseOutput () "" (BC.unpack out)
            case p of
                Left e -> throwM $ TagLookupFailed $ T.pack $ show e
                Right oid -> return $ Tag tag oid
        else throwM $ TagLookupFailed ""
  where
    parseOutput = do
        oid <- (string "object " *> manyTill alphaNum newline)
        lift $ mkOid $ TL.pack oid

cliCreateTag :: MonadCli m
             => CommitOid CliRepo -> Signature -> Text -> Text
             -> ReaderT CliRepo m (Tag CliRepo)
cliCreateTag oid@(renderObjOid -> sha) tagger msg name = do
    tsha <- doRunGit run ["mktag"] $ setStdin $ TL.unlines $
        [ "object " <> fromStrict sha
        , "type commit"
        , "tag " <> fromStrict name
        , "tagger " <> fromStrict (signatureName tagger)
          <> " <" <> fromStrict (signatureEmail tagger) <> "> "
          <> TL.pack (formatTime defaultTimeLocale "%s %z"
                      (signatureWhen tagger))
        , ""] <> TL.lines (fromStrict msg)
    Tag <$> mkOid (TL.init tsha) <*> pure oid

cliWorkingTreeDirty :: MonadCli m => ReaderT CliRepo m Bool
cliWorkingTreeDirty = do
    status <- runGit [ "status", "-s", "-uno", "-z" ]
    return $ TL.length status > 0

cliFactory :: MonadCli m => RepositoryFactory (ReaderT CliRepo m) m CliRepo
cliFactory = RepositoryFactory
    { openRepository  = openCliRepository
    , runRepository   = flip runReaderT
    }

openCliRepository :: MonadIO m => RepositoryOptions -> m CliRepo
openCliRepository opts = do
    let path = repoPath opts
    exists <- liftIO $ doesDirectoryExist path
    when (not exists && repoAutoCreate opts) $ do
        liftIO $ createDirectoryIfMissing True path
        shellyNoDir $ silently $
            git_ (CliRepo opts) $ ["--bare" | repoIsBare opts] <> ["init"]
    return $ CliRepo opts

-- Cli.hs
