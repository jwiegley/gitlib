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
import           Data.Tuple
import qualified Filesystem as F
import qualified Filesystem.Path.CurrentOS as F
import qualified Git
import qualified Git.Commit.Push as Git
import qualified Git.Tree.Builder.Pure as Pure
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

type BlobOid m         = Git.BlobOid (CmdLineRepository m)
type TreeOid m         = Git.TreeOid (CmdLineRepository m)
type CommitOid m       = Git.CommitOid (CmdLineRepository m)
type TagOid m          = Git.TagOid (CmdLineRepository m)

type Blob m            = Git.Blob (CmdLineRepository m)
type Tree m            = Git.Tree (CmdLineRepository m)
type TreeEntry m       = Git.TreeEntry (CmdLineRepository m)
type Commit m          = Git.Commit (CmdLineRepository m)
type Tag m             = Git.Tag (CmdLineRepository m)

type Object m          = Git.Object (CmdLineRepository m)
type ObjectOid m       = Git.ObjectOid (CmdLineRepository m)
type RefTarget m       = Git.RefTarget (CmdLineRepository m)

type TreeBuilder m     = Git.TreeBuilder (CmdLineRepository m)
type ModifiedBuilder m = Git.ModifiedBuilder (CmdLineRepository m)

instance Git.MonadGit m => Git.Repository (CmdLineRepository m) where
    type Oid (CmdLineRepository m) = Git.SHA
    data Tree (CmdLineRepository m) = CmdLineTree (TreeOid m)
    data Options (CmdLineRepository m) = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    parseOid = Git.textToSha

    lookupReference  = cliLookupRef
    createReference  = cliUpdateRef
    updateReference  = cliUpdateRef
    deleteReference  = cliDeleteRef
    listReferences   = cliListRefs
    lookupCommit     = cliLookupCommit
    lookupTree       = cliLookupTree
    lookupBlob       = cliLookupBlob
    lookupTag        = error "Not defined CmdLineRepository.cliLookupTag"
    lookupObject     = error "Not defined CmdLineRepository.cliLookupObject"
    existsObject     = cliExistsObject
    pushCommit       = \name _ rrefname -> Git.genericPushCommit name rrefname
    listObjects      = cliListObjects
    newTreeBuilder   = Pure.newPureTreeBuilder cliReadTree cliWriteTree
    treeEntry        = cliTreeEntry
    listTreeEntries  = cliListTreeEntries
    treeOid          = \(CmdLineTree toid) -> toid
    hashContents     = cliHashContents
    createBlob       = cliCreateBlob
    createCommit     = cliCreateCommit
    createTag        = cliCreateTag
    remoteFetch      = error "Not defined: CmdLineRepository.remoteFetch"
    deleteRepository =
        cliGet >>= liftIO . F.removeTree . Git.repoPath . repoOptions

mkOid :: Git.MonadGit m
      => forall o. TL.Text -> CmdLineRepository m (Tagged o Git.SHA)
mkOid = fmap Tagged <$> Git.textToSha . toStrict

shaToRef :: Git.MonadGit m => TL.Text -> CmdLineRepository m (RefTarget m)
shaToRef = fmap Git.RefObj . mkOid

parseCliTime :: String -> ZonedTime
parseCliTime = fromJust . parseTime defaultTimeLocale "%s %z"

formatCliTime :: ZonedTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%s %z"

lexer :: TokenParser u
lexer = makeTokenParser haskellDef

git :: [TL.Text] -> Sh TL.Text
git = run "git"

git_ :: [TL.Text] -> Sh ()
git_ = run_ "git"

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
                      => CommitOid m -> Text -> Text -> Maybe FilePath
                      -> CmdLineRepository m (CommitOid m)
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
                       , TL.concat [ fromStrict (Git.renderObjOid cname)
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
            mcref <- Git.resolveReference remoteRefName
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
    leftHead <- Git.resolveReference "HEAD"

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
                    Just lh -> recordMerge repo lh
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
        git_ [ "--git-dir", repoPath repo, "rm", "--cached", fromStrict fp ]
    handleFile repo fp (Git.Unchanged, Git.Deleted) =
        git_ [ "--git-dir", repoPath repo, "rm", "--cached", fromStrict fp ]
    handleFile repo fp (_, _) =
        git_ [ "--git-dir", repoPath repo, "add", fromStrict fp ]

    getOid name = do
        mref <- Git.resolveReference name
        case mref of
            Nothing  -> failure (Git.BackendError $
                                 T.append "Reference missing: " name)
            Just ref -> return ref

    charToModKind 'M' = Just Git.Modified
    charToModKind 'U' = Just Git.Unchanged
    charToModKind 'A' = Just Git.Added
    charToModKind 'D' = Just Git.Deleted
    charToModKind _   = Nothing

    returnConflict xs =
        Map.fromList
            . map (\(f, (l, r)) -> (f, getModKinds l r))
            . filter (\(_, (l, r)) -> ((&&) `on` isJust) l r)
            . map (\l -> (toStrict $ TL.drop 3 l,
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

cliLookupBlob :: Git.MonadGit m
              => BlobOid m -> CmdLineRepository m (Blob m)
cliLookupBlob oid@(Git.renderObjOid -> sha) = do
    repo <- cliGet
    (r,out,_) <-
        liftIO $ readProcessWithExitCode "git"
            [ "--git-dir", TL.unpack (repoPath repo)
            , "cat-file", "-p", TL.unpack (fromStrict sha) ]
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
        then mkOid . fromStrict . T.init . T.decodeUtf8 $ out
        else failure Git.BlobCreateFailed

cliHashContents :: Git.MonadGit m
                => Git.BlobContents (CmdLineRepository m)
                -> CmdLineRepository m (BlobOid m)
cliHashContents b = cliDoCreateBlob b False

cliCreateBlob :: Git.MonadGit m
              => Git.BlobContents (CmdLineRepository m)
              -> CmdLineRepository m (BlobOid m)
cliCreateBlob b = cliDoCreateBlob b True

cliExistsObject :: Git.MonadGit m
                => Git.SHA -> CmdLineRepository m Bool
cliExistsObject (Git.shaToText -> sha) = do
    repo <- cliGet
    shellyNoDir $ silently $ errExit False $ do
        git_ [ "--git-dir", repoPath repo, "cat-file", "-e", fromStrict sha ]
        ec <- lastExitCode
        return (ec == 0)

cliListObjects :: Git.MonadGit m
               => Maybe (CommitOid m) -> CommitOid m -> Bool
               -> CmdLineRepository m [ObjectOid m]
cliListObjects mhave need alsoTrees = do
    shas <- doRunGit run
            ([ "--no-pager", "log", "--format=%H %T" ]
             <> (case mhave of
                      Nothing   -> [ fromStrict (Git.renderObjOid need) ]
                      Just have ->
                          [ fromStrict (Git.renderObjOid have)
                          , TL.append "^"
                            (fromStrict (Git.renderObjOid need)) ]))
            $ return ()
    concat <$> mapM (go . T.words . toStrict) (TL.lines shas)
  where
    go [csha,tsha] = do
        coid <- Git.parseObjOid csha
        toid <- Git.parseObjOid tsha
        return $ [Git.CommitObjOid coid] <> [Git.TreeObjOid toid | alsoTrees]
    go x = failure (Git.BackendError $
                    "Unexpected output from git-log: " <> T.pack (show x))

cliReadTree :: Git.MonadGit m
            => Tree m
            -> CmdLineRepository m (Pure.EntryHashMap (CmdLineRepository m))
cliReadTree (CmdLineTree (Git.renderObjOid -> sha)) = do
    contents <- runGit ["ls-tree", "-z", fromStrict sha]
    -- Even though the tree entries are separated by \NUL, for whatever
    -- reason @git ls-tree@ also outputs a newline at the end.
    HashMap.fromList
        <$> mapM cliParseLsTree (L.init (TL.splitOn "\NUL" contents))

cliParseLsTree :: Git.MonadGit m
               => TL.Text -> CmdLineRepository m (Text, TreeEntry m)
cliParseLsTree line =
    let [prefix,path] = TL.splitOn "\t" line
        [mode,kind,sha] = TL.words prefix
    in liftM2 (,) (return (toStrict path)) $ case kind of
        "blob"   -> do
            oid <- mkOid sha
            return $ Git.BlobEntry oid $ case mode of
                "100644" -> Git.PlainBlob
                "100755" -> Git.ExecutableBlob
                "120000" -> Git.SymlinkBlob
                _        -> Git.UnknownBlob
        "commit" -> Git.CommitEntry <$> mkOid sha
        "tree"   -> Git.TreeEntry <$> mkOid sha
        _ -> failure (Git.BackendError "This cannot happen")

cliWriteTree :: Git.MonadGit m
             => Pure.EntryHashMap (CmdLineRepository m)
             -> CmdLineRepository m (TreeOid m)
cliWriteTree entMap = do
    rendered <- mapM renderLine (HashMap.toList entMap)
    when (null rendered) $ failure Git.TreeEmptyCreateFailed
    oid      <- doRunGit run ["mktree", "-z", "--missing"]
                $ setStdin $ TL.append (TL.intercalate "\NUL" rendered) "\NUL"
    mkOid (TL.init oid)
  where
    renderLine (path, Git.BlobEntry (Git.renderObjOid -> sha) kind) =
        return $ TL.concat [ case kind of
                                  Git.PlainBlob      -> "100644"
                                  Git.ExecutableBlob -> "100755"
                                  Git.SymlinkBlob    -> "120000"
                                  Git.UnknownBlob    -> "100000"
                           , " blob ", fromStrict sha, "\t", fromStrict path ]
    renderLine (path, Git.CommitEntry coid) = do
        return $ TL.concat [ "160000 commit "
                           , fromStrict (Git.renderObjOid coid), "\t"
                           , fromStrict path ]
    renderLine (path, Git.TreeEntry toid) = do
        return $ TL.concat
            [ "040000 tree "
            , fromStrict (Git.renderObjOid toid), "\t"
            , fromStrict path ]

cliLookupTree :: Git.MonadGit m => TreeOid m -> CmdLineRepository m (Tree m)
cliLookupTree oid@(Git.renderObjOid -> sha) = do
    repo <- cliGet
    ec <- shellyNoDir $ silently $ errExit False $ do
        git_ $ [ "--git-dir", repoPath repo, "cat-file", "-t", fromStrict sha ]
        lastExitCode
    if ec == 0
        then return $ CmdLineTree oid
        else failure (Git.ObjectLookupFailed sha 40)

cliTreeEntry :: Git.MonadGit m => Tree m -> Text
             -> CmdLineRepository m (Maybe (TreeEntry m))
cliTreeEntry tree fp = do
    repo <- cliGet
    mentryLines <- shellyNoDir $ silently $ errExit False $ do
        contents <- git $ [ "--git-dir", repoPath repo
                          , "ls-tree", "-z"
                          , fromStrict (Git.renderObjOid (Git.treeOid tree))
                          , "--", fromStrict fp ]
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

cliListTreeEntries :: Git.MonadGit m
                   => Tree m
                   -> CmdLineRepository m [(Text, TreeEntry m)]
cliListTreeEntries tree = do
    contents <- runGit [ "ls-tree", "-t", "-r", "-z"
                       , fromStrict (Git.renderObjOid (Git.treeOid tree)) ]
    mapM cliParseLsTree (L.init (TL.splitOn "\NUL" contents))

cliLookupCommit :: Git.MonadGit m
                => CommitOid m -> CmdLineRepository m (Commit m)
cliLookupCommit (Git.renderObjOid -> sha) = do
    output <- doRunGit run ["cat-file", "--batch"]
                  $ setStdin (TL.append (fromStrict sha) "\n")
    result <- runParserT parseOutput () "" (TL.unpack output)
    case result of
        Left e  -> failure $ Git.CommitLookupFailed (T.pack (show e))
        Right c -> return c
  where
    parseOutput = do
        coid       <- manyTill alphaNum space
        _          <- string "commit " *> manyTill digit newline
        treeOid    <- string "tree " *> manyTill anyChar newline
        parentOids <- many (string "parent " *> manyTill anyChar newline)
        author     <- parseSignature "author"
        committer  <- parseSignature "committer"
        message    <- newline *> many anyChar

        coid'  <- lift $ mkOid (TL.pack coid)
        toid'  <- lift $ mkOid (TL.pack treeOid)
        poids' <- lift $ mapM (mkOid . TL.pack) parentOids

        return Git.Commit
            { Git.commitOid       = coid'
            , Git.commitAuthor    = author
            , Git.commitCommitter = committer
            , Git.commitLog       = T.pack (init message)
            , Git.commitTree      = toid'
            , Git.commitParents   = poids'
            , Git.commitEncoding  = "utf-8"
            }

    parseSignature txt =
        Git.Signature
            <$> (string (T.unpack txt ++ " ")
                 *> (T.pack <$> manyTill anyChar (try (string " <"))))
            <*> (T.pack <$> manyTill anyChar (try (string "> ")))
            <*> (parseCliTime <$> manyTill anyChar newline)

cliCreateCommit :: Git.MonadGit m
                => [CommitOid m]
                -> TreeOid m
                -> Git.Signature
                -> Git.Signature
                -> Text
                -> Maybe Text
                -> CmdLineRepository m (Commit m)
cliCreateCommit parentOids treeOid author committer message ref = do
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

    coid <- mkOid (TL.init oid)
    let commit = Git.Commit
            { Git.commitOid       = coid
            , Git.commitAuthor    = author
            , Git.commitCommitter = committer
            , Git.commitLog       = message
            , Git.commitTree      = treeOid
            , Git.commitParents   = parentOids
            , Git.commitEncoding  = "utf-8"
            }
    when (isJust ref) $
        void $ cliUpdateRef (fromJust ref) (Git.RefObj (Git.commitOid commit))

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

cliLookupRef :: Git.MonadGit m
             => Text -> CmdLineRepository m (Maybe (RefTarget m))
cliLookupRef refName = do
    xs <- cliShowRef (Just refName)
    let name = fromStrict refName
        ref  = maybe Nothing (lookup name) xs
    maybe (return Nothing) (fmap Just . shaToRef) ref

cliUpdateRef :: Git.MonadGit m
             => Text -> Git.RefTarget (CmdLineRepository m)
             -> CmdLineRepository m ()
cliUpdateRef refName (Git.RefObj (Git.renderObjOid -> sha)) =
    runGit_ ["update-ref", fromStrict refName, fromStrict sha]

cliUpdateRef refName (Git.RefSymbolic targetName) =
    runGit_ ["symbolic-ref", fromStrict refName, fromStrict targetName]

cliDeleteRef :: Git.MonadGit m => Text -> CmdLineRepository m ()
cliDeleteRef refName = runGit_ ["update-ref", "-d", fromStrict refName]

cliListRefs :: Git.MonadGit m
            => CmdLineRepository m [Text]
cliListRefs = do
    mxs <- cliShowRef Nothing
    return $ case mxs of
        Nothing -> []
        Just xs -> map (toStrict . fst) xs

cliResolveRef :: Git.MonadGit m
              => Text -> CmdLineRepository m (Maybe (CommitOid m))
cliResolveRef refName = do
    repo <- cliGet
    (rev, ec) <- shellyNoDir $ silently $ errExit False $ do
        rev <- git [ "--git-dir", repoPath repo
                   , "rev-parse", "--quiet", "--verify"
                   , fromStrict refName ]
        ec <- lastExitCode
        return (rev, ec)
    if ec == 0
        then Just <$> mkOid (TL.init rev)
        else return Nothing

-- cliLookupTag :: TagOid -> CmdLineRepository Tag
-- cliLookupTag oid = undefined

cliCreateTag :: Git.MonadGit m
             => CommitOid m -> Git.Signature -> Text -> Text
             -> CmdLineRepository m (Tag m)
cliCreateTag oid@(Git.renderObjOid -> sha) tagger msg name = do
    tsha <- doRunGit run ["mktag"] $ setStdin $ TL.unlines $
        [ "object " <> fromStrict sha
        , "type commit"
        , "tag " <> fromStrict name
        , "tagger " <> fromStrict (Git.signatureName tagger)
          <> " <" <> fromStrict (Git.signatureEmail tagger) <> "> "
          <> TL.pack (formatTime defaultTimeLocale "%s %z"
                      (Git.signatureWhen tagger))
        , ""] <> TL.lines (fromStrict msg)
    Git.Tag <$> mkOid (TL.init tsha) <*> pure oid

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
            when (not exists && Git.repoAutoCreate opts) $ do
                liftIO $ F.createTree (fromText (fromStrict p))
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
