{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TupleSections #-}

{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Hit where

import           Conduit
import           Control.Applicative hiding (many)
import qualified Control.Exception as X
import           Control.Monad
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Char8 as BC
import           Data.Foldable (for_)
import           Data.Function
import qualified Data.Git as DG
import qualified Data.Git.Named as DGN
import qualified Data.Git.Ref as DGF
import qualified Data.Git.Repository as DGR
import qualified Data.Git.Revision as DGV
import qualified Data.Git.Storage as DGS
import qualified Data.Git.Storage.Object as DGO
import qualified Data.Git.Types as DGT
import qualified Data.HashMap.Strict as HashMap
import           Data.List as L
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text as TL
import           Data.Time
import qualified Filesystem.Path.CurrentOS as F
import           Git
import qualified Git.Tree.Builder.Pure as Pure
import           Shelly hiding (FilePath, trace)
import           System.Directory
import           System.Locale (defaultTimeLocale)
import           System.IO (withFile, hPutStrLn, IOMode(WriteMode))
import           Text.Parsec.Char
import           Text.Parsec.Combinator
import           Text.Parsec.Language (haskellDef)
import           Text.Parsec.Prim
import           Text.Parsec.Token

toStrict :: TL.Text -> T.Text
toStrict = id

fromStrict :: T.Text -> TL.Text
fromStrict = id

data HitRepo = HitRepo
    { hitOptions :: RepositoryOptions
    , hitGit     :: DGS.Git
    }

hitRepoPath :: HitRepo -> TL.Text
hitRepoPath = TL.pack . repoPath . hitOptions

hitWorkingDir :: HitRepo -> Maybe TL.Text
hitWorkingDir = fmap TL.pack . repoWorkingDir . hitOptions

refToSHA :: DG.Ref -> SHA
refToSHA = SHA . DGF.toBinary

refToOid :: DG.Ref -> Tagged a (Oid HitRepo)
refToOid = Tagged . refToSHA

oidToRef :: Tagged a (Oid HitRepo) -> DG.Ref
oidToRef = DGF.fromBinary . getSHA . untag

oidToHex :: Tagged a (Oid HitRepo) -> Text
oidToHex = shaToText . untag

--- This is a duplicate of an unexported function in Data.Git.Named
toRefTy :: String -> DGN.RefSpecTy
toRefTy s
    | "refs/tags/" `isPrefixOf` s    = DGN.RefTag $ DGN.RefName $ drop 10 s
    | "refs/heads/" `isPrefixOf` s   = DGN.RefBranch $ DGN.RefName $ drop 11 s
    | "refs/remotes/" `isPrefixOf` s = DGN.RefRemote $ DGN.RefName $ drop 13 s
    | "refs/patches/" `isPrefixOf` s = DGN.RefPatches $ drop 13 s
    | "refs/stash" == s              = DGN.RefStash
    | "HEAD" == s                    = DGN.RefHead
    | "ORIG_HEAD" == s               = DGN.RefOrigHead
    | "FETCH_HEAD" == s              = DGN.RefFetchHead
    | otherwise                      = DGN.RefOther $ s


instance (Applicative m, MonadThrow m, MonadIO m)
         => MonadGit HitRepo (ReaderT HitRepo m) where
    type Oid HitRepo     = SHA  -- TODO maybe use DG.Ref directly here?
    data Tree HitRepo    = CmdLineTree (TreeOid HitRepo)
    data Options HitRepo = Options

    facts = return RepositoryFacts
        { hasSymbolicReferences = True }

    getRepository    = ask
    closeRepository  = getRepository >>= liftIO . DGS.closeRepo . hitGit
    deleteRepository = getRepository >>=
        liftIO . removeDirectoryRecursive . TL.unpack . hitRepoPath

    parseOid = textToSha

    lookupReference   = cliLookupRef
    createReference   = cliUpdateRef
    updateReference   = cliUpdateRef
    deleteReference   = cliDeleteRef
    sourceReferences  = cliSourceRefs
    lookupCommit      = cliLookupCommit
    lookupTree        = hitLookupTree
    lookupBlob        = hitLookupBlob
    lookupTag         = error "Not defined cliLookupTag"
    lookupObject      = error "Not defined cliLookupObject"
    existsObject      = hitExistsObject
    sourceObjects     = cliSourceObjects
    newTreeBuilder    = Pure.newPureTreeBuilder cliReadTree cliWriteTree
    treeOid (CmdLineTree toid) = return toid
    treeEntry         = hitTreeEntry
    sourceTreeEntries = cliSourceTreeEntries
    hashContents      = hitHashContents
    createBlob        = hitCreateBlob
    createCommit      = cliCreateCommit
    createTag         = cliCreateTag

    diffContentsWithTree = error "Not defined cliDiffContentsWithTree"

type MonadHit m = (Applicative m, MonadThrow m, MonadIO m)

mkOid :: MonadHit m => forall o. TL.Text -> ReaderT HitRepo m (Tagged o SHA)
mkOid = fmap Tagged <$> textToSha . toStrict

shaToRef :: MonadHit m => TL.Text -> ReaderT HitRepo m (RefTarget HitRepo)
shaToRef = fmap (RefObj . untag) . mkOid

parseCliTime :: String -> ZonedTime
parseCliTime = fromJust . parseTime defaultTimeLocale "%s %z"

formatCliTime :: ZonedTime -> Text
formatCliTime = T.pack . formatTime defaultTimeLocale "%s %z"

lexer :: TokenParser u
lexer = makeTokenParser haskellDef

gitStdOpts :: HitRepo -> [TL.Text]
gitStdOpts repo = [ "--git-dir", hitRepoPath repo ]
    ++ maybe [] (\w -> [ "--work-tree", w ]) (hitWorkingDir repo)

git :: HitRepo -> [TL.Text] -> Sh TL.Text
git repo args = run "git" $ gitStdOpts repo ++ args

git_ :: HitRepo -> [TL.Text] -> Sh ()
git_ repo args = run_ "git" $ gitStdOpts repo ++ args

doRunGit :: MonadHit m
         => (F.FilePath -> [TL.Text] -> Sh a) -> [TL.Text] -> Sh ()
         -> ReaderT HitRepo m a
doRunGit f args act = do
    repo <- getRepository
    shelly $ silently $
        act >> f "git" (gitStdOpts repo <> args)

runGit :: MonadHit m => [TL.Text] -> ReaderT HitRepo m TL.Text
runGit = flip (doRunGit run) (return ())

runGit_ :: MonadHit m => [TL.Text] -> ReaderT HitRepo m ()
runGit_ = flip (doRunGit run_) (return ())

cliRepoDoesExist :: HitRepo -> Text -> Sh (Either GitException ())
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

cliPushCommit :: MonadHit m
              => CommitOid HitRepo -> Text -> Text -> Maybe FilePath
              -> ReaderT HitRepo m (CommitOid HitRepo)
cliPushCommit cname remoteNameOrURI remoteRefName msshCmd = do
    repo <- getRepository
    merr <- shelly $ silently $ errExit False $ do
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

cliResetHard :: MonadHit m => Text -> ReaderT HitRepo m ()
cliResetHard refname =
    doRunGit run_ [ "reset", "--hard", fromStrict refname ] $ return ()

cliPullCommit :: MonadHit m
              => Text -> Text -> Text -> Text -> Maybe FilePath
              -> ReaderT HitRepo m (MergeResult HitRepo)
cliPullCommit remoteNameOrURI remoteRefName user email msshCmd = do
    repo     <- getRepository
    leftHead <- fmap Tagged <$> hitResolveRef "HEAD"
    eres     <- shelly $ silently $ errExit False $ do
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
    recordMerge :: MonadHit m
                => CommitOid HitRepo -> ReaderT HitRepo m (MergeResult HitRepo)
    recordMerge leftHead = do
        repo <- getRepository
        rightHead <- Tagged <$> getOid "MERGE_HEAD"
        xs <- shelly $ silently $ errExit False $ do
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

    getOid :: MonadHit m => Text -> ReaderT HitRepo m (Oid HitRepo)
    getOid name = do
        mref <- hitResolveRef name
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

hitLookupBlob :: MonadHit m
              => BlobOid HitRepo
              -> ReaderT HitRepo m (Blob HitRepo (ReaderT HitRepo m))
hitLookupBlob oid = do
    g <- hitGit <$> getRepository
    mobj <- liftIO $ DG.getObject g (oidToRef oid) True
    maybe (throwM BlobLookupFailed) -- TODO: must we handle ObjDeltaOfs, ObjDeltaRef?
          (\(DGO.ObjBlob b) -> return $ Blob oid $ BlobStringLazy $ DGT.blobGetContent b)
          mobj

hitHashContents :: MonadHit m
                => BlobContents (ReaderT HitRepo m)
                -> ReaderT HitRepo m (BlobOid HitRepo)
hitHashContents b = do
    bs <- blobContentsToLazyByteString b
    let sz = fromIntegral $ BL.length bs
    let ref = DGO.objectHash DGO.TypeBlob sz bs
    return $ refToOid ref

hitCreateBlob :: MonadHit m
              => BlobContents (ReaderT HitRepo m)
              -> ReaderT HitRepo m (BlobOid HitRepo)
hitCreateBlob b = do
    g <- hitGit <$> getRepository
    bs <- blobContentsToLazyByteString b
    let obj = DGO.ObjBlob (DG.Blob bs)
    ref <- liftIO $ DG.setObject g obj
    return $ refToOid ref

hitExistsObject :: MonadHit m => SHA -> ReaderT HitRepo m Bool
hitExistsObject sha = do
    g <- hitGit <$> getRepository
    let ref = DGF.fromBinary $ getSHA sha
    mobj <- liftIO $ DGS.getObjectRaw g ref True
    return $ isJust mobj

cliSourceObjects :: MonadHit m
                 => Maybe (CommitOid HitRepo) -> CommitOid HitRepo -> Bool
                 -> Producer (ReaderT HitRepo m) (ObjectOid HitRepo)
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

cliReadTree :: MonadHit m
            => Tree HitRepo -> ReaderT HitRepo m (Pure.EntryHashMap HitRepo)
cliReadTree (CmdLineTree (renderObjOid -> sha)) = do
    contents <- runGit ["ls-tree", "-z", fromStrict sha]
    -- Even though the tree entries are separated by \NUL, for whatever
    -- reason @git ls-tree@ also outputs a newline at the end.
    HashMap.fromList
        <$> mapM cliParseLsTree (L.init (TL.splitOn "\NUL" contents))

cliParseLsTree :: MonadHit m
               => TL.Text -> ReaderT HitRepo m (TreeFilePath, TreeEntry HitRepo)
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

cliWriteTree :: MonadHit m
             => Pure.EntryHashMap HitRepo -> ReaderT HitRepo m (TreeOid HitRepo)
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

hitLookupTree :: MonadHit m
              => TreeOid HitRepo -> ReaderT HitRepo m (Tree HitRepo)
hitLookupTree oid = do
    g <- hitGit <$> getRepository
    let hex = oidToHex oid
    if hex == emptyTreeId
      then return $ CmdLineTree oid
      else do
        let ref = oidToRef oid
        mtr <- liftIO $ DGR.getTreeMaybe g ref
        maybe (throwM $ ObjectLookupFailed hex 40)
              (\_ -> return $ CmdLineTree oid)
              mtr

blobKindFor :: DG.ModePerm -> BlobKind
blobKindFor (DG.ModePerm 0o120000) = SymlinkBlob
blobKindFor (DG.ModePerm bits) =
    if bits .&. 1 == 1 then ExecutableBlob else PlainBlob

convertTreeEnt :: DGT.TreeEnt -> TreeEntry HitRepo
convertTreeEnt (DG.ModePerm 0o40000, _, ref) = TreeEntry $ refToOid ref
convertTreeEnt (mode, _, ref) = BlobEntry (refToOid ref) (blobKindFor mode)

hasName :: B.ByteString -> DGT.TreeEnt -> Bool
hasName n (_,name,_) = name == n

trace :: String -> IO ()
trace _ = return ()
--trace = putStrLn

findInTree :: [TreeFilePath] -> DG.Tree -> DGR.Git -> IO (Maybe (TreeEntry HitRepo))
findInTree [] _ _ = return Nothing
findInTree (n:ns) (DG.Tree ents) g =
    case L.find (hasName n) ents of
        Nothing -> do
            trace $ " FIT " ++ BC.unpack n ++ " » ∅"
            return Nothing
        Just ent@(_, _, ref) -> do
            trace $ " FIT " ++ BC.unpack n ++ " » " ++ DGF.toHexString ref
            case ns of
                [] -> return $ Just $ convertTreeEnt ent
                _ -> findInTreeRef ns ref g

findInTreeRef :: [TreeFilePath] -> DG.Ref -> DGR.Git -> IO (Maybe (TreeEntry HitRepo))
findInTreeRef ns ref g = do
    mtr <- DGR.getTreeMaybe g ref
    case mtr of
        Nothing -> do
            trace $ " FTR " ++ DGF.toHexString ref ++ " » ∅"
            return Nothing
        Just tr -> do
            trace $ " FTR " ++ DGF.toHexString ref ++ " » " ++ show tr
            findInTree ns tr g

hitTreeEntry :: MonadHit m
             => Tree HitRepo -> TreeFilePath
             -> ReaderT HitRepo m (Maybe (TreeEntry HitRepo))
hitTreeEntry tree fp = do
    HitRepo _ g <- getRepository
    toid <- treeOid tree
    let ref = oidToRef toid
    liftIO $ do
        trace $ "LOOK " ++ DGF.toHexString ref ++ " " ++ BC.unpack fp
        findInTreeRef (BC.split '/' fp) ref g

cliSourceTreeEntries :: MonadHit m
                     => Tree HitRepo
                     -> Producer (ReaderT HitRepo m) (TreeFilePath, TreeEntry HitRepo)
cliSourceTreeEntries tree = do
    contents <- lift $ do
        toid <- treeOid tree
        runGit [ "ls-tree", "-t", "-r", "-z"
               , fromStrict (renderObjOid toid)
               ]
    forM_ (L.init (TL.splitOn "\NUL" contents)) $
        yield <=< lift . cliParseLsTree

cliLookupCommit :: MonadHit m
                => CommitOid HitRepo -> ReaderT HitRepo m (Commit HitRepo)
cliLookupCommit (renderObjOid -> sha) = do
    output <- doRunGit run ["cat-file", "--batch"] $
        setStdin (TL.append (fromStrict sha) "\n")
    result <- runParserT parseOutput () "" (TL.unpack output)
    case result of
        Left e  -> throwM $ CommitLookupFailed (T.pack (show e))
        Right c -> return c
  where
    parseOutput :: (Stream s  (ReaderT HitRepo m) Char, MonadHit m)
                => ParsecT s u (ReaderT HitRepo m) (Commit HitRepo)
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

cliCreateCommit :: MonadHit m
                => [CommitOid HitRepo]
                -> TreeOid HitRepo
                -> Signature
                -> Signature
                -> Text
                -> Maybe Text
                -> ReaderT HitRepo m (Commit HitRepo)
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

cliShowRef :: MonadHit m
           => Maybe Text -> ReaderT HitRepo m (Maybe [(TL.Text,TL.Text)])
cliShowRef mrefName = do
    repo <- getRepository
    shelly $ silently $ errExit False $ do
        rev <- git repo $ [ "show-ref" ]
                 <> [ fromStrict (fromJust mrefName) | isJust mrefName ]
        ec  <- lastExitCode
        return $ if ec == 0
                 then Just $ map ((\(x:y:[]) -> (y,x)) . TL.words)
                           $ TL.lines rev
                 else Nothing

cliLookupRef :: MonadHit m
             => Text -> ReaderT HitRepo m (Maybe (RefTarget HitRepo))
cliLookupRef refName = do
    repo <- getRepository
    (ec,rev) <- shelly $ silently $ errExit False $ do
        rev <- git repo [ "symbolic-ref", fromStrict refName ]
        ec  <- lastExitCode
        return (ec,rev)
    if ec == 0
        then return . Just . RefSymbolic . toStrict . TL.init $ rev
        else fmap RefObj <$> hitResolveRef refName

cliUpdateRef :: MonadHit m => Text -> RefTarget HitRepo -> ReaderT HitRepo m ()
cliUpdateRef refName (RefObj (renderOid -> sha)) =
    runGit_ ["update-ref", fromStrict refName, fromStrict sha]

cliUpdateRef refName (RefSymbolic targetName) =
    runGit_ ["symbolic-ref", fromStrict refName, fromStrict targetName]

cliDeleteRef :: MonadHit m => Text -> ReaderT HitRepo m ()
cliDeleteRef refName = runGit_ ["update-ref", "-d", fromStrict refName]

cliSourceRefs :: MonadHit m => Producer (ReaderT HitRepo m) Text
cliSourceRefs = do
    mxs <- lift $ cliShowRef Nothing
    yieldMany $ case mxs of
        Nothing -> []
        Just xs -> map (toStrict . fst) xs

hitResolveRef :: MonadHit m => Text -> ReaderT HitRepo m (Maybe (Oid HitRepo))
hitResolveRef refName@(TL.unpack -> name) = do
    path <- DGS.gitRepoPath <$> hitGit <$> getRepository
    let readThru spec = do
        mr :: Either X.IOException DGN.RefContentTy
           <- X.try $ DGN.readRefFile path spec
        trace $ "REF " ++ name ++ ": " ++ show spec ++ " » " ++ show mr
        case mr of
            Left _ -> return Nothing
            Right (DGN.RefDirect ref) -> return $ Just $ refToSHA ref
            Right (DGN.RefContentUnknown _) -> throwM $ ReferenceLookupFailed refName
            Right (DGN.RefLink spec') -> readThru spec'
    liftIO $ readThru $ toRefTy name


-- cliLookupTag :: MonadHit m
--              => TagOid HitRepo -> ReaderT HitRepo m (Tag HitRepo)
-- cliLookupTag oid = undefined

cliCreateTag :: MonadHit m
             => CommitOid HitRepo -> Signature -> Text -> Text
             -> ReaderT HitRepo m (Tag HitRepo)
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

cliWorkingTreeDirty :: MonadHit m => ReaderT HitRepo m Bool
cliWorkingTreeDirty = do
    status <- runGit [ "status", "-s", "-uno", "-z" ]
    return $ TL.length status > 0

hitFactory :: MonadHit m => RepositoryFactory (ReaderT HitRepo m) m HitRepo
hitFactory = RepositoryFactory
    { openRepository  = openHitRepository
    , runRepository   = flip runReaderT
    }

openHitRepository :: MonadIO m => RepositoryOptions -> m HitRepo
openHitRepository opts = liftIO $ do
    let path = F.decodeString $ repoPath opts
    exists <- DG.isRepo path
    when (not exists && repoAutoCreate opts) $ do
        DG.initRepo path -- creates dirs, but still need HEAD
        let head = F.encodeString $ (F.</>) path "HEAD"
        withFile head WriteMode $ flip hPutStrLn "ref: refs/heads/master"
        let pack = F.encodeString $ (F.</>) path $ (F.</>) "objects" "pack"
        createDirectory pack
    g <- DGS.openRepo path
    return $ HitRepo opts g

-- Hit.hs
