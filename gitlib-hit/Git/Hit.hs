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

module Git.Hit
       ( hitFactory
       , convertTime
       ) where

import           Prelude hiding (FilePath)
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
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text as TL
import           Data.Time
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString)
import           Git
import qualified Git.Tree.Builder.Pure as Pure
import           Shelly hiding (FilePath, trace, (</>))
import           System.Directory
import           System.Locale (defaultTimeLocale)
import           System.IO (withFile, hPutStrLn, IOMode(WriteMode))

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

--- Following two are duplicates of unexported functions in Data.Git.Named
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

fromRefTy :: DGN.RefSpecTy -> String
fromRefTy (DGN.RefBranch h)  = "refs/heads/" ++ DGN.refNameRaw h
fromRefTy (DGN.RefTag h)     = "refs/tags/" ++ DGN.refNameRaw h
fromRefTy (DGN.RefRemote h)  = "refs/remotes/" ++ DGN.refNameRaw h
fromRefTy (DGN.RefPatches h) = "refs/patches/" ++ h
fromRefTy DGN.RefStash       = "refs/stash"
fromRefTy DGN.RefHead        = "HEAD"
fromRefTy DGN.RefOrigHead    = "ORIG_HEAD"
fromRefTy DGN.RefFetchHead   = "FETCH_HEAD"
fromRefTy (DGN.RefOther h)   = h

toPath :: FilePath -> DGN.RefSpecTy -> FilePath
toPath gitRepo (DGN.RefBranch h)  = gitRepo </> "refs" </> "heads" </> DGV.fromString (DGN.refNameRaw h)
toPath gitRepo (DGN.RefTag h)     = gitRepo </> "refs" </> "tags" </> DGV.fromString (DGN.refNameRaw h)
toPath gitRepo (DGN.RefRemote h)  = gitRepo </> "refs" </> "remotes" </> DGV.fromString (DGN.refNameRaw h)
toPath gitRepo (DGN.RefPatches h) = gitRepo </> "refs" </> "patches" </> DGV.fromString h
toPath gitRepo DGN.RefStash       = gitRepo </> "refs" </> "stash"
toPath gitRepo DGN.RefHead        = gitRepo </> "HEAD"
toPath gitRepo DGN.RefOrigHead    = gitRepo </> "ORIG_HEAD"
toPath gitRepo DGN.RefFetchHead   = gitRepo </> "FETCH_HEAD"
toPath gitRepo (DGN.RefOther h)   = gitRepo </> DGV.fromString h

-- END duplicates

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

    lookupReference   = hitLookupRef
    createReference   = hitUpdateRef
    updateReference   = hitUpdateRef
    deleteReference   = hitDeleteRef
    sourceReferences  = hitSourceRefs
    lookupCommit      = hitLookupCommit
    lookupTree        = hitLookupTree
    lookupBlob        = hitLookupBlob
    lookupTag         = undefined
    lookupObject      = undefined
    existsObject      = hitExistsObject
    sourceObjects     = undefined
    newTreeBuilder    = Pure.newPureTreeBuilder cliReadTree cliWriteTree
    treeOid (CmdLineTree toid) = return toid
    treeEntry         = hitTreeEntry
    sourceTreeEntries = cliSourceTreeEntries
    hashContents      = hitHashContents
    createBlob        = hitCreateBlob
    createCommit      = hitCreateCommit
    createTag         = hitCreateTag
    readIndex         = undefined
    writeIndex        = undefined
    diffContentsWithTree = undefined

type MonadHit m = (Applicative m, MonadThrow m, MonadIO m)

mkOid :: MonadHit m => forall o. TL.Text -> ReaderT HitRepo m (Tagged o SHA)
mkOid = fmap Tagged <$> textToSha . toStrict

gitStdOpts :: HitRepo -> [TL.Text]
gitStdOpts repo = [ "--git-dir", hitRepoPath repo ]
    ++ maybe [] (\w -> [ "--work-tree", w ]) (hitWorkingDir repo)

doRunGit :: MonadHit m
         => (FilePath -> [TL.Text] -> Sh a) -> [TL.Text] -> Sh ()
         -> ReaderT HitRepo m a
doRunGit f args act = do
    repo <- getRepository
    shelly $ silently $
        act >> f "git" (gitStdOpts repo <> args)

runGit :: MonadHit m => [TL.Text] -> ReaderT HitRepo m TL.Text
runGit = flip (doRunGit run) (return ())

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

convertPerson :: DG.Person -> Signature
convertPerson p = Signature
  { signatureName = T.decodeUtf8 $ DG.personName p
  , signatureEmail = T.decodeUtf8 $ DG.personEmail p
  , signatureWhen = DGT.toZonedTime $ DG.personTime p
  }

stripPlus :: String -> String
stripPlus ('+':xs) = xs
stripPlus xs = xs

convertTime :: ZonedTime -> DGT.GitTime
convertTime zt = DGT.GitTime sec tz
  where sec = read $ formatTime defaultTimeLocale "%s" zt
        tz = read $ stripPlus $ timeZoneOffsetString $ zonedTimeZone zt

sigToPerson :: Signature -> DG.Person
sigToPerson s = DG.Person
  { DG.personName = T.encodeUtf8 $ signatureName s
  , DG.personEmail = T.encodeUtf8 $ signatureEmail s
  , DG.personTime = convertTime $ signatureWhen s
  }

convertCommit :: CommitOid HitRepo -> DG.Commit -> Commit HitRepo
convertCommit oid c = Commit
  { commitOid       = oid
  , commitParents   = map refToOid $ DG.commitParents c
  , commitTree      = refToOid $ DG.commitTreeish c
  , commitAuthor    = convertPerson $ DG.commitAuthor c
  , commitCommitter = convertPerson $ DG.commitCommitter c
  , commitLog       = T.decodeUtf8 $ DG.commitMessage c
  , commitEncoding  = maybe "" T.decodeUtf8 $ DG.commitEncoding c
  }

hitLookupCommit :: MonadHit m
                => CommitOid HitRepo -> ReaderT HitRepo m (Commit HitRepo)
hitLookupCommit oid@(oidToRef -> ref) = do
    g <- hitGit <$> getRepository
    c <- liftIO $ DG.getCommit g ref
    return $ convertCommit oid c

hitCreateCommit :: MonadHit m
                => [CommitOid HitRepo]
                -> TreeOid HitRepo
                -> Signature
                -> Signature
                -> Text
                -> Maybe Text
                -> ReaderT HitRepo m (Commit HitRepo)
hitCreateCommit parentOids treeOid author committer message ref = do
    g <- hitGit <$> getRepository
    let c = DG.Commit
          { DG.commitTreeish = oidToRef treeOid
          , DG.commitParents = map oidToRef parentOids
          , DG.commitAuthor = sigToPerson author
          , DG.commitCommitter = sigToPerson committer
          , DG.commitEncoding = Nothing
          , DG.commitExtras = []
          , DG.commitMessage = T.encodeUtf8 message
          }
    h <- liftIO $ DGS.setObject g $ DGO.ObjCommit c
    let oid = refToOid h
    let k = Commit
          { commitOid       = oid
          , commitAuthor    = author
          , commitCommitter = committer
          , commitLog       = message
          , commitTree      = treeOid
          , commitParents   = parentOids
          , commitEncoding  = "utf-8"
          }
    for_ ref $ flip hitUpdateRef (RefObj (untag oid))
    return k

tryReadRef :: FilePath -> DGN.RefSpecTy -> IO (Maybe DGN.RefContentTy)
tryReadRef path spec = (Just <$> DGN.readRefFile path spec)
    `X.catch` \(_ :: X.IOException) -> return Nothing

refContentToTarget :: Text -> DGN.RefContentTy -> IO (RefTarget HitRepo)
refContentToTarget name rc = case rc of
  DGN.RefDirect ref -> return $ RefObj $ refToSHA ref
  DGN.RefLink spec -> return $ RefSymbolic $ TL.pack $ fromRefTy spec
  DGN.RefContentUnknown _ -> throwM $ ReferenceLookupFailed name

hitLookupRef :: MonadHit m
             => Text -> ReaderT HitRepo m (Maybe (RefTarget HitRepo))
hitLookupRef refName@(TL.unpack -> name) = do
    path <- DGS.gitRepoPath <$> hitGit <$> getRepository
    liftIO $ do
        mrc <- tryReadRef path $ toRefTy name
        case mrc of
            Nothing -> return Nothing
            Just rc -> Just <$> refContentToTarget refName rc

targetContent :: RefTarget HitRepo -> DGN.RefContentTy
targetContent (RefObj (getSHA -> sha)) = DGN.RefDirect $ DGF.fromBinary sha
targetContent (RefSymbolic name) = DGN.RefLink $ toRefTy $ TL.unpack name

pathAction :: MonadHit m => (FilePath -> IO a) -> ReaderT HitRepo m a
pathAction act = DGS.gitRepoPath <$> hitGit <$> getRepository >>= liftIO . act

hitUpdateRef :: MonadHit m => Text -> RefTarget HitRepo -> ReaderT HitRepo m ()
hitUpdateRef (TL.unpack -> name) target = pathAction upd
  where upd path =
          DGN.writeRefFile path (toRefTy name) $ targetContent target

hitDeleteRef :: MonadHit m => Text -> ReaderT HitRepo m ()
hitDeleteRef (TL.unpack -> name) = pathAction del
  where del path =
          removeFile $ encodeString $ toPath path $ toRefTy name

hitSourceRefs :: MonadHit m => Producer (ReaderT HitRepo m) Text
hitSourceRefs = do
    g <- lift $ hitGit <$> getRepository
    -- TODO: produces branches & tags, but not remotes (compare `git show-ref`)
    refs <- liftIO $ do
        bb <- Set.map DGN.RefBranch <$> DG.branchList g
        tt <- Set.map DGN.RefTag <$> DG.tagList g
        return $ Set.union bb tt
    yieldMany $ Set.map (TL.pack . fromRefTy) refs

hitCreateTag :: MonadHit m
             => CommitOid HitRepo -> Signature -> Text -> Text
             -> ReaderT HitRepo m (Tag HitRepo)
hitCreateTag oid tagger msg name = do
    g <- hitGit <$> getRepository
    let tag = DG.Tag 
          { DG.tagRef = oidToRef oid
          , DG.tagObjectType = DGT.TypeCommit
          , DG.tagName = sigToPerson tagger
          , DG.tagBlob = T.encodeUtf8 name
          , DG.tagS = T.encodeUtf8 $ T.snoc msg '\n'
          }
    ref <- liftIO $ DGS.setObject g $ DGO.ObjTag tag
    return $ Tag
        { tagOid = refToOid ref
        , tagCommit = oid
        }

hitFactory :: MonadHit m => RepositoryFactory (ReaderT HitRepo m) m HitRepo
hitFactory = RepositoryFactory
    { openRepository  = openHitRepository
    , runRepository   = flip runReaderT
    }

openHitRepository :: MonadIO m => RepositoryOptions -> m HitRepo
openHitRepository opts = liftIO $ do
    let path = fromString $ repoPath opts
    exists <- DG.isRepo path
    when (not exists && repoAutoCreate opts) $ do
        DG.initRepo path -- creates dirs, but still need HEAD
        let head = encodeString $ path </> "HEAD"
        withFile head WriteMode $ flip hPutStrLn "ref: refs/heads/master"
        let pack = encodeString $ path </> "objects" </> "pack"
        createDirectory pack
    g <- DGS.openRepo path
    return $ HitRepo opts g

-- Hit.hs
