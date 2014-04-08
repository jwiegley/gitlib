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

module Git.Hit
       ( hitFactory
       , convertTime
       ) where

import           Conduit
import           Control.Applicative (Applicative, (<$>))
import qualified Control.Exception as X
import           Control.Monad (when)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.Bits ((.&.))
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import           Data.Foldable (for_)
import qualified Data.Git as DG
import qualified Data.Git.Named as DGN
import qualified Data.Git.Ref as DGF
import qualified Data.Git.Repository as DGR
import qualified Data.Git.Storage as DGS
import qualified Data.Git.Storage.Object as DGO
import qualified Data.Git.Types as DGT
import qualified Data.HashMap.Strict as HashMap
import           Data.List as L
import           Data.Maybe (isJust)
import qualified Data.Set as Set
import           Data.String (fromString)
import           Data.Tagged (Tagged(..), untag)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Data.Time
import           Filesystem.Path.CurrentOS (FilePath, (</>), encodeString)
import           Git
import           Git.Hit.Internal
import qualified Git.Tree.Builder.Pure as Pure
import           Prelude hiding (FilePath)
import qualified System.Directory as Dir
import           System.IO (withFile, hPutStrLn, IOMode(WriteMode))
import           System.Locale (defaultTimeLocale)


data HitRepo = HitRepo
    { hitOptions :: RepositoryOptions
    , hitGit     :: DGS.Git
    }

hitRepoPath :: HitRepo -> Text
hitRepoPath = T.pack . repoPath . hitOptions

-- END duplicates

instance IsOid DG.Ref where
    renderOid = T.pack . DGF.toHexString

instance (Applicative m, MonadThrow m, MonadIO m)
         => MonadGit HitRepo (ReaderT HitRepo m) where
    type Oid HitRepo     = DG.Ref
    newtype Tree HitRepo = HitTree (TreeOid HitRepo)
    data Options HitRepo = Options

    facts = return RepositoryFacts
        { hasSymbolicReferences = True }

    getRepository    = ask
    closeRepository  = getRepository >>= liftIO . DGS.closeRepo . hitGit
    deleteRepository = getRepository >>=
        liftIO . Dir.removeDirectoryRecursive . T.unpack . hitRepoPath

    parseOid = return . DGF.fromHexString . T.unpack

    lookupReference   = hitLookupRef
    createReference   = hitUpdateRef
    updateReference   = hitUpdateRef
    deleteReference   = hitDeleteRef
    sourceReferences  = hitSourceRefs
    lookupCommit      = hitLookupCommit
    lookupTree        = hitLookupTree
    lookupBlob        = hitLookupBlob
    lookupTag         = error "undefined: lookupTag"
    lookupObject      = error "undefined: lookupObject"
    existsObject      = hitExistsObject
    sourceObjects     = error "undefined: sourceObjects"
    newTreeBuilder    = Pure.newPureTreeBuilder hitReadTree hitWriteTree
    treeOid (HitTree toid) = return toid
    treeEntry         = hitTreeEntry
    sourceTreeEntries = hitSourceTreeEntries
    hashContents      = hitHashContents
    createBlob        = hitCreateBlob
    createCommit      = hitCreateCommit
    createTag         = hitCreateTag
    readIndex         = error "undefined: readIndex"
    writeIndex        = error "undefined: writeIndex"
    diffContentsWithTree = error "undefined: diffContentsWithTree"

type MonadHit m = (Applicative m, MonadThrow m, MonadIO m)

hitLookupBlob :: MonadHit m
              => BlobOid HitRepo
              -> ReaderT HitRepo m (Blob HitRepo (ReaderT HitRepo m))
hitLookupBlob oid = do
    g <- hitGit <$> getRepository
    mobj <- liftIO $ DG.getObject g (untag oid) True
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
    return $ Tagged ref

hitCreateBlob :: MonadHit m
              => BlobContents (ReaderT HitRepo m)
              -> ReaderT HitRepo m (BlobOid HitRepo)
hitCreateBlob b = do
    g <- hitGit <$> getRepository
    bs <- blobContentsToLazyByteString b
    let obj = DGO.ObjBlob (DG.Blob bs)
    ref <- liftIO $ DG.setObject g obj
    return $ Tagged ref

hitExistsObject :: MonadHit m => Oid HitRepo -> ReaderT HitRepo m Bool
hitExistsObject ref = do
    g <- hitGit <$> getRepository
    mobj <- liftIO $ DGS.getObjectRaw g ref True
    return $ isJust mobj

getTreeHandleEmpty :: TreeOid HitRepo -> DGR.Git -> IO DGT.Tree
getTreeHandleEmpty (Tagged oid) g = do
    if emptyTreeId == (T.pack (DGF.toHexString oid))
      then return $ DGT.Tree []
      else DGR.getTree g oid

hitReadTree :: MonadHit m
            => Tree HitRepo -> ReaderT HitRepo m (Pure.EntryHashMap HitRepo)
hitReadTree (HitTree oid) = do
    g <- hitGit <$> getRepository
    DGT.Tree ents <- liftIO $ getTreeHandleEmpty oid g
    let f e@(_, name, _) = (name, convertTreeEnt e)
    return $ HashMap.fromList $ map f ents

type TreePathEntry = (TreeFilePath, TreeEntry HitRepo)

treeOrd :: DGT.TreeEnt -> DGT.TreeEnt -> Ordering
treeOrd (_, name1, _) (_, name2, _) = compare name1 name2

hitWriteTree :: MonadHit m
             => Pure.EntryHashMap HitRepo -> ReaderT HitRepo m (TreeOid HitRepo)
hitWriteTree entMap = do
    g <- hitGit <$> getRepository
    let ents = sortBy treeOrd $ map unconvertTreeEnt $ HashMap.toList entMap
    ref <- liftIO $ DGS.setObject g $ DGO.ObjTree $ DGT.Tree ents
    return $ Tagged ref

hitLookupTree :: MonadHit m
              => TreeOid HitRepo -> ReaderT HitRepo m (Tree HitRepo)
hitLookupTree oid = do
    g <- hitGit <$> getRepository
    let hex = T.pack $ DGF.toHexString $ untag oid
    if hex == emptyTreeId
      then return $ HitTree oid
      else do
        let ref = untag oid
        mtr <- liftIO $ DGR.getTreeMaybe g ref
        maybe (throwM $ ObjectLookupFailed hex 40)
              (\_ -> return $ HitTree oid)
              mtr

blobKindFor :: DG.ModePerm -> BlobKind
blobKindFor (DG.ModePerm 0o120000) = SymlinkBlob
blobKindFor (DG.ModePerm bits) =
    if bits .&. 1 == 1 then ExecutableBlob else PlainBlob

convertTreeEnt :: DGT.TreeEnt -> TreeEntry HitRepo
convertTreeEnt (DG.ModePerm 0o40000, _, ref) = TreeEntry $ Tagged ref
convertTreeEnt (mode, _, ref) = BlobEntry (Tagged ref) (blobKindFor mode)
-- TODO could be commit entry?

convertTreeEntWithPath :: TreeFilePath -> DGT.TreeEnt -> (TreeFilePath, TreeEntry HitRepo)
convertTreeEntWithPath dir e@(_, p, _) = (BC.append dir p, convertTreeEnt e)

unconvertTreeEnt :: (TreeFilePath, TreeEntry HitRepo) -> DGT.TreeEnt
unconvertTreeEnt (path, e) = case e of
    BlobEntry oid PlainBlob      -> (DG.ModePerm 0o100644, path, untag oid)
    BlobEntry oid ExecutableBlob -> (DG.ModePerm 0o100755, path, untag oid)
    BlobEntry oid SymlinkBlob    -> (DG.ModePerm 0o120000, path, untag oid)
    TreeEntry oid                -> (DG.ModePerm 0o040000, path, untag oid)
    CommitEntry oid              -> (DG.ModePerm 0o160000, path, untag oid)

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
    g <- hitGit <$> getRepository
    toid <- treeOid tree
    let ref = untag toid
    liftIO $ do
        trace $ "LOOK " ++ DGF.toHexString ref ++ " " ++ BC.unpack fp
        findInTreeRef (BC.split '/' fp) ref g

hitReadTreeRec :: TreeFilePath
               -> TreeOid HitRepo
               -> DGR.Git
               -> IO [TreePathEntry]
hitReadTreeRec dir oid g = do
    DGT.Tree ents <- getTreeHandleEmpty oid g
    fmap concat $ mapM f $ map (convertTreeEntWithPath dir) ents
  where
    f :: TreePathEntry -> IO [TreePathEntry]
    f e@(p, TreeEntry oid) = do
        es <- hitReadTreeRec (BC.append p "/") oid g
        return (e:es)
    f e = return [e]

hitSourceTreeEntries :: MonadHit m
                     => Tree HitRepo
                     -> Producer (ReaderT HitRepo m) TreePathEntry
hitSourceTreeEntries (HitTree toid) = do
    g <- lift $ hitGit <$> getRepository
    ents <- liftIO $ hitReadTreeRec "" toid g
    yieldMany ents

convertPerson :: DG.Person -> Signature
convertPerson p = Signature
  { signatureName = decodeUtf8 $ DG.personName p
  , signatureEmail = decodeUtf8 $ DG.personEmail p
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
  { DG.personName = encodeUtf8 $ signatureName s
  , DG.personEmail = encodeUtf8 $ signatureEmail s
  , DG.personTime = convertTime $ signatureWhen s
  }

convertCommit :: CommitOid HitRepo -> DG.Commit -> Commit HitRepo
convertCommit oid c = Commit
  { commitOid       = oid
  , commitParents   = map Tagged $ DG.commitParents c
  , commitTree      = Tagged $ DG.commitTreeish c
  , commitAuthor    = convertPerson $ DG.commitAuthor c
  , commitCommitter = convertPerson $ DG.commitCommitter c
  , commitLog       = decodeUtf8 $ DG.commitMessage c
  , commitEncoding  = maybe "" decodeUtf8 $ DG.commitEncoding c
  }

hitLookupCommit :: MonadHit m
                => CommitOid HitRepo -> ReaderT HitRepo m (Commit HitRepo)
hitLookupCommit oid@(untag -> ref) = do
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
          { DG.commitTreeish = untag treeOid
          , DG.commitParents = map untag parentOids
          , DG.commitAuthor = sigToPerson author
          , DG.commitCommitter = sigToPerson committer
          , DG.commitEncoding = Nothing
          , DG.commitExtras = []
          , DG.commitMessage = encodeUtf8 message
          }
    h <- liftIO $ DGS.setObject g $ DGO.ObjCommit c
    let oid = Tagged h
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
  DGN.RefDirect ref -> return $ RefObj ref
  DGN.RefLink spec -> return $ RefSymbolic $ T.pack $ fromRefTy spec
  DGN.RefContentUnknown _ -> throwM $ ReferenceLookupFailed name

hitLookupRef :: MonadHit m
             => Text -> ReaderT HitRepo m (Maybe (RefTarget HitRepo))
hitLookupRef refName@(T.unpack -> name) = do
    path <- DGS.gitRepoPath <$> hitGit <$> getRepository
    liftIO $ do
        mrc <- tryReadRef path $ toRefTy name
        case mrc of
            Nothing -> return Nothing
            Just rc -> Just <$> refContentToTarget refName rc

targetContent :: RefTarget HitRepo -> DGN.RefContentTy
targetContent (RefObj ref) = DGN.RefDirect ref
targetContent (RefSymbolic name) = DGN.RefLink $ toRefTy $ T.unpack name

pathAction :: MonadHit m => (FilePath -> IO a) -> ReaderT HitRepo m a
pathAction act = DGS.gitRepoPath <$> hitGit <$> getRepository >>= liftIO . act

hitUpdateRef :: MonadHit m => Text -> RefTarget HitRepo -> ReaderT HitRepo m ()
hitUpdateRef (T.unpack -> name) target = pathAction upd
  where upd path =
          DGN.writeRefFile path (toRefTy name) $ targetContent target

hitDeleteRef :: MonadHit m => Text -> ReaderT HitRepo m ()
hitDeleteRef (T.unpack -> name) = pathAction del
  where del path =
          Dir.removeFile $ encodeString $ toPath path $ toRefTy name

hitSourceRefs :: MonadHit m => Producer (ReaderT HitRepo m) Text
hitSourceRefs = do
    g <- lift $ hitGit <$> getRepository
    -- TODO: produces branches & tags, but not remotes (compare `git show-ref`)
    refs <- liftIO $ do
        bb <- Set.map DGN.RefBranch <$> DG.branchList g
        tt <- Set.map DGN.RefTag <$> DG.tagList g
        return $ Set.union bb tt
    yieldMany $ Set.map (T.pack . fromRefTy) refs

hitCreateTag :: MonadHit m
             => CommitOid HitRepo -> Signature -> Text -> Text
             -> ReaderT HitRepo m (Tag HitRepo)
hitCreateTag oid tagger msg name = do
    g <- hitGit <$> getRepository
    let tag = DG.Tag
          { DG.tagRef = untag oid
          , DG.tagObjectType = DGT.TypeCommit
          , DG.tagName = sigToPerson tagger
          , DG.tagBlob = encodeUtf8 name
          , DG.tagS = encodeUtf8 $ T.snoc msg '\n'
          }
    ref <- liftIO $ DGS.setObject g $ DGO.ObjTag tag
    return $ Tag
        { tagOid = Tagged ref
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
        Dir.createDirectory pack
    g <- DGS.openRepo path
    return $ HitRepo opts g

-- Hit.hs
