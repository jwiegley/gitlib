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
       , hitTime
       ) where

import           Conduit
import           Control.Applicative (Applicative, (<$>))
import qualified Control.Exception as X
import           Control.Monad (when)
import           Control.Monad.Trans.Reader (ReaderT, runReaderT, ask)
import           Data.Bits ((.&.))
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


{---- Type and instance declarations ----}

newtype HitRepo = HitRepo { hitGit :: DGS.Git }

type MonadHit m = (Applicative m, MonadThrow m, MonadIO m)

instance IsOid DG.Ref where
    renderOid = T.pack . DGF.toHexString

instance (Applicative m, MonadThrow m, MonadIO m)
         => MonadGit HitRepo (ReaderT HitRepo m) where
    type Oid HitRepo     = DG.Ref
    newtype Tree HitRepo = HitTree (TreeOid HitRepo)
    data Options HitRepo = Options

    facts            = return RepositoryFacts { hasSymbolicReferences = True }
    parseOid         = return . DGF.fromHexString . T.unpack
    getRepository    = ask
    closeRepository  = getRepository >>= liftIO . DGS.closeRepo . hitGit
    deleteRepository = getRepository >>= liftIO . rm
      where rm = Dir.removeDirectoryRecursive . encodeString .
                 DGS.gitRepoPath . hitGit

    sourceObjects         = error "undefined: sourceObjects"
    readIndex             = error "undefined: readIndex"
    writeIndex            = error "undefined: writeIndex"
    diffContentsWithTree  = error "undefined: diffContentsWithTree"

    createBlob            = hitCreateBlob
    createCommit          = hitCreateCommit
    createReference       = hitUpdateRef
    createTag             = hitCreateTag
    deleteReference       = hitDeleteRef
    existsObject          = hitExistsObject
    hashContents          = hitHashContents
    lookupBlob            = hitLookupBlob
    lookupCommit          = hitLookupCommit
    lookupObject          = hitLookupObject
    lookupReference       = hitLookupRef
    lookupTag             = hitLookupTag
    lookupTree            = hitLookupTree
    newTreeBuilder        = Pure.newPureTreeBuilder hitReadTree hitWriteTree
    sourceReferences      = hitSourceRefs
    sourceTreeEntries     = hitSourceTreeEntries
    treeEntry             = hitTreeEntry
    treeOid (HitTree oid) = return oid
    updateReference       = hitUpdateRef


{---- Conversions between different library types ----}

hitTime :: ZonedTime -> DGT.GitTime
hitTime zt = DGT.GitTime sec tz
  where sec = read $ formatTime defaultTimeLocale "%s" zt
        tz = read $ stripPlus $ timeZoneOffsetString $ zonedTimeZone zt

hitPerson :: Signature -> DG.Person
hitPerson s = DG.Person
  { DG.personName = encodeUtf8 $ signatureName s
  , DG.personEmail = encodeUtf8 $ signatureEmail s
  , DG.personTime = hitTime $ signatureWhen s
  }

gitPerson :: DG.Person -> Signature
gitPerson p = Signature
  { signatureName = decodeUtf8 $ DG.personName p
  , signatureEmail = decodeUtf8 $ DG.personEmail p
  , signatureWhen = DGT.toZonedTime $ DG.personTime p
  }

gitCommit :: CommitOid HitRepo -> DG.Commit -> Commit HitRepo
gitCommit oid c = Commit
  { commitOid       = oid
  , commitParents   = map Tagged $ DG.commitParents c
  , commitTree      = Tagged $ DG.commitTreeish c
  , commitAuthor    = gitPerson $ DG.commitAuthor c
  , commitCommitter = gitPerson $ DG.commitCommitter c
  , commitLog       = decodeUtf8 $ DG.commitMessage c
  , commitEncoding  = maybe "" decodeUtf8 $ DG.commitEncoding c
  }

hitModePerm :: BlobKind -> DG.ModePerm
hitModePerm PlainBlob      = DG.ModePerm 0o100644
hitModePerm ExecutableBlob = DG.ModePerm 0o100755
hitModePerm SymlinkBlob    = DG.ModePerm 0o120000

gitBlobKind :: DG.ModePerm -> BlobKind
gitBlobKind (DG.ModePerm 0o120000) = SymlinkBlob
gitBlobKind (DG.ModePerm bits)
    | bits .&. 1 == 1 = ExecutableBlob
    | otherwise = PlainBlob

hitRefContent :: RefTarget HitRepo -> DGN.RefContentTy
hitRefContent (RefObj ref) = DGN.RefDirect ref
hitRefContent (RefSymbolic name) = DGN.RefLink $ toRefTy $ T.unpack name

gitRefTarget :: Text -> DGN.RefContentTy -> IO (RefTarget HitRepo)
gitRefTarget name rc = case rc of
  DGN.RefDirect ref -> return $ RefObj ref
  DGN.RefLink spec -> return $ RefSymbolic $ T.pack $ fromRefTy spec
  DGN.RefContentUnknown _ -> throwM $ ReferenceLookupFailed name

gitBlob :: BlobOid HitRepo -> DG.Blob -> Blob HitRepo m
gitBlob oid = Blob oid . BlobStringLazy . DGT.blobGetContent

type TreePathEntry = (TreeFilePath, TreeEntry HitRepo)

hitTreeEnt :: TreePathEntry -> DGT.TreeEnt
hitTreeEnt (path, e) = case e of
    BlobEntry oid k -> (hitModePerm k, path, untag oid)
    TreeEntry oid   -> (DG.ModePerm 0o040000, path, untag oid)
    CommitEntry oid -> (DG.ModePerm 0o160000, path, untag oid)

gitTreeEnt :: DGT.TreeEnt -> TreeEntry HitRepo
gitTreeEnt (mode, _, ref) = case mode of
    DG.ModePerm 0o040000 -> TreeEntry $ Tagged ref
    DG.ModePerm 0o160000 -> CommitEntry $ Tagged ref
    _ -> BlobEntry (Tagged ref) $ gitBlobKind mode

gitTreePathEnt :: TreeFilePath -> DGT.TreeEnt -> TreePathEntry
gitTreePathEnt dir e@(_,p,_) = (BC.append dir p, gitTreeEnt e)

gitTreeNameEnt :: DGT.TreeEnt -> TreePathEntry
gitTreeNameEnt e@(_,name,_) = (name, gitTreeEnt e)

gitObject :: MonadHit m => Oid HitRepo -> DGO.Object
          -> Object HitRepo (ReaderT HitRepo m)
gitObject ref obj = case obj of
  DGO.ObjCommit c -> CommitObj $ gitCommit (Tagged ref) c
  DGO.ObjTag t    -> TagObj $ Tag (Tagged ref) $ Tagged (DGT.tagRef t)
  DGO.ObjBlob b   -> BlobObj $ Blob (Tagged ref) $ BlobStringLazy $
                     DGT.blobGetContent b
  DGO.ObjTree _   -> TreeObj $ HitTree $ Tagged ref
  -- TODO: handle ObjDeltaOfs, ObjDeltaRef?


{---- The git operations ----}

hitFactory :: MonadHit m => RepositoryFactory (ReaderT HitRepo m) m HitRepo
hitFactory = RepositoryFactory
    { openRepository  = hitOpenRepo
    , runRepository   = flip runReaderT
    }


hitOpenRepo :: MonadIO m => RepositoryOptions -> m HitRepo
hitOpenRepo opts = liftIO $ do
    let path = fromString $ repoPath opts
    exists <- DG.isRepo path
    when (not exists && repoAutoCreate opts) $ do
        DG.initRepo path -- creates subdirs only
        -- compensate for initRepo not creating HEAD file
        let head = encodeString $ path </> "HEAD"
        withFile head WriteMode $ flip hPutStrLn "ref: refs/heads/master"
        -- likewise the objects/pack subdir
        let pack = encodeString $ path </> "objects" </> "pack"
        Dir.createDirectory pack
    HitRepo <$> DGS.openRepo path


hitCreateBlob :: MonadHit m
              => BlobContents (ReaderT HitRepo m)
              -> ReaderT HitRepo m (BlobOid HitRepo)
hitCreateBlob b = do
    g <- hitGit <$> getRepository
    obj <- DGO.ObjBlob . DG.Blob <$> blobContentsToLazyByteString b
    fmap Tagged $ liftIO $ DG.setObject g obj


hitCreateCommit :: MonadHit m
                => [CommitOid HitRepo] -> TreeOid HitRepo
                -> Signature -> Signature -> Text -> Maybe Text
                -> ReaderT HitRepo m (Commit HitRepo)
hitCreateCommit parentOids treeOid author committer message ref = do
    g <- hitGit <$> getRepository
    let c = DG.Commit
          { DG.commitTreeish = untag treeOid
          , DG.commitParents = map untag parentOids
          , DG.commitAuthor = hitPerson author
          , DG.commitCommitter = hitPerson committer
          , DG.commitEncoding = Nothing
          , DG.commitExtras = []
          , DG.commitMessage = encodeUtf8 message
          }
    oid <- fmap Tagged $ liftIO $ DGS.setObject g $ DGO.ObjCommit c
    for_ ref $ flip hitUpdateRef (RefObj (untag oid))
    return $ Commit
        { commitOid       = oid
        , commitAuthor    = author
        , commitCommitter = committer
        , commitLog       = message
        , commitTree      = treeOid
        , commitParents   = parentOids
        , commitEncoding  = "utf-8"
        }


hitUpdateRef :: MonadHit m => Text -> RefTarget HitRepo
             -> ReaderT HitRepo m ()
hitUpdateRef (T.unpack -> name) target = withPath upd
  where
    upd path =
      DGN.writeRefFile path (toRefTy name) $ hitRefContent target


hitCreateTag :: MonadHit m
             => CommitOid HitRepo -> Signature -> Text -> Text
             -> ReaderT HitRepo m (Tag HitRepo)
hitCreateTag oid tagger msg name = do
    g <- hitGit <$> getRepository
    let tag = DG.Tag
          { DG.tagRef = untag oid
          , DG.tagObjectType = DGT.TypeCommit
          , DG.tagName = hitPerson tagger
          , DG.tagBlob = encodeUtf8 name
          , DG.tagS = encodeUtf8 $ T.snoc msg '\n'
          }
    ref <- liftIO $ DGS.setObject g $ DGO.ObjTag tag
    return $ Tag
        { tagOid = Tagged ref
        , tagCommit = oid
        }


hitDeleteRef :: MonadHit m => Text -> ReaderT HitRepo m ()
hitDeleteRef (T.unpack -> name) = withPath del
  where
    del path =
      Dir.removeFile $ encodeString $ toPath path $ toRefTy name


hitExistsObject :: MonadHit m => Oid HitRepo -> ReaderT HitRepo m Bool
hitExistsObject ref = do
    g <- hitGit <$> getRepository
    fmap isJust $ liftIO $ DGS.getObjectRaw g ref True


hitHashContents :: MonadHit m
                => BlobContents (ReaderT HitRepo m)
                -> ReaderT HitRepo m (BlobOid HitRepo)
hitHashContents b =
    Tagged . hashLazyBlob <$> blobContentsToLazyByteString b


hitLookupBlob :: MonadHit m
              => BlobOid HitRepo
              -> ReaderT HitRepo m (Blob HitRepo (ReaderT HitRepo m))
hitLookupBlob oid = do
    g <- hitGit <$> getRepository
    mobj <- liftIO $ DG.getObject g (untag oid) True
    maybe (throwM BlobLookupFailed)
          -- TODO: must we handle ObjDeltaOfs, ObjDeltaRef?
          (\(DGO.ObjBlob b) -> return $ gitBlob oid b)
          mobj


hitLookupCommit :: MonadHit m
                => CommitOid HitRepo -> ReaderT HitRepo m (Commit HitRepo)
hitLookupCommit oid = do
    g <- hitGit <$> getRepository
    fmap (gitCommit oid) $ liftIO $ DG.getCommit g $ untag oid


hitLookupObject :: MonadHit m
                => Oid HitRepo
                -> ReaderT HitRepo m (Object HitRepo (ReaderT HitRepo m))
hitLookupObject oid = do
    g <- hitGit <$> getRepository
    mobj <- liftIO $ DG.getObject g oid True
    maybe (throwM $ ObjectLookupFailed (renderOid oid) 0)
          (return . gitObject oid)
          mobj


hitLookupRef :: MonadHit m
             => Text -> ReaderT HitRepo m (Maybe (RefTarget HitRepo))
hitLookupRef refName = withPath look
  where
    look path = do
      mrc <- tryReadRef path $ toRefTy $ T.unpack refName
      case mrc of
        Nothing -> return Nothing
        Just rc -> Just <$> gitRefTarget refName rc


hitLookupTag :: MonadHit m
             => TagOid HitRepo -> ReaderT HitRepo m (Tag HitRepo)
hitLookupTag oid = do
    g <- hitGit <$> getRepository
    mobj <- liftIO $ DG.getObject g (untag oid) True
    maybe (throwM $ TagLookupFailed "")
          (\(DGO.ObjTag t) -> return $ Tag oid $ Tagged $ DGT.tagRef t)
          mobj


hitLookupTree :: MonadHit m
              => TreeOid HitRepo -> ReaderT HitRepo m (Tree HitRepo)
hitLookupTree oid@(Tagged ref) = do
    g <- hitGit <$> getRepository
    if isEmpty ref
        then return $ HitTree oid
        else do
            mtr <- liftIO $ DGR.getTreeMaybe g ref
            maybe (throwM $ ObjectLookupFailed (renderOid ref) 40)
                  (\_ -> return $ HitTree oid)
                  mtr


hitReadTree :: MonadHit m
            => Tree HitRepo -> ReaderT HitRepo m (Pure.EntryHashMap HitRepo)
hitReadTree (HitTree (Tagged ref)) = do
    g <- hitGit <$> getRepository
    DGT.Tree ents <- liftIO $ getTree g ref
    return $ HashMap.fromList $ map gitTreeNameEnt ents


hitWriteTree :: MonadHit m
             => Pure.EntryHashMap HitRepo -> ReaderT HitRepo m (TreeOid HitRepo)
hitWriteTree entMap = do
    g <- hitGit <$> getRepository
    let ord (_,n1,_) (_,n2,_) = compare n1 n2
    let ents = sortBy ord $ map hitTreeEnt $ HashMap.toList entMap
    fmap Tagged $ liftIO $ DGS.setObject g $ DGO.ObjTree $ DGT.Tree ents


hitSourceRefs :: MonadHit m => Producer (ReaderT HitRepo m) Text
hitSourceRefs = do
    g <- lift $ hitGit <$> getRepository
    -- TODO: this produces branches & tags, but not remotes
    -- (compare with `git show-ref`).
    refs <- liftIO $ do
        bb <- Set.map DGN.RefBranch <$> DG.branchList g
        tt <- Set.map DGN.RefTag <$> DG.tagList g
        return $ Set.union bb tt
    yieldMany $ Set.map (T.pack . fromRefTy) refs


hitSourceTreeEntries :: MonadHit m
                     => Tree HitRepo
                     -> Producer (ReaderT HitRepo m) TreePathEntry
hitSourceTreeEntries (HitTree oid) = do
    g <- lift $ hitGit <$> getRepository
    ents <- liftIO $ readTreeRecurse "" (untag oid) g
    yieldMany ents


hitTreeEntry :: MonadHit m
             => Tree HitRepo -> TreeFilePath
             -> ReaderT HitRepo m (Maybe (TreeEntry HitRepo))
hitTreeEntry (HitTree (Tagged ref)) fp = do
    g <- hitGit <$> getRepository
    liftIO $ do
      dbg D_TREE $ "LOOK " ++ DGF.toHexString ref ++ " " ++ BC.unpack fp
      searchTreeRef (BC.split '/' fp) ref g



{---- Tree operations ----}

isEmpty :: DG.Ref -> Bool
isEmpty ref = DGF.toHex ref == encodeUtf8 emptyTreeId

-- These compensate for the fact that Hit functions fail when looking
-- up the empty tree hash (4b825dc642cb6eb9a060e54bf8d69288fbee4904)
getTree :: DGR.Git -> DG.Ref -> IO DGT.Tree
getTree g ref =
    if isEmpty ref then return $ DGT.Tree []
    else DGR.getTree g ref

getTreeMaybe :: DGR.Git -> DG.Ref -> IO (Maybe DGT.Tree)
getTreeMaybe g ref =
    if isEmpty ref then return $ Just $ DGT.Tree []
    else DGR.getTreeMaybe g ref

-- Mutually recursive functions to descend through a tree by following
-- a hierarchical path.
searchTree :: [TreeFilePath] -> DG.Tree -> DGR.Git
           -> IO (Maybe (TreeEntry HitRepo))
searchTree [] _ _ = return Nothing
searchTree (n:ns) (DG.Tree ents) g =
    let hasName n (_,x,_) = n == x in
    case L.find (hasName n) ents of
      Nothing -> do
        dbg D_TREE $ " FIT " ++ BC.unpack n ++ " » ∅"
        return Nothing
      Just ent@(_, _, ref) -> do
        dbg D_TREE $ " FIT " ++ BC.unpack n ++ " » " ++ DGF.toHexString ref
        case ns of
          [] -> return $ Just $ gitTreeEnt ent
          _ -> searchTreeRef ns ref g

searchTreeRef :: [TreeFilePath] -> DG.Ref -> DGR.Git
              -> IO (Maybe (TreeEntry HitRepo))
searchTreeRef ns ref g = do
    mtr <- getTreeMaybe g ref
    case mtr of
      Nothing -> do
        dbg D_TREE $ " FTR " ++ DGF.toHexString ref ++ " » ∅"
        return Nothing
      Just tr -> do
        dbg D_TREE $ " FTR " ++ DGF.toHexString ref ++ " » " ++ show tr
        searchTree ns tr g


readTreeRecurse :: TreeFilePath -> DG.Ref -> DGR.Git -> IO [TreePathEntry]
readTreeRecurse dir ref g = do
    DGT.Tree ents <- getTree g ref
    fmap concat $ mapM f $ map (gitTreePathEnt dir) ents
  where
    f :: TreePathEntry -> IO [TreePathEntry]
    f e@(p, TreeEntry oid) = do
        es <- readTreeRecurse (BC.append p "/") (untag oid) g
        return (e:es)
    f e = return [e]


{---- Other Utilities ----}

hashLazyBlob :: BL.ByteString -> DG.Ref
hashLazyBlob bs =
    DGO.objectHash DGO.TypeBlob sz bs
    where sz = fromIntegral $ BL.length bs

-- readRefFile throws an IOException when a ref file is not found.
-- Convert that to a Maybe result.
tryReadRef :: FilePath -> DGN.RefSpecTy -> IO (Maybe DGN.RefContentTy)
tryReadRef path spec = (Just <$> DGN.readRefFile path spec)
    `X.catch` \(_ :: X.IOException) -> return Nothing

withPath :: MonadHit m => (FilePath -> IO a) -> ReaderT HitRepo m a
withPath act = DGS.gitRepoPath <$> hitGit <$> getRepository >>= liftIO . act

stripPlus :: String -> String
stripPlus ('+':xs) = xs
stripPlus xs = xs

data Debuggable = D_TREE

dbg :: Debuggable -> String -> IO ()
--dbg D_TREE s = putStrLn s
dbg _ _ = return ()

-- Hit.hs
