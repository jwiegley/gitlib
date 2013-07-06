{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Interface for working with Git repositories.
module Git
       ( RepositoryFacts(..)
       , Repository(..)
       , RepositoryFactory(..)
       , RepositoryOptions(..)
       , withBackendDo
       , withRepository
       , withRepository'
       , MonadGit

       , IsOid(..)
       , copyOid
       , Object(..)
       , ObjRef(..)
       , objectOid
       , SHA(..)
       , textToSha
       , shaToText

       , Blob(..)
       , BlobOid
       , BlobContents(..)
       , BlobKind(..)
       , ByteSource
       , BlobRef
       , blobEntry
       , blobRefOid
       , resolveBlobRef
       , createBlobUtf8
       , catBlob
       , catBlobUtf8
       , copyBlob
       , blobContentsToByteString
       , blobToByteString

       , TreeT
       , TreeBuilder(..)
       , TreeEntry(..)
       , TreeOid
       , TreeRef
       , createTree
       , withNewTree
       , mutateTree
       , mutateTreeRef
       , currentTree
       , currentTreeRef
       , withTree
       , withTreeRef
       , dropEntry
       , getEntry
       , putBlob
       , putBlob'
       , putCommit
       , putEntry
       , putTree
       , resolveTreeRef
       , treeEntry
       , getTreeEntryOid
       , treeRef
       , treeRefOid
       , ModifyTreeResult(..)
       , fromModifyTreeResult
       , toModifyTreeResult
       , emptyTreeId
       , treeBlobEntries
       , copyTreeEntry
       , copyTree

       , Commit(..)
       , CommitOid
       , CommitName(..)
       , CommitRef
       , Signature(..)
       , commitEntry
       , commitNameToRef
       , commitRef
       , commitRefOid
       , commitRefTarget
       , getCommitParents
       , copyCommitName
       , copyCommitOid
       , nameOfCommit
       , renderCommitName
       , resolveCommitRef
       , commitTreeEntry
       , copyCommit
       , genericPushCommit

       , PinnedEntry(..)
       , commitHistoryFirstParent
       , commitEntryHistory
       , identifyEntry

       , Tag(..)
       , TagOid
       , TagRef
       , tagRefOid

       , RefTarget(..)
       , Reference(..)
       , referenceToRef
       , resolveReferenceTree

       , GitException(..)
       , ModificationKind(..)
       , MergeStatus(..)
       , MergeResult(..)
       , mergeStatus
       , copyConflict
       , allMissingObjects
       , withNewRepository
       , withNewRepository'
       ) where

import           Control.Applicative
import qualified Control.Exception.Lifted as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.Binary as Bin
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Conduit
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Function
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Hashable
import           Data.List
import           Data.Map (Map)
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Traversable hiding (mapM, forM, sequence)
import           Data.Typeable
import           Filesystem (removeTree, isDirectory)
import           Filesystem.Path.CurrentOS hiding (null, concat)
import           Prelude hiding (FilePath)
import           System.Mem (performGC)

{- $repositories -}
data RepositoryFacts = RepositoryFacts
    { hasSymbolicReferences :: !Bool
    } deriving Show

type MonadGit m = (Failure Git.GitException m, Applicative m,
                   MonadIO m, MonadBaseControl IO m)

class (Eq o, Ord o, Show o) => IsOid o where
    renderOid :: o -> Text
    renderOid = renderObjOid . Tagged
    renderObjOid :: Tagged a o -> Text
    renderObjOid = renderOid . untag

-- | 'Repository' is the central point of contact between user code and
-- Git data objects.  Every object must belong to some repository.
class (Applicative m, Monad m, Failure GitException m, IsOid (Oid m))
      => Repository m where
    type Oid m     :: *
    data Tree m    :: *
    data Options m :: *

    facts :: m RepositoryFacts

    parseOid :: Text -> m (Oid m)
    parseObjOid :: forall o. Text -> m (Tagged o (Oid m))
    parseObjOid sha = Tagged <$> parseOid sha

    -- References
    createReference  :: Text -> RefTarget m (Commit m)
                     -> m (Reference m (Commit m))
    createReference_ :: Text -> RefTarget m (Commit m) -> m ()
    createReference_ = (void .) . createReference
    lookupReference  :: Text -> m (Maybe (Reference m (Commit m)))
    updateReference  :: Text -> RefTarget m (Commit m)
                     -> m (Reference m (Commit m))
    updateReference_ :: Text -> RefTarget m (Commit m) -> m ()
    updateReference_ = (void .) . updateReference
    deleteReference  :: Text -> m ()

    allReferences :: m [Reference m (Commit m)]
    allReferences = catMaybes <$> (mapM lookupReference =<< allReferenceNames)

    allReferenceNames :: m [Text]
    allReferenceNames = map referenceName <$> allReferences

    resolveReference :: Text -> m (Maybe (CommitRef m))
    resolveReference name = lookupReference name >>= referenceToRef (Just name)

    -- Lookup
    lookupCommit :: CommitOid m -> m (Commit m)
    lookupTree   :: TreeOid m -> m (Tree m)
    lookupBlob   :: BlobOid m -> m (Blob m)
    lookupTag    :: TagOid m -> m (Tag m)

    lookupObject :: Text -> m (Object m)
    existsObject :: Oid m -> m Bool

    traverseObjects :: forall a.
                       (Object m -> m a) -> Maybe (CommitName m) -> m [a]
    traverseObjects_ :: (Object m -> m ()) -> Maybe (CommitName m) -> m ()
    traverseObjects_ = (void .) . traverseObjects

    pushCommit :: (MonadTrans t, MonadGit m, MonadGit (t m),
                   Repository m, Repository (t m))
               => CommitName m -> Maybe Text -> Text
               -> t m (CommitRef (t m))

    traverseCommits :: forall a.
                       (CommitRef m -> m a) -> CommitName m -> m [a]
    traverseCommits_ :: (CommitRef m -> m ()) -> CommitName m -> m ()
    traverseCommits_ = (void .) . traverseCommits

    missingObjects :: Maybe (CommitName m) -- ^ A commit we may already have
                   -> CommitName m         -- ^ The commit we need
                   -> m [Object m]         -- ^ All the objects in between

    -- Object creation
    newTreeBuilder :: Maybe (Tree m) -> m (TreeBuilder m)

    treeOid :: Tree m -> TreeOid m
    getTreeEntry :: Tree m -> FilePath -> m (Maybe (TreeEntry m))
    traverseEntries :: (FilePath -> TreeEntry m -> m a) -> Tree m -> m [a]
    traverseEntries_ :: (FilePath -> TreeEntry m -> m a) -> Tree m -> m ()
    traverseEntries_ = (void .) . traverseEntries

    hashContents :: BlobContents m -> m (BlobOid m)
    createBlob   :: BlobContents m -> m (BlobOid m)
    createCommit :: [CommitRef m] -> TreeRef m
                 -> Signature -> Signature -> Text -> Maybe Text -> m (Commit m)
    createTag :: CommitOid m -> Signature -> Text -> Text -> m (Tag m)

    deleteRepository :: m ()

    -- Pack files
    buildPackFile :: FilePath -> [Either (CommitOid m) (TreeOid m)]
                  -> m FilePath
    buildPackFile _ _ =
        failure (BackendError "Backend does not support building pack files")

    buildPackIndex :: FilePath -> ByteString -> m (Text, FilePath, FilePath)
    buildPackIndex _ _ =
        failure (BackendError "Backend does not support building pack indexes")

    writePackFile :: FilePath -> m ()
    writePackFile _ =
        failure (BackendError "Backend does not support writing  pack files")

    -- Git remotes
    remoteFetch :: Text {- URI -} -> Text {- fetch spec -} -> m ()

{- $exceptions -}
-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data GitException = BackendError Text
                  | GitError Text
                  | RepositoryNotExist
                  | RepositoryInvalid
                  | RepositoryCannotAccess Text
                  | BlobCreateFailed
                  | BlobEmptyCreateFailed
                  | BlobEncodingUnknown Text
                  | BlobLookupFailed
                  | PushNotFastForward Text
                  | TranslationException Text
                  | TreeCreateFailed Text
                  | TreeBuilderCreateFailed
                  | TreeBuilderInsertFailed Text
                  | TreeBuilderRemoveFailed Text
                  | TreeBuilderWriteFailed Text
                  | TreeLookupFailed
                  | TreeCannotTraverseBlob
                  | TreeCannotTraverseCommit
                  | TreeEntryLookupFailed FilePath
                  | TreeUpdateFailed
                  | TreeWalkFailed
                  | CommitCreateFailed
                  | CommitLookupFailed Text
                  | ReferenceCreateFailed
                  | ReferenceDeleteFailed Text
                  | RefCannotCreateFromPartialOid
                  | ReferenceListingFailed
                  | ReferenceLookupFailed Text
                  | ObjectLookupFailed Text Int
                  | ObjectRefRequiresFullOid
                  | OidCopyFailed
                  | OidParseFailed Text
                  | QuotaHardLimitExceeded Int Int
                  deriving (Eq, Show, Typeable)

-- jww (2013-02-11): Create a BackendException data constructor of forall
-- e. Exception e => BackendException e, so that each can throw a derived
-- exception.
instance Exc.Exception GitException

{- $oids -}
type BlobOid m   = Tagged (Blob m) (Oid m)
type TreeOid m   = Tagged (Tree m) (Oid m)
type CommitOid m = Tagged (Commit m) (Oid m)
type TagOid m    = Tagged (Tag m) (Oid m)

{- $references -}
data RefTarget m a = RefObj !(ObjRef m a) | RefSymbolic !Text

data Reference m a = Reference
    { referenceName   :: !Text
    , referenceTarget :: !(RefTarget m a) }

data CommitName m = CommitObjectId !(CommitOid m)
                  | CommitRefName !Text
                  | CommitReference !(Reference m (Commit m))

instance Repository m => Show (CommitName m) where
    show (CommitObjectId coid) = T.unpack (renderObjOid coid)
    show (CommitRefName name)  = show name
    show (CommitReference ref) = show (referenceName ref)

nameOfCommit :: Commit m -> CommitName m
nameOfCommit = CommitObjectId . commitOid

commitNameToRef :: Repository m => CommitName m -> m (Maybe (CommitRef m))
commitNameToRef (CommitObjectId coid) = return (Just (ByOid coid))
commitNameToRef (CommitRefName name)  = resolveReference name
commitNameToRef (CommitReference ref) = referenceToRef Nothing (Just ref)

renderCommitName :: Repository m => CommitName m -> Text
renderCommitName (CommitObjectId coid) = renderObjOid coid
renderCommitName (CommitRefName name)  = name
renderCommitName (CommitReference ref) = referenceName ref

copyOid :: (Repository m, Repository n) => Oid m -> n (Oid n)
copyOid = parseOid . renderOid

copyCommitOid :: (Repository m, MonadGit m, Repository n, MonadGit n)
              => CommitOid m -> n (CommitOid n)
copyCommitOid coid = do
    ncoid <- parseOid (renderObjOid coid)
    return (Tagged ncoid)

copyCommitName :: (Repository m, MonadGit m, Repository n, MonadGit n)
               => CommitName m -> n (Maybe (CommitName n))
copyCommitName (CommitObjectId coid) =
    Just . CommitObjectId . Tagged <$> parseOid (renderObjOid coid)
copyCommitName (CommitRefName name) = return (Just (CommitRefName name))
copyCommitName (CommitReference ref) =
    fmap CommitReference <$> lookupReference (referenceName ref)

{- $objects -}
data ObjRef m a = ByOid !(Tagged a (Oid m)) | Known !a

type BlobRef m   = ObjRef m (Blob m)
type TreeRef m   = ObjRef m (Tree m)
type CommitRef m = ObjRef m (Commit m)
type TagRef m    = ObjRef m (Tag m)

data Object m = BlobObj      !(BlobRef m)
              | TreeObj      !(TreeRef m)
              | CommitObj    !(CommitRef m)
              | TagObj       !(TagRef m)

objectOid :: Repository m => Object m -> m (Oid m)
objectOid (BlobObj ref)   = return . untag $ blobRefOid ref
objectOid (TreeObj ref)   = return . untag $ treeRefOid ref
objectOid (CommitObj ref) = return . untag $ commitRefOid ref
objectOid (TagObj ref)    = return . untag $ tagRefOid ref

{- $blobs -}
data Blob m = Blob { blobOid      :: !(BlobOid m)
                   , blobContents :: !(BlobContents m) }

blobRefOid :: Repository m => BlobRef m -> BlobOid m
blobRefOid (ByOid oid) = oid
blobRefOid (Known (Blob {..})) = blobOid

resolveBlobRef :: Repository m => BlobRef m -> m (Blob m)
resolveBlobRef (ByOid oid) = lookupBlob oid
resolveBlobRef (Known obj) = return obj

type ByteSource m = Producer m ByteString

data BlobContents m = BlobString !ByteString
                    | BlobStream !(ByteSource m)
                    | BlobSizedStream !(ByteSource m) !Int

data BlobKind = PlainBlob | ExecutableBlob | SymlinkBlob | UnknownBlob
              deriving (Show, Eq, Enum)

instance Eq (BlobContents m) where
  BlobString str1 == BlobString str2 = str1 == str2
  _ == _ = False

{- $trees -}
newtype TreeT m a = TreeT { runTreeT :: StateT (TreeBuilder m) m a }

instance Functor m => Functor (TreeT m) where
    fmap f (TreeT t) = TreeT (fmap f t)

instance Monad m => Monad (TreeT m) where
    return x = TreeT (return x)
    TreeT x >>= f = TreeT (x >>= runTreeT . f)

instance (Functor m, Monad m) => Applicative (TreeT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (TreeT m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus m) => MonadPlus (TreeT m) where
    mzero       = TreeT $ mzero
    m `mplus` n = TreeT $ runTreeT m `mplus` runTreeT n

instance (MonadFix m) => MonadFix (TreeT m) where
    mfix f = TreeT $ mfix $ \ ~a -> runTreeT (f a)

instance MonadTrans TreeT where
    lift m = TreeT $ lift m

instance (MonadIO m) => MonadIO (TreeT m) where
    liftIO = lift . liftIO

getBuilder :: Monad m => TreeT m (TreeBuilder m)
getBuilder = TreeT get

putBuilder :: Monad m => TreeBuilder m -> TreeT m ()
putBuilder = TreeT . put

data BuilderAction = GetEntry | PutEntry | DropEntry
                   deriving (Eq, Show)

getEntry :: Repository m => FilePath -> TreeT m (Maybe (TreeEntry m))
getEntry path = do
    tb <- getBuilder
    snd <$> lift (queryTreeBuilder tb path GetEntry
                  (toModifyTreeResult TreeEntryPersistent))

putEntry :: Repository m => FilePath -> TreeEntry m -> TreeT m ()
putEntry path ent = do
    tb  <- getBuilder
    tb' <- fst <$> lift (queryTreeBuilder tb path PutEntry
                         (const (TreeEntryMutated ent)))
    putBuilder tb'

dropEntry :: Repository m => FilePath -> TreeT m ()
dropEntry path = do
    tb  <- getBuilder
    tb' <- fst <$> lift (queryTreeBuilder tb path DropEntry
                         (const TreeEntryDeleted))
    putBuilder tb'

putBlob' :: Repository m => FilePath -> BlobOid m -> BlobKind -> TreeT m ()
putBlob' path b kind = putEntry path (BlobEntry b kind)

putBlob :: Repository m => FilePath -> BlobOid m -> TreeT m ()
putBlob path b = putBlob' path b PlainBlob

putTree :: Repository m => FilePath -> TreeOid m -> TreeT m ()
putTree path t = putEntry path (TreeEntry t)

putCommit :: Repository m => FilePath -> CommitOid m -> TreeT m ()
putCommit path c = putEntry path (CommitEntry c)

doWithTree :: Repository m => Maybe (Tree m) -> TreeT m a -> m (a, TreeRef m)
doWithTree mtr act =
    fst <$> (runStateT (runTreeT go) =<< newTreeBuilder mtr)
  where
    go = liftM2 (,) act currentTreeRef

withTree :: Repository m => Tree m -> TreeT m a -> m (a, TreeRef m)
withTree tr = doWithTree (Just tr)

withTreeRef :: Repository m => TreeRef m -> TreeT m a -> m (a, TreeRef m)
withTreeRef ref action = do
    tree <- resolveTreeRef ref
    doWithTree (Just tree) action

mutateTree :: Repository m => Tree m -> TreeT m a -> m (TreeRef m)
mutateTree tr action = snd <$> withTree tr action

mutateTreeRef :: Repository m => TreeRef m -> TreeT m a -> m (TreeRef m)
mutateTreeRef tr action = snd <$> withTreeRef tr action

currentTreeRef :: Repository m => TreeT m (TreeRef m)
currentTreeRef = do
    tb <- getBuilder
    (tb', tref) <- lift $ writeTreeBuilder tb
    putBuilder tb'
    return tref

currentTree :: Repository m => TreeT m (Tree m)
currentTree = lift . resolveTreeRef =<< currentTreeRef

withNewTree :: Repository m => TreeT m a -> m (a, TreeRef m)
withNewTree = doWithTree Nothing

createTree :: Repository m => TreeT m a -> m (TreeRef m)
createTree action = snd <$> withNewTree action

data TreeEntry m = BlobEntry   { blobEntryOid   :: !(BlobOid m)
                               , blobEntryKind  :: !BlobKind }
                 | TreeEntry   { treeEntryOid   :: !(TreeOid m) }
                 | CommitEntry { commitEntryOid :: !(CommitOid m) }

getTreeEntryOid :: Repository m => TreeEntry m -> Oid m
getTreeEntryOid (BlobEntry boid _) = untag boid
getTreeEntryOid (TreeEntry toid)   = untag toid
getTreeEntryOid (CommitEntry coid) = untag coid

blobEntry :: Repository m => BlobOid m -> BlobKind -> TreeEntry m
blobEntry = BlobEntry

treeEntry :: Repository m => Tree m -> TreeEntry m
treeEntry = TreeEntry . treeOid

commitEntry :: Repository m => Commit m -> TreeEntry m
commitEntry = CommitEntry . commitOid

data ModifyTreeResult m = TreeEntryNotFound
                        | TreeEntryDeleted
                        | TreeEntryPersistent (TreeEntry m)
                        | TreeEntryMutated (TreeEntry m)

fromModifyTreeResult :: ModifyTreeResult m -> Maybe (TreeEntry m)
fromModifyTreeResult TreeEntryNotFound       = Nothing
fromModifyTreeResult TreeEntryDeleted        = Nothing
fromModifyTreeResult (TreeEntryPersistent x) = Just x
fromModifyTreeResult (TreeEntryMutated x)    = Just x

toModifyTreeResult :: (TreeEntry m -> ModifyTreeResult m)
                   -> Maybe (TreeEntry m)
                   -> ModifyTreeResult m
toModifyTreeResult _ Nothing  = TreeEntryNotFound
toModifyTreeResult f (Just x) = f x

-- | A 'Tree' is anything that is "treeish".
treeRef :: Tree m -> TreeRef m
treeRef = Known

treeRefOid :: Repository m => TreeRef m -> TreeOid m
treeRefOid (ByOid x) = x
treeRefOid (Known x) = treeOid x

resolveTreeRef :: Repository m => TreeRef m -> m (Tree m)
resolveTreeRef (ByOid oid) = lookupTree oid
resolveTreeRef (Known obj) = return obj

{- $commits -}
data Signature = Signature
    { signatureName  :: !Text
    , signatureEmail :: !Text
    , signatureWhen  :: !ZonedTime
    } deriving Show

instance Default Signature where
    def = Signature
        { signatureName  = T.empty
        , signatureEmail = T.empty
        , signatureWhen  = ZonedTime
            { zonedTimeToLocalTime = LocalTime
                { localDay = ModifiedJulianDay 0
                , localTimeOfDay = TimeOfDay 0 0 0
                }
            , zonedTimeZone = utc
            }
        }

data Commit m = Commit
    { commitOid       :: !(CommitOid m)
    , commitParents   :: ![CommitRef m]
    , commitTree      :: !(TreeRef m)
    , commitAuthor    :: !Signature
    , commitCommitter :: !Signature
    , commitLog       :: !Text
    , commitEncoding  :: !Text
    }

commitRef :: Commit m -> CommitRef m
commitRef = Known

commitRefTarget :: Commit c -> RefTarget m (Commit c)
commitRefTarget = RefObj . Known

commitRefOid :: Repository m => CommitRef m -> CommitOid m
commitRefOid (ByOid x) = x
commitRefOid (Known x) = commitOid x

resolveCommitRef :: Repository m => CommitRef m -> m (Commit m)
resolveCommitRef (ByOid oid) = lookupCommit oid
resolveCommitRef (Known obj) = return obj

referenceToRef :: Repository m
               => Maybe Text -> Maybe (Reference m (Commit m))
               -> m (Maybe (CommitRef m))
referenceToRef mname mref =
    case mref of
        Nothing -> return Nothing
        Just (Reference { referenceTarget = RefObj x }) ->
            return (Just x)
        Just ref@(Reference { referenceTarget = RefSymbolic name' }) ->
            if fromMaybe name' mname /= name'
            then resolveReference name'
            else failure (ReferenceLookupFailed (referenceName ref))

{- $tags -}

data Tag m = Tag
    { tagOid    :: !(TagOid m)
    , tagCommit :: !(CommitRef m)
    }

tagRefOid :: Repository m => TagRef m -> TagOid m
tagRefOid (ByOid x) = x
tagRefOid (Known x) = tagOid x

{- $merges -}

data ModificationKind = Unchanged | Modified | Added | Deleted | TypeChanged
                      deriving (Eq, Ord, Enum, Show, Read)

data MergeStatus
    = NoConflict
    | BothModified
    | LeftModifiedRightDeleted
    | LeftDeletedRightModified
    | BothAdded
    | LeftModifiedRightTypeChanged
    | LeftTypeChangedRightModified
    | LeftDeletedRightTypeChanged
    | LeftTypeChangedRightDeleted
    | BothTypeChanged
    deriving (Eq, Ord, Enum, Show, Read)

mergeStatus :: ModificationKind -> ModificationKind -> MergeStatus
mergeStatus Unchanged Unchanged     = NoConflict
mergeStatus Unchanged Modified      = NoConflict
mergeStatus Unchanged Added         = undefined
mergeStatus Unchanged Deleted       = NoConflict
mergeStatus Unchanged TypeChanged   = NoConflict

mergeStatus Modified Unchanged      = NoConflict
mergeStatus Modified Modified       = BothModified
mergeStatus Modified Added          = undefined
mergeStatus Modified Deleted        = LeftModifiedRightDeleted
mergeStatus Modified TypeChanged    = LeftModifiedRightTypeChanged

mergeStatus Added Unchanged         = undefined
mergeStatus Added Modified          = undefined
mergeStatus Added Added             = BothAdded
mergeStatus Added Deleted           = undefined
mergeStatus Added TypeChanged       = undefined

mergeStatus Deleted Unchanged       = NoConflict
mergeStatus Deleted Modified        = LeftDeletedRightModified
mergeStatus Deleted Added           = undefined
mergeStatus Deleted Deleted         = NoConflict
mergeStatus Deleted TypeChanged     = LeftDeletedRightTypeChanged

mergeStatus TypeChanged Unchanged   = NoConflict
mergeStatus TypeChanged Modified    = LeftTypeChangedRightModified
mergeStatus TypeChanged Added       = undefined
mergeStatus TypeChanged Deleted     = LeftTypeChangedRightDeleted
mergeStatus TypeChanged TypeChanged = BothTypeChanged

data MergeResult m
    = MergeSuccess
        { mergeCommit    :: CommitOid m
        }
    | MergeConflicted
        { mergeCommit    :: CommitOid m
        , mergeHeadLeft  :: CommitOid m
        , mergeHeadRight :: CommitOid m
        , mergeConflicts :: Map FilePath (ModificationKind, ModificationKind)
        }

instance Repository m => Show (MergeResult m) where
    show (MergeSuccess mc) = "MergeSuccess (" ++ show mc ++ ")"
    show (MergeConflicted mc hl hr cs) =
        "MergeResult"
     ++ "\n    { mergeCommit    = " ++ show mc
     ++ "\n    , mergeHeadLeft  = " ++ show hl
     ++ "\n    , mergeHeadRight = " ++ show hr
     ++ "\n    , mergeConflicts = " ++ show cs
     ++ "\n    }"

copyConflict :: (Repository m, MonadGit m, Repository n, MonadGit n)
             => MergeResult m -> n (MergeResult n)
copyConflict (MergeSuccess mc) =
    MergeSuccess <$> (Tagged <$> parseOid (renderObjOid mc))
copyConflict (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> (Tagged <$> parseOid (renderObjOid hl))
                    <*> (Tagged <$> parseOid (renderObjOid hr))
                    <*> (Tagged <$> parseOid (renderObjOid mc))
                    <*> pure cs

{- $miscellaneous -}

newtype SHA = SHA B.ByteString deriving (Eq, Ord, Read)

instance Show SHA where
    show = T.unpack . shaToText

instance Bin.Binary SHA where
    put (SHA t) = Bin.put t
    get = SHA <$> Bin.get

instance Hashable SHA where
    hashWithSalt salt (SHA bs) = hashWithSalt salt bs

instance IsOid SHA where
    renderOid = shaToText

shaToText :: SHA -> Text
shaToText (SHA bs) = T.decodeUtf8 (B16.encode bs)

textToSha :: Monad m => Text -> m SHA
textToSha t =
    case B16.decode $ T.encodeUtf8 t of
        (bs, "") -> return (SHA bs)
        _ -> fail "Invalid base16 encoding"

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Repository m => Text -> m ByteString
catBlob str =
    if len == 40
    then do
        oid <- parseOid str
        lookupBlob (Tagged oid) >>= blobToByteString

    else do
        obj <- lookupObject str
        case obj of
            BlobObj (ByOid oid) -> lookupBlob oid >>= blobToByteString
            BlobObj (Known x)   -> blobToByteString x
            _ -> failure (ObjectLookupFailed str len)
  where
    len = T.length str

catBlobUtf8 :: Repository m => Text -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobContentsToByteString :: Repository m => BlobContents m -> m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobContentsToByteString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

blobToByteString :: Repository m => Blob m -> m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

data TreeBuilder m = TreeBuilder
    { mtbBaseTreeRef    :: Maybe (TreeRef m)
    , mtbPendingUpdates :: HashMap Text (TreeBuilder m)
    , mtbNewBuilder     :: Maybe (Tree m) -> m (TreeBuilder m)
    , mtbWriteContents  :: TreeBuilder m -> m (TreeBuilder m, TreeRef m)
    , mtbLookupEntry    :: Text -> m (Maybe (TreeEntry m))
    , mtbEntryCount     :: m Int
    , mtbPutEntry       :: TreeBuilder m -> Text -> TreeEntry m
                        -> m (TreeBuilder m)
    , mtbDropEntry      :: TreeBuilder m -> Text -> m (TreeBuilder m)
    }

instance Monad m => Monoid (TreeBuilder m) where
    mempty = TreeBuilder
        { mtbBaseTreeRef    = Nothing
        , mtbPendingUpdates = HashMap.empty
        , mtbNewBuilder     = error "Implement TreeBuilder.mtbNewBuilder"
        , mtbWriteContents  = error "Implement TreeBuilder.mtbWriteContents"
        , mtbLookupEntry    = \_ -> return Nothing
        , mtbEntryCount     = return 0
        , mtbPutEntry       = \tb' _ _ -> return tb'
        , mtbDropEntry      = \tb' _ -> return tb'
        }
    tb1 `mappend` tb2 = tb2
        { mtbBaseTreeRef    = mtbBaseTreeRef tb1
        , mtbPendingUpdates = mtbPendingUpdates tb1
        }

emptyTreeId :: Text
emptyTreeId = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- | Perform a query action on a TreeBuilder using the supplied action kind
--   and user function.
--
--   This is a complex algorithm which has been rewritten many times, so I
--   will try to guide you through it as best I can.
queryTreeBuilder :: Repository m
                  => TreeBuilder m
                  -> FilePath
                  -> BuilderAction
                  -> (Maybe (TreeEntry m) -> ModifyTreeResult m)
                  -> m (TreeBuilder m, Maybe (TreeEntry m))
queryTreeBuilder builder path kind f =
    fmap fromModifyTreeResult <$> walk builder (splitPath path)
  where
    walk _ [] = error "queryTreeBuilder called without a path"
    walk tb (name:names) = do
        y <- case HashMap.lookup name (mtbPendingUpdates tb) of
            Just x  -> return $ Left x
            Nothing -> do
                mentry <- mtbLookupEntry tb name
                case mentry of
                    Nothing
                        | kind == PutEntry && not (null names) ->
                            Left <$> mtbNewBuilder tb Nothing
                        | otherwise -> return $ Right Nothing
                    x@(Just _) -> return $ Right x
        x <- update tb name names y
        return x

    update tb name [] (Left stb)
        | kind == GetEntry = do
            (_, tref) <- writeTreeBuilder stb
            returnTree tb name $ f (Just (TreeEntry (treeRefOid tref)))
        | otherwise = returnTree tb name (f Nothing)

    update tb name [] (Right y)   = returnTree tb name (f y)
    update tb _ _ (Right Nothing) = return (tb, TreeEntryNotFound)

    update _ _ _ (Right (Just BlobEntry {})) =
        failure TreeCannotTraverseBlob
    update _ _ _ (Right (Just CommitEntry {})) =
        failure TreeCannotTraverseCommit

    update tb name names arg = do
        stb <- case arg of
            Left stb' -> return stb'
            Right (Just (TreeEntry st')) -> do
                tree <- lookupTree st'
                mtbNewBuilder tb (Just tree)
            _ -> error "queryTreeBuilder encountered the impossible"

        (stb', z) <- walk stb names
        let tb' = postUpdate tb stb' name z
        return $ tb' `seq` (tb', z)

    returnTree tb n z = do
        tb' <- case z of
            TreeEntryNotFound     -> return tb
            TreeEntryPersistent _ -> return tb
            TreeEntryDeleted      -> mtbDropEntry tb tb n
            TreeEntryMutated z'   -> mtbPutEntry tb tb n z'
        return $ tb' `seq` (tb', z)

    postUpdate tb _ _ TreeEntryNotFound       = tb
    postUpdate tb _ _ (TreeEntryPersistent _) = tb
    postUpdate tb stb name _ =
        tb { mtbPendingUpdates =
                  HashMap.insert name stb (mtbPendingUpdates tb) }

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTreeBuilder :: Repository m
                 => TreeBuilder m
                 -> m (TreeBuilder m, TreeRef m)
writeTreeBuilder builder = do
    (tb', mtref) <- go builder
    tref <- case mtref of
        Nothing -> do
            emptyTreeOid <- parseObjOid emptyTreeId
            return $ ByOid emptyTreeOid
        Just tref -> return tref
    return (tb', tref)
  where
    go tb = do
        tb' <- foldM update tb $ HashMap.toList (mtbPendingUpdates tb)
        let tb'' = tb' { mtbPendingUpdates = HashMap.empty }
        cnt <- mtbEntryCount tb''
        if cnt == 0
            then return (tb'', Nothing)
            else fmap Just <$> mtbWriteContents tb'' tb''

    update tb' (k,v) = do
        -- The intermediate TreeBuilder will be dropped after this fold is
        -- completed, by setting mtbPendingUpdates to HashMap.empty, above.
        (_,mtref) <- go v
        case mtref of
            Nothing   -> mtbDropEntry tb' tb' k
            Just tref -> mtbPutEntry tb' tb' k (TreeEntry (treeRefOid tref))

treeBlobEntries :: Repository m => Tree m -> m [(FilePath,TreeEntry m)]
treeBlobEntries tree =
    mconcat <$> traverseEntries go tree
  where
    go fp e@(BlobEntry _ PlainBlob)      = return [(fp,e)]
    go fp e@(BlobEntry _ ExecutableBlob) = return [(fp,e)]
    go _ _ = return []

commitTreeEntry :: Repository m
                => Commit m
                -> FilePath
                -> m (Maybe (TreeEntry m))
commitTreeEntry c path =
    flip getTreeEntry path =<< resolveTreeRef (commitTree c)

copyBlob :: (Repository m, Repository (t m), MonadTrans t)
         => BlobRef m
         -> HashSet Text
         -> t m (BlobOid (t m), HashSet Text)
copyBlob blobr needed = do
    let oid = untag (blobRefOid blobr)
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        bs <- lift $ blobToByteString
              =<< resolveBlobRef (ByOid (Tagged oid))
        boid <- createBlob (BlobString bs)

        let x = HashSet.delete sha needed
        return $ boid `seq` x `seq` (boid, x)

        else return (Tagged oid2, needed)

copyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
              => TreeEntry m
              -> HashSet Text
              -> t m (TreeEntry (t m), HashSet Text)
copyTreeEntry (BlobEntry oid kind) needed = do
    (b,needed') <- copyBlob (ByOid oid) needed
    return (BlobEntry b kind, needed')
copyTreeEntry (CommitEntry oid) needed = do
    coid <- parseOid (renderObjOid oid)
    return (CommitEntry (Tagged coid), needed)
copyTreeEntry (TreeEntry _) _ = error "This should never be called"

copyTree :: (Repository m, Repository (t m), MonadTrans t)
         => TreeRef m
         -> HashSet Text
         -> t m (TreeRef (t m), HashSet Text)
copyTree tr needed = do
    let oid = untag (treeRefOid tr)
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        tree            <- lift $ resolveTreeRef tr
        entries         <- lift $ traverseEntries (curry return) tree
        (needed', tref) <- withNewTree $ foldM doCopyTreeEntry needed entries

        let x = HashSet.delete sha needed'
        return $ tref `seq` x `seq` (tref, x)

        else return (ByOid (Tagged oid2), needed)
  where
    doCopyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
                    => HashSet Text -> (FilePath, TreeEntry m)
                    -> TreeT (t m) (HashSet Text)
    doCopyTreeEntry needed' (_,TreeEntry {}) = return needed'
    doCopyTreeEntry needed' (fp,ent) = do
        (ent2,needed'') <- lift $ copyTreeEntry ent needed'
        putEntry fp ent2
        return needed''

copyCommit :: (Repository m, Repository (t m), MonadTrans t)
           => CommitRef m
           -> Maybe Text
           -> HashSet Text
           -> t m (CommitRef (t m), HashSet Text)
copyCommit cr mref needed = do
    let oid = untag (commitRefOid cr)
        sha = renderOid oid
    commit <- lift $ resolveCommitRef cr
    oid2   <- parseOid sha
    if HashSet.member sha needed
        then do
        let parents = commitParents commit
        (parentRefs,needed') <- foldM copyParent ([],needed) parents
        (tr,needed'') <- copyTree (commitTree commit) needed'

        commit' <- createCommit (reverse parentRefs) tr
            (commitAuthor commit)
            (commitCommitter commit)
            (commitLog commit)
            mref

        let cref = commitRef $! commit'
            x    = HashSet.delete sha needed''
        return $ cref `seq` x `seq` (cref, x)

        else return (ByOid (Tagged oid2), needed)
  where
    copyParent (prefs,needed') cref = do
        (cref2,needed'') <- copyCommit cref Nothing needed'
        let x = cref2 `seq` (cref2:prefs)
        return $ x `seq` needed'' `seq` (x,needed'')

-- | Given a list of objects (commit and top-level trees) return by
--   'missingObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
allMissingObjects :: Repository m => [Object m] -> m [Object m]
allMissingObjects objs =
    fmap concat . forM objs $ \obj -> case obj of
        TreeObj ref -> do
            tr       <- resolveTreeRef ref
            subobjss <- flip traverseEntries tr $ \_ ent ->
                return $ case ent of
                    BlobEntry oid _ -> [BlobObj (ByOid oid)]
                    TreeEntry oid   -> [TreeObj (ByOid oid)]
                    _ -> []
            return (obj:concat subobjss)
        _ -> return [obj]

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works.
genericPushCommit :: (Repository m, Repository (t m), MonadTrans t)
                  => CommitName m -> Text -> t m (CommitRef (t m))
genericPushCommit cname remoteRefName = do
    mrref    <- lookupReference remoteRefName
    commits1 <- lift $ traverseCommits crefToSha cname
    fastForward <- case mrref of
        Just rref -> do
            mrsha <- referenceSha rref
            case mrsha of
                Nothing -> failure (PushNotFastForward $
                                    "Could not find SHA for " <> remoteRefName)
                Just rsha
                    | rsha `elem` commits1 -> do
                        roid <- lift $ parseOid rsha
                        return $ Just (Just (CommitObjectId (Tagged roid)))
                    | otherwise -> do
                        failure (PushNotFastForward $
                                 "SHA " <> rsha
                                        <> " not found in remote")
        Nothing -> return (Just Nothing)
    case fastForward of
        Nothing -> failure (PushNotFastForward "unexpected")
        Just liftedMrref -> do
            objs <- lift $ allMissingObjects
                        =<< missingObjects liftedMrref cname
            shas <- mapM (\obj -> renderOid <$> lift (objectOid obj)) objs
            mref <- lift $ commitNameToRef cname
            case mref of
                Nothing -> failure (ReferenceLookupFailed (T.pack (show cname)))
                Just ref -> do
                    (cref,_) <- copyCommit ref Nothing (HashSet.fromList shas)
                    -- jww (2013-04-18): This is something the user must
                    -- decide to do
                    -- updateRef_ remoteRefName (RefObj cref)
                    return cref
  where
    referenceSha ref = do
        r <- referenceToRef Nothing (Just ref)
        return $ renderObjOid . commitRefOid <$> r

    crefToSha cref  = return (renderObjOid (commitRefOid cref))

commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
commitHistoryFirstParent c =
    case commitParents c of
        []    -> return [c]
        (p:_) -> do ps <- commitHistoryFirstParent =<< resolveCommitRef p
                    return (c:ps)

data PinnedEntry m = PinnedEntry
    { pinnedOid    :: Oid m
    , pinnedCommit :: Commit m
    , pinnedEntry  :: TreeEntry m
    }

identifyEntry :: Repository m => Commit m -> TreeEntry m -> m (PinnedEntry m)
identifyEntry co x = do
    let oid = case x of
            BlobEntry oid' _ -> untag oid'
            TreeEntry oid'   -> untag oid'
            CommitEntry oid' -> untag oid'
    return (PinnedEntry oid co x)

commitEntryHistory :: Repository m => Commit m -> FilePath -> m [PinnedEntry m]
commitEntryHistory c path =
    map head . filter (not . null) . groupBy ((==) `on` pinnedOid) <$> go c
  where
    go co = do
        entry <- getCommitTreeEntry co
        rest  <- case commitParents co of
            []    -> return []
            (p:_) -> go =<< resolveCommitRef p
        return $ maybe rest (:rest) entry

    getCommitTreeEntry co = do
        ce <- commitTreeEntry co path
        case ce of
            Nothing  -> return Nothing
            Just ce' -> Just <$> identifyEntry co ce'

getCommitParents :: Repository m => Commit m -> m [Commit m]
getCommitParents = traverse resolveCommitRef . commitParents

resolveReferenceTree :: Repository m => Text -> m (Maybe (Tree m))
resolveReferenceTree refName = do
    c <- resolveReference refName
    case c of
        Nothing -> return Nothing
        Just c' ->
            Just <$> (resolveCommitRef c' >>= resolveTreeRef . commitTree)

withNewRepository :: (Repository (t m), MonadGit (t m),
                      MonadBaseControl IO m, MonadIO m, MonadTrans t)
                  => RepositoryFactory t m c
                  -> FilePath -> t m a -> m a
withNewRepository factory path action = do
    liftIO $ do
        exists <- isDirectory path
        when exists $ removeTree path

    -- we want exceptions to leave the repo behind
    a <- withRepository' factory (defaultOptions factory)
        { repoPath       = path
        , repoIsBare     = True
        , repoAutoCreate = True
        } action

    liftIO $ do
        exists <- isDirectory path
        when exists $ removeTree path

    return a

withNewRepository' :: (Repository (t m), MonadGit (t m),
                       MonadBaseControl IO m, MonadIO m, MonadTrans t)
                   => RepositoryFactory t m c -> FilePath -> t m a -> m a
withNewRepository' factory path action =
    Exc.bracket_ recover recover $
        withRepository' factory (defaultOptions factory)
            { repoPath       = path
            , repoIsBare     = True
            , repoAutoCreate = True
            } action
  where
    recover = liftIO $ do
        exists <- isDirectory path
        when exists $ removeTree path


data RepositoryOptions = RepositoryOptions
    { repoPath       :: !FilePath
    , repoIsBare     :: !Bool
    , repoAutoCreate :: !Bool
    }

instance Default RepositoryOptions where
    def = RepositoryOptions "" True True

data RepositoryFactory t m c = RepositoryFactory
    { openRepository  :: RepositoryOptions -> m c
    , runRepository   :: forall a. c -> t m a -> m a
    , closeRepository :: c -> m ()
    , getRepository   :: t m c
    , defaultOptions  :: !RepositoryOptions
    , startupBackend  :: m ()
    , shutdownBackend :: m ()
    }

withBackendDo :: (MonadIO m, MonadBaseControl IO m)
              => RepositoryFactory t m a -> m b -> m b
withBackendDo fact f = do
    startupBackend fact
    Exc.finally f (liftIO performGC >> shutdownBackend fact)

withRepository' :: (Repository (t m), MonadTrans t,
                    MonadBaseControl IO m, MonadIO m)
                => RepositoryFactory t m c
                -> RepositoryOptions
                -> t m a
                -> m a
withRepository' factory opts action =
    Exc.bracket
        (openRepository factory opts)
        (closeRepository factory)
        (flip (runRepository factory) action)

withRepository :: (Repository (t m), MonadTrans t,
                   MonadBaseControl IO m, MonadIO m)
               => RepositoryFactory t m c
               -> FilePath
               -> t m a
               -> m a
withRepository factory path =
    withRepository' factory
        (defaultOptions factory) { repoPath = path }

-- Git.hs
