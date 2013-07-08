{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

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
       , ObjectOid(..)
       , objectOid
       , loadObject
       , objectToObjOid
       , untagObjOid
       , SHA(..)
       , textToSha
       , shaToText

       , Blob(..)
       , BlobOid
       , BlobContents(..)
       , BlobKind(..)
       , ByteSource
       , blobEntry
       , createBlobUtf8
       , catBlob
       , catBlobUtf8
       , copyBlob
       , blobContentsToByteString
       , blobToByteString

       , TreeT
       , TreeBuilder(..)
       , ModifiedBuilder(..)
       , TreeEntry(..)
       , TreeOid
       , createTree
       , withNewTree
       , mutateTree
       , mutateTreeOid
       , currentTree
       , withTree
       , withTreeOid
       , dropEntry
       , getEntry
       , putBlob
       , putBlob'
       , putCommit
       , putEntry
       , putTree
       , treeEntry
       , getTreeEntryOid
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
       , Signature(..)
       , commitEntry
       , commitRefTarget
       , getCommitParents
       , copyCommitName
       , copyCommitOid
       , nameOfCommit
       , commitNameToOid
       , renderCommitName
       , commitTreeEntry
       , copyCommit
       , genericPushCommit

       , PinnedEntry(..)
       , commitHistoryFirstParent
       , commitEntryHistory
       , identifyEntry

       , Tag(..)
       , TagOid

       , RefTarget(..)
       , Reference(..)
       , referenceToOid
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
    createReference  :: Text -> RefTarget m -> m (Reference m)
    createReference_ :: Text -> RefTarget m -> m ()
    createReference_ = (void .) . createReference
    lookupReference  :: Text -> m (Maybe (Reference m))
    updateReference  :: Text -> RefTarget m -> m (Reference m)
    updateReference_ :: Text -> RefTarget m -> m ()
    updateReference_ = (void .) . updateReference
    deleteReference  :: Text -> m ()

    allReferences :: m [Reference m]
    allReferences = catMaybes <$> (mapM lookupReference =<< allReferenceNames)

    allReferenceNames :: m [Text]
    allReferenceNames = map referenceName <$> allReferences

    resolveReference :: Text -> m (Maybe (CommitOid m))
    resolveReference name = lookupReference name >>= referenceToOid (Just name)

    -- Lookup
    lookupCommit :: CommitOid m -> m (Commit m)
    lookupTree   :: TreeOid m -> m (Tree m)
    lookupBlob   :: BlobOid m -> m (Blob m)
    lookupTag    :: TagOid m -> m (Tag m)

    lookupObject :: Text -> m (Object m)
    lookupObjectOid :: Text -> m (ObjectOid m)
    lookupObjectOid = fmap objectToObjOid . lookupObject
    existsObject :: Oid m -> m Bool

    traverseObjects :: forall a.
                       (Oid m -> m a) -> Maybe (CommitOid m) -> m [a]
    traverseObjects_ :: (Oid m -> m ()) -> Maybe (CommitOid m) -> m ()
    traverseObjects_ = (void .) . traverseObjects

    pushCommit :: (MonadTrans t, MonadGit m, MonadGit (t m),
                   Repository m, Repository (t m))
               => CommitOid m -> Maybe Text -> Text -> t m (CommitOid (t m))

    traverseCommits :: forall a.
                       (CommitOid m -> m a) -> CommitOid m -> m [a]
    traverseCommits_ :: (CommitOid m -> m ()) -> CommitOid m -> m ()
    traverseCommits_ = (void .) . traverseCommits

    missingObjects :: Maybe (CommitOid m) -- ^ A commit we may already have
                   -> CommitOid m         -- ^ The commit we need
                   -> m [ObjectOid m]     -- ^ All the objects in between

    -- Object creation, not to be called by users directly
    newTreeBuilder :: Maybe (Tree m) -> m (TreeBuilder m)

    treeOid :: Tree m -> TreeOid m
    getTreeEntry :: Tree m -> FilePath -> m (Maybe (TreeEntry m))
    traverseEntries :: (FilePath -> TreeEntry m -> m a) -> Tree m -> m [a]
    traverseEntries_ :: (FilePath -> TreeEntry m -> m a) -> Tree m -> m ()
    traverseEntries_ = (void .) . traverseEntries

    hashContents :: BlobContents m -> m (BlobOid m)
    createBlob   :: BlobContents m -> m (BlobOid m)
    createCommit :: [CommitOid m] -> TreeOid m
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
data RefTarget m = RefObj !(CommitOid m) | RefSymbolic !Text

data Reference m = Reference
    { referenceName   :: !Text
    , referenceTarget :: !(RefTarget m) }

data CommitName m = CommitObjectId !(CommitOid m)
                  | CommitReferenceName !Text
                  | CommitReference !(Reference m)

instance Repository m => Show (CommitName m) where
    show (CommitObjectId coid) = T.unpack (renderObjOid coid)
    show (CommitReferenceName name)  = show name
    show (CommitReference ref) = show (referenceName ref)

nameOfCommit :: Commit m -> CommitName m
nameOfCommit = CommitObjectId . commitOid

commitNameToOid :: Repository m => CommitName m -> m (Maybe (CommitOid m))
commitNameToOid (CommitObjectId coid) = return (Just coid)
commitNameToOid (CommitReferenceName name)  = resolveReference name
commitNameToOid (CommitReference ref) = referenceToOid Nothing (Just ref)

renderCommitName :: Repository m => CommitName m -> Text
renderCommitName (CommitObjectId coid) = renderObjOid coid
renderCommitName (CommitReferenceName name)  = name
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
copyCommitName (CommitReferenceName name) =
    return (Just (CommitReferenceName name))
copyCommitName (CommitReference ref) =
    fmap CommitReference <$> lookupReference (referenceName ref)

{- $objects -}
data ObjectOid m = BlobObjOid   !(BlobOid m)
                 | TreeObjOid   !(TreeOid m)
                 | CommitObjOid !(CommitOid m)
                 | TagObjOid    !(TagOid m)

data Object m = BlobObj   !(Blob m)
              | TreeObj   !(Tree m)
              | CommitObj !(Commit m)
              | TagObj    !(Tag m)

objectOid :: Repository m => Object m -> Oid m
objectOid (BlobObj obj)   = untag (blobOid obj)
objectOid (TreeObj obj)   = untag (treeOid obj)
objectOid (CommitObj obj) = untag (commitOid obj)
objectOid (TagObj obj)    = untag (tagOid obj)

loadObject :: Repository m => ObjectOid m -> m (Object m)
loadObject (BlobObjOid oid)   = BlobObj   <$> lookupBlob oid
loadObject (TreeObjOid oid)   = TreeObj   <$> lookupTree oid
loadObject (CommitObjOid oid) = CommitObj <$> lookupCommit oid
loadObject (TagObjOid oid)    = TagObj    <$> lookupTag oid

objectToObjOid :: Repository m => Object m -> ObjectOid m
objectToObjOid (BlobObj obj)   = BlobObjOid (blobOid obj)
objectToObjOid (TreeObj obj)   = TreeObjOid (treeOid obj)
objectToObjOid (CommitObj obj) = CommitObjOid (commitOid obj)
objectToObjOid (TagObj obj)    = TagObjOid (tagOid obj)

untagObjOid :: Repository m => ObjectOid m -> Oid m
untagObjOid (BlobObjOid oid)   = untag oid
untagObjOid (TreeObjOid oid)   = untag oid
untagObjOid (CommitObjOid oid) = untag oid
untagObjOid (TagObjOid oid)    = untag oid

{- $blobs -}
data Blob m = Blob { blobOid      :: !(BlobOid m)
                   , blobContents :: !(BlobContents m) }

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

doWithTree :: Repository m => Maybe (Tree m) -> TreeT m a -> m (a, TreeOid m)
doWithTree mtr act =
    fst <$> (runStateT (runTreeT go) =<< newTreeBuilder mtr)
  where
    go = liftM2 (,) act currentTreeOid

withTree :: Repository m => Tree m -> TreeT m a -> m (a, TreeOid m)
withTree tr = doWithTree (Just tr)

withTreeOid :: Repository m => TreeOid m -> TreeT m a -> m (a, TreeOid m)
withTreeOid oid action = do
    tree <- lookupTree oid
    doWithTree (Just tree) action

mutateTree :: Repository m => Tree m -> TreeT m a -> m (TreeOid m)
mutateTree tr action = snd <$> withTree tr action

mutateTreeOid :: Repository m => TreeOid m -> TreeT m a -> m (TreeOid m)
mutateTreeOid tr action = snd <$> withTreeOid tr action

currentTreeOid :: Repository m => TreeT m (TreeOid m)
currentTreeOid = do
    tb <- getBuilder
    (tb', toid) <- lift $ writeTreeBuilder tb
    putBuilder tb'
    return toid

currentTree :: Repository m => TreeT m (Tree m)
currentTree = lift . lookupTree =<< currentTreeOid

withNewTree :: Repository m => TreeT m a -> m (a, TreeOid m)
withNewTree = doWithTree Nothing

createTree :: Repository m => TreeT m a -> m (TreeOid m)
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
    , commitParents   :: ![CommitOid m]
    , commitTree      :: !(TreeOid m)
    , commitAuthor    :: !Signature
    , commitCommitter :: !Signature
    , commitLog       :: !Text
    , commitEncoding  :: !Text
    }

commitRefTarget :: Commit m -> RefTarget m
commitRefTarget = RefObj . commitOid

referenceToOid :: Repository m
               => Maybe Text -> Maybe (Reference m)
               -> m (Maybe (CommitOid m))
referenceToOid mname mref =
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
    , tagCommit :: !(CommitOid m)
    }

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
            BlobObj b -> blobToByteString b
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

data ModifiedBuilder m = ModifiedBuilder (TreeBuilder m)
                       | BuilderUnchanged (TreeBuilder m)

instance Monoid (ModifiedBuilder m) where
    mempty = BuilderUnchanged (error "ModifiedBuilder is a semigroup")
    BuilderUnchanged _ `mappend` BuilderUnchanged b2 = BuilderUnchanged b2
    ModifiedBuilder b1  `mappend` BuilderUnchanged _ = ModifiedBuilder b1
    BuilderUnchanged _ `mappend` ModifiedBuilder b2  = ModifiedBuilder b2
    ModifiedBuilder _  `mappend` ModifiedBuilder b2  = ModifiedBuilder b2

fromBuilderMod :: ModifiedBuilder m -> TreeBuilder m
fromBuilderMod (BuilderUnchanged tb) = tb
fromBuilderMod (ModifiedBuilder tb)  = tb

data TreeBuilder m = TreeBuilder
    { mtbBaseTreeOid    :: Maybe (TreeOid m)
    , mtbPendingUpdates :: HashMap Text (TreeBuilder m)
    , mtbNewBuilder     :: Maybe (Tree m) -> m (TreeBuilder m)
    , mtbWriteContents  :: TreeBuilder m -> m (ModifiedBuilder m, TreeOid m)
    , mtbLookupEntry    :: Text -> m (Maybe (TreeEntry m))
    , mtbEntryCount     :: m Int
    , mtbPutEntry       :: TreeBuilder m -> Text -> TreeEntry m
                        -> m (ModifiedBuilder m)
    , mtbDropEntry      :: TreeBuilder m -> Text -> m (ModifiedBuilder m)
    }

instance Monad m => Monoid (TreeBuilder m) where
    mempty = TreeBuilder
        { mtbBaseTreeOid    = Nothing
        , mtbPendingUpdates = HashMap.empty
        , mtbNewBuilder     = error "Implement TreeBuilder.mtbNewBuilder"
        , mtbWriteContents  = error "Implement TreeBuilder.mtbWriteContents"
        , mtbLookupEntry    = \_ -> return Nothing
        , mtbEntryCount     = return 0
        , mtbPutEntry       = \tb _ _ -> return (BuilderUnchanged tb)
        , mtbDropEntry      = \tb _ -> return (BuilderUnchanged tb)
        }
    tb1 `mappend` tb2 = tb2
        { mtbBaseTreeOid    = mtbBaseTreeOid tb1
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
queryTreeBuilder builder path kind f = do
    (mtb, mtresult) <- walk (BuilderUnchanged builder) (splitPath path)
    return (fromBuilderMod mtb, fromModifyTreeResult mtresult)
  where
    walk _ [] = error "queryTreeBuilder called without a path"
    walk bm (name:names) = do
        let tb = fromBuilderMod bm
        y <- case HashMap.lookup name (mtbPendingUpdates tb) of
            Just x  -> return $ Left (BuilderUnchanged x)
            Nothing -> do
                mentry <- mtbLookupEntry tb name
                case mentry of
                    Nothing
                        | kind == PutEntry && not (null names) ->
                            Left . ModifiedBuilder
                                <$> mtbNewBuilder tb Nothing
                        | otherwise -> return $ Right Nothing
                    Just x -> return $ Right (Just x)
        update bm name names y

    doUpdate GetEntry bm name sbm = do
        (_, tref) <- writeTreeBuilder (fromBuilderMod sbm)
        returnTree bm name $ f (Just (TreeEntry tref))
    doUpdate _ bm name _ = returnTree bm name (f Nothing)

    update bm name [] (Left sbm) = doUpdate kind bm name sbm
    update bm name [] (Right y)  = returnTree bm name (f y)

    update bm _ _ (Right Nothing) = return (bm, TreeEntryNotFound)
    update _ _ _ (Right (Just BlobEntry {})) =
        failure TreeCannotTraverseBlob
    update _ _ _ (Right (Just CommitEntry {})) =
        failure TreeCannotTraverseCommit

    update bm name names arg = do
        sbm <- case arg of
            Left sbm' -> return sbm'
            Right (Just (TreeEntry st')) -> do
                tree <- lookupTree st'
                ModifiedBuilder
                    <$> mtbNewBuilder (fromBuilderMod bm) (Just tree)
            _ -> error "queryTreeBuilder encountered the impossible"

        (sbm', z) <- walk sbm names
        let bm' = bm <> postUpdate bm sbm' name
        return $ bm' `seq` (bm', z)

    returnTree bm@(fromBuilderMod -> tb) n z = do
        bm' <- case z of
            TreeEntryNotFound     -> return bm
            TreeEntryPersistent _ -> return bm
            TreeEntryDeleted      -> do
                bm' <- mtbDropEntry tb tb n
                let tb'   = fromBuilderMod bm'
                    upds' = mtbPendingUpdates tb'
                return $ case bm' of
                    ModifiedBuilder _ ->
                        ModifiedBuilder tb'
                            { mtbPendingUpdates = HashMap.delete n upds' }
                    BuilderUnchanged _ ->
                        if HashMap.member n upds'
                        then ModifiedBuilder tb'
                            { mtbPendingUpdates = HashMap.delete n upds' }
                        else bm'
            TreeEntryMutated z'   -> mtbPutEntry tb tb n z'
        let bm'' = bm <> bm'
        return $ bm'' `seq` (bm'', z)

    postUpdate bm (BuilderUnchanged _) _ = bm
    postUpdate (fromBuilderMod -> tb) (ModifiedBuilder sbm) name =
        ModifiedBuilder $ tb
            { mtbPendingUpdates =
                   HashMap.insert name sbm (mtbPendingUpdates tb) }

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTreeBuilder :: Repository m
                 => TreeBuilder m
                 -> m (TreeBuilder m, TreeOid m)
writeTreeBuilder builder = do
    (bm, mtref) <- go (BuilderUnchanged builder)
    tref <- case mtref of
        Nothing   -> parseObjOid emptyTreeId
        Just tref -> return tref
    return (fromBuilderMod bm, tref)
  where
    go bm = do
        let upds = mtbPendingUpdates (fromBuilderMod bm)
        bm' <- if HashMap.size upds == 0
               then return bm
               else do
                   bm' <- foldM update bm $ HashMap.toList upds
                   return $ ModifiedBuilder (fromBuilderMod bm')
                       { mtbPendingUpdates = HashMap.empty }
        let tb' = fromBuilderMod bm'
        cnt <- mtbEntryCount tb'
        if cnt == 0
            then return (bm', Nothing)
            else do
                 (bm'', tref) <- mtbWriteContents tb' tb'
                 return (bm' <> bm'', Just tref)

    update bm (k,v) = do
        let tb = fromBuilderMod bm
        -- The intermediate TreeBuilder will be dropped after this fold is
        -- completed, by setting mtbPendingUpdates to HashMap.empty, above.
        (_,mtref) <- go (BuilderUnchanged v)
        bm' <- case mtref of
            Nothing   -> mtbDropEntry tb tb k
            Just tref -> mtbPutEntry tb tb k (TreeEntry tref)
        return $ bm <> bm'

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
    flip getTreeEntry path =<< lookupTree (commitTree c)

copyBlob :: (Repository m, Repository (t m), MonadTrans t)
         => BlobOid m
         -> HashSet Text
         -> t m (BlobOid (t m), HashSet Text)
copyBlob blobr needed = do
    let oid = untag blobr
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        bs <- lift $ blobToByteString =<< lookupBlob (Tagged oid)
        boid <- createBlob (BlobString bs)

        let x = HashSet.delete sha needed
        return $ boid `seq` x `seq` (boid, x)

        else return (Tagged oid2, needed)

copyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
              => TreeEntry m
              -> HashSet Text
              -> t m (TreeEntry (t m), HashSet Text)
copyTreeEntry (BlobEntry oid kind) needed = do
    (b,needed') <- copyBlob oid needed
    return (BlobEntry b kind, needed')
copyTreeEntry (CommitEntry oid) needed = do
    coid <- parseOid (renderObjOid oid)
    return (CommitEntry (Tagged coid), needed)
copyTreeEntry (TreeEntry _) _ = error "This should never be called"

copyTree :: (Repository m, Repository (t m), MonadTrans t)
         => TreeOid m
         -> HashSet Text
         -> t m (TreeOid (t m), HashSet Text)
copyTree tr needed = do
    let oid = untag tr
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        tree            <- lift $ lookupTree tr
        entries         <- lift $ traverseEntries (curry return) tree
        (needed', tref) <- withNewTree $ foldM doCopyTreeEntry needed entries

        let x = HashSet.delete sha needed'
        return $ tref `seq` x `seq` (tref, x)

        else return (Tagged oid2, needed)
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
           => CommitOid m
           -> Maybe Text
           -> HashSet Text
           -> t m (CommitOid (t m), HashSet Text)
copyCommit cr mref needed = do
    let oid = untag cr
        sha = renderOid oid
    commit <- lift $ lookupCommit cr
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

        let coid = commitOid commit'
            x    = HashSet.delete sha needed''
        return $ coid `seq` x `seq` (coid, x)

        else return (Tagged oid2, needed)
  where
    copyParent (prefs,needed') cref = do
        (cref2,needed'') <- copyCommit cref Nothing needed'
        let x = cref2 `seq` (cref2:prefs)
        return $ x `seq` needed'' `seq` (x,needed'')

-- | Given a list of objects (commit and top-level trees) return by
--   'missingObjects', expand it to include all subtrees and blobs as well.
--   Ordering is preserved.
allMissingObjects :: Repository m => [ObjectOid m] -> m [ObjectOid m]
allMissingObjects objs =
    fmap concat . forM objs $ \obj -> case obj of
        TreeObjOid toid -> do
            tr <- lookupTree toid
            subobjss <- flip traverseEntries tr $ \_ ent ->
                return $ case ent of
                    BlobEntry oid _ -> [BlobObjOid oid]
                    TreeEntry oid   -> [TreeObjOid oid]
                    _ -> []
            return (obj:concat subobjss)
        _ -> return [obj]

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works.
genericPushCommit :: (Repository m, Repository (t m), MonadTrans t)
                  => CommitOid m -> Text -> t m (CommitOid (t m))
genericPushCommit coid remoteRefName = do
    mrref    <- lookupReference remoteRefName
    commits1 <- lift $ traverseCommits crefToSha coid
    fastForward <- case mrref of
        Just rref -> do
            mrsha <- referenceSha rref
            case mrsha of
                Nothing -> failure (PushNotFastForward $
                                    "Could not find SHA for " <> remoteRefName)
                Just rsha
                    | rsha `elem` commits1 -> do
                        roid <- lift $ parseOid rsha
                        return $ Just (Just (Tagged roid))
                    | otherwise -> do
                        failure (PushNotFastForward $
                                 "SHA " <> rsha
                                        <> " not found in remote")
        Nothing -> return (Just Nothing)
    case fastForward of
        Nothing -> failure (PushNotFastForward "unexpected")
        Just rref -> do
            objs <- lift $ allMissingObjects =<< missingObjects rref coid
            shas <- mapM (return . renderOid . untagObjOid) objs
            (cref,_) <- copyCommit coid Nothing (HashSet.fromList shas)
            -- jww (2013-04-18): This is something the user must
            -- decide to do
            -- updateReference_ remoteRefName (RefObj cref)
            return cref
  where
    referenceSha ref = fmap renderObjOid <$> referenceToOid Nothing (Just ref)
    crefToSha cref   = return $ renderObjOid cref

commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
commitHistoryFirstParent c =
    case commitParents c of
        []    -> return [c]
        (p:_) -> do ps <- commitHistoryFirstParent =<< lookupCommit p
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
            (p:_) -> go =<< lookupCommit p
        return $ maybe rest (:rest) entry

    getCommitTreeEntry co = do
        ce <- commitTreeEntry co path
        case ce of
            Nothing  -> return Nothing
            Just ce' -> Just <$> identifyEntry co ce'

getCommitParents :: Repository m => Commit m -> m [Commit m]
getCommitParents = traverse lookupCommit . commitParents

resolveReferenceTree :: Repository m => Text -> m (Maybe (Tree m))
resolveReferenceTree refName = do
    c <- resolveReference refName
    case c of
        Nothing -> return Nothing
        Just c' ->
            Just <$> (lookupCommit c' >>= lookupTree . commitTree)

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
