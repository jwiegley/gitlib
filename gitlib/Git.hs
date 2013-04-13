{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}

-- | Interface for working with Git repositories.
module Git where

import           Control.Applicative
import qualified Control.Exception.Lifted as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)

{- $repositories -}
data RepositoryFacts = RepositoryFacts
    { hasSymbolicReferences :: !Bool
    } deriving Show

type MonadGit m = (Failure Git.GitException m, MonadIO m, Applicative m)

-- | 'Repository' is the central point of contact between user code and
-- Git data objects.  Every object must belong to some repository.
class (Applicative m, Monad m, Failure GitException m,
       Eq (Oid m), Ord (Oid m), Show (Oid m)) => Repository m where
    data Oid m
    data TreeData m
    data Options m

    facts :: m RepositoryFacts

    parseOid  :: Text -> m (Oid m)
    renderOid :: Oid m -> Text
    renderOid = renderObjOid . Tagged
    renderObjOid :: Tagged a (Oid m) -> Text
    renderObjOid = renderOid . unTagged

    -- References
    createRef  :: Text -> RefTarget m (Commit m) -> m (Reference m (Commit m))
    createRef_ :: Text -> RefTarget m (Commit m) -> m ()
    createRef_ = (void .) . createRef
    lookupRef  :: Text -> m (Maybe (Reference m (Commit m)))
    updateRef  :: Text -> RefTarget m (Commit m) -> m (Reference m (Commit m))
    updateRef_ :: Text -> RefTarget m (Commit m) -> m ()
    updateRef_ = (void .) . updateRef
    deleteRef  :: Text -> m ()

    allRefs :: m [Reference m (Commit m)]
    allRefs = catMaybes <$> (mapM lookupRef =<< allRefNames)

    allRefNames :: m [Text]
    allRefNames = map refName <$> allRefs

    resolveRef :: Text -> m (Maybe (CommitRef m))
    resolveRef name = lookupRef name >>= referenceToRef (Just name)

    -- Lookup
    lookupCommit :: CommitOid m -> m (Commit m)
    lookupTree   :: TreeOid m -> m (Tree m)
    lookupBlob   :: BlobOid m -> m (Blob m)
    lookupTag    :: TagOid m -> m (Tag m)

    lookupObject :: Text -> m (Object m)
    existsObject :: Oid m -> m Bool

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
                   -> m [Oid m]            -- ^ All the objects in between

    -- Object creation
    newTree :: m (Tree m)
    hashContents :: BlobContents m -> m (BlobOid m)
    createBlob :: BlobContents m -> m (BlobOid m)
    createCommit :: [CommitRef m] -> TreeRef m
                 -> Signature -> Signature -> Text -> Maybe Text -> m (Commit m)
    createTag :: CommitOid m -> Signature -> Text -> Text -> m (Tag m)

    deleteRepository :: m ()

{- $exceptions -}
-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data GitException = BackendError Text
                  | RepositoryNotExist
                  | RepositoryInvalid
                  | BlobCreateFailed
                  | BlobEmptyCreateFailed
                  | BlobEncodingUnknown Text
                  | BlobLookupFailed
                  | PushNotFastForward Text
                  | TranslationException Text
                  | TreeCreateFailed Text
                  | TreeBuilderCreateFailed
                  | TreeBuilderInsertFailed
                  | TreeBuilderRemoveFailed
                  | TreeBuilderWriteFailed
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
    { refName   :: !Text
    , refTarget :: !(RefTarget m a) }

data CommitName m = CommitObjectId !(Tagged (Commit m) (Oid m))
                  | CommitRefName !Text
                  | CommitReference !(Reference m (Commit m))

instance Repository m => Show (CommitName m) where
    show (CommitObjectId coid) = T.unpack (renderObjOid coid)
    show (CommitRefName name)  = show name
    show (CommitReference ref) = show (refName ref)

nameOfCommit :: Commit m -> CommitName m
nameOfCommit = CommitObjectId . commitOid

commitNameToRef :: Repository m => CommitName m -> m (Maybe (CommitRef m))
commitNameToRef (CommitObjectId coid) = return (Just (ByOid coid))
commitNameToRef (CommitRefName name)  = resolveRef name
commitNameToRef (CommitReference ref) = referenceToRef Nothing (Just ref)

renderCommitName :: Repository m => CommitName m -> Text
renderCommitName (CommitObjectId coid) = renderObjOid coid
renderCommitName (CommitRefName name)  = name
renderCommitName (CommitReference ref) = refName ref

copyOid :: (Repository m, MonadGit m, Repository n, MonadGit n)
        => Oid m -> n (Oid n)
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
    fmap CommitReference <$> lookupRef (refName ref)

{- $objects -}
data ObjRef m a = ByOid !(Tagged a (Oid m)) | Known !a

type BlobRef m   = ObjRef m (Blob m)
type TreeRef m   = ObjRef m (Tree m)
type CommitRef m = ObjRef m (Commit m)
type TagRef m    = ObjRef m (Tag m)

data Object m = BlobObj   !(BlobRef m)
              | TreeObj   !(TreeRef m)
              | CommitObj !(CommitRef m)
              | TagObj    !(TagRef m)

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
data TreeEntry m = BlobEntry   { blobEntryOid   :: !(BlobOid m)
                               , blobEntryKind  :: !BlobKind }
                 | TreeEntry   { treeEntryRef   :: !(TreeRef m) }
                 | CommitEntry { commitEntryRef :: !(CommitOid m) }

blobEntry :: Repository m => BlobOid m -> BlobKind -> TreeEntry m
blobEntry = BlobEntry

treeEntry :: Repository m => Tree m -> TreeEntry m
treeEntry = TreeEntry . treeRef

commitEntry :: Repository m => Commit m -> TreeEntry m
commitEntry = CommitEntry . commitOid

-- | A 'Tree' is anything that is "treeish".
--
-- Minimal complete definition: 'modifyTree'.  Note that for some treeish
-- things, like Tags, it should always be an error to attempt to modify the
-- tree in any way.
data Tree m = Tree
    { modifyTree :: FilePath    -- path within the tree
                 -> Bool        -- create subtree's leading up to path?
                 -> (Maybe (TreeEntry m) -> m (Maybe (TreeEntry m)))
                 -> m (Maybe (TreeEntry m))

    , lookupEntry      :: FilePath -> m (Maybe (TreeEntry m))
    , putTreeEntry     :: FilePath -> TreeEntry m -> m ()
    , putBlob'         :: FilePath -> BlobOid m -> BlobKind -> m ()
    , putBlob          :: FilePath -> BlobOid m -> m ()
    , putTree          :: FilePath -> TreeRef m -> m ()
    , putCommit        :: FilePath -> CommitOid m -> m ()
    , dropFromTree     :: FilePath -> m ()
    , writeTree        :: m (TreeOid m)
    , traverseEntries  :: forall a. (FilePath -> TreeEntry m -> m a) -> m [a]
    , traverseEntries_ :: (FilePath -> TreeEntry m -> m ()) -> m ()
    , getTreeData      :: !(TreeData m)
    }

mkTree :: Repository m
       => (Tree m
           -> FilePath           -- path within the tree
           -> Bool               -- create subtree's leading up to path?
           -> (Maybe (TreeEntry m) -> m (Maybe (TreeEntry m)))
           -> m (Maybe (TreeEntry m)))
       -> (Tree m -> m (TreeOid m))
       -> (forall a. Tree m -> (FilePath -> TreeEntry m -> m a) -> m [a])
       -> TreeData m
       -> Tree m
mkTree modifyTree' writeTree' traverseEntries' treeData = tr
  where
    tr = Tree
        { modifyTree       = modifyTree' tr
        , lookupEntry      = \path -> modifyTree' tr path False return
        , putTreeEntry     = \path ent ->
                                 void $ modifyTree' tr path True
                                     (const (return (Just ent)))
        , putBlob'         = \path b kind ->
                                 putTreeEntry tr path (BlobEntry b kind)
        , putBlob          = \path b -> putBlob' tr path b PlainBlob
        , putTree          = \path tr' -> putTreeEntry tr path (TreeEntry tr')
        , putCommit        = \path c -> putTreeEntry tr path (CommitEntry c)
        , dropFromTree     = \path ->
                                 void $ modifyTree' tr path False
                                     (const (return Nothing))
        , writeTree        = writeTree' tr
        , traverseEntries  = traverseEntries' tr
        , traverseEntries_ = void . traverseEntries' tr
        , getTreeData      = treeData
        }

treeRef :: Tree m -> TreeRef m
treeRef = Known

treeRefOid :: Repository m => TreeRef m -> m (TreeOid m)
treeRefOid (ByOid x) = return x
treeRefOid (Known x) = writeTree x

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
        Just ref@(Reference { refTarget = RefObj x }) ->
            return (Just x)
        Just ref@(Reference { refTarget = RefSymbolic name' }) ->
            if fromMaybe name' mname /= name'
            then resolveRef name'
            else failure (ReferenceLookupFailed (refName ref))

{- $tags -}

data Tag m = Tag { tagCommit :: !(CommitRef m) }

{- $merges -}

data MergeConflict m
    = NoConflict
    | MergeConflict
        { conflictedHead      :: CommitOid m
        , conflictedMergeHead :: CommitOid m
        , conflictedFiles     :: Map FilePath (Either ByteString ByteString)
        }

sameMergeBasis :: Repository m => MergeConflict m -> MergeConflict m -> Bool
sameMergeBasis NoConflict NoConflict = True
sameMergeBasis NoConflict _          = False
sameMergeBasis _ NoConflict          = False
sameMergeBasis x y =
      conflictedHead x      == conflictedHead y
    && conflictedMergeHead x == conflictedMergeHead y
    &&    Map.keys (conflictedFiles x)
      == Map.keys (conflictedFiles y)

instance Repository m => Show (MergeConflict m) where
    show NoConflict = "NoConflict"

    show (MergeConflict h mh fs) =
        "MergeConflict { conflictedHead = " ++ show h
        ++ ", conflictedMergeHead = " ++ show mh
        ++ ", conflictedFiles = " ++ show fs
        ++ " }"

copyConflict :: (Repository m, MonadGit m, Repository n, MonadGit n)
             => MergeConflict m -> n (MergeConflict n)
copyConflict NoConflict = return NoConflict
copyConflict (MergeConflict h mh files) =
    MergeConflict <$> (Tagged <$> parseOid (renderObjOid h))
                  <*> (Tagged <$> parseOid (renderObjOid mh))
                  <*> pure files

{- $miscellaneous -}

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
    }

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
withRepository factory path action =
    withRepository' factory
        ((defaultOptions factory) { repoPath = path }) action

-- Git.hs
