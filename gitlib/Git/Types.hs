{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ > 707
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

module Git.Types where

import           Conduit
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.Fail (MonadFail)
import           Control.Monad.Trans.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as BL
import           Data.HashMap.Strict (HashMap)
import           Data.Hashable
import           Data.Map (Map)
import           Data.Semigroup
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Typeable

type RawFilePath = ByteString

data RepositoryFacts = RepositoryFacts
    { hasSymbolicReferences :: !Bool
    } deriving Show

type RefName       = Text
type CommitAuthor  = Text
type CommitEmail   = Text
type CommitMessage = Text
type TreeFilePath  = RawFilePath

-- | 'Repository' is the central point of contact between user code and Git
--   data objects.  Every object must belong to some repository.
class (Applicative m, Monad m, MonadThrow m,
       IsOid (Oid r), Show (Oid r), Eq (Oid r), Ord (Oid r))
      => MonadGit r m | m -> r where
    type Oid r :: *
    data Tree r :: *
    data Options r :: *

    facts :: m RepositoryFacts
    parseOid :: Text -> m (Oid r)

    getRepository :: m r
    closeRepository :: m ()
    deleteRepository :: m ()

    -- References
    createReference :: RefName -> RefTarget r -> m ()
    lookupReference :: RefName -> m (Maybe (RefTarget r))
    updateReference :: RefName -> RefTarget r -> m ()
    deleteReference :: RefName -> m ()
    sourceReferences :: ConduitT i RefName m ()

    -- Object lookup
    lookupObject  :: Oid r -> m (Object r m)
    existsObject  :: Oid r -> m Bool
    sourceObjects :: Maybe (CommitOid r)    -- ^ A commit we may already have
                  -> CommitOid r            -- ^ The commit we need
                  -> Bool                   -- ^ Include commit trees also?
                  -> ConduitT i (ObjectOid r) m () -- ^ All the objects in between

    lookupCommit  :: CommitOid r -> m (Commit r)
    lookupTree    :: TreeOid r -> m (Tree r)
    lookupBlob    :: BlobOid r -> m (Blob r m)
    lookupTag     :: TagOid r -> m (Tag r)

    readIndex :: TreeT r m ()
    writeIndex :: TreeT r m ()

    -- Working with trees
    newTreeBuilder :: Maybe (Tree r) -> m (TreeBuilder r m)

    treeOid   :: Tree r -> m (TreeOid r)
    treeEntry :: Tree r -> TreeFilePath -> m (Maybe (TreeEntry r))
    sourceTreeEntries :: Tree r -> ConduitT i (TreeFilePath, TreeEntry r) m ()

    diffContentsWithTree :: ConduitT () (Either TreeFilePath ByteString) m ()
                         -> Tree r -> ConduitT i ByteString m ()

    -- Creating other objects
    hashContents :: BlobContents m -> m (BlobOid r)
    createBlob   :: BlobContents m -> m (BlobOid r)
    createCommit :: [CommitOid r] -> TreeOid r
                 -> Signature -> Signature -> CommitMessage -> Maybe RefName
                 -> m (Commit r)
    createTag :: CommitOid r -> Signature -> CommitMessage -> Text -> m (Tag r)

data RepositoryOptions = RepositoryOptions
    { repoPath       :: !FilePath
    , repoWorkingDir :: !(Maybe FilePath)
    , repoIsBare     :: !Bool
    , repoAutoCreate :: !Bool
    }

defaultRepositoryOptions :: RepositoryOptions
defaultRepositoryOptions = RepositoryOptions "" Nothing False False

data RepositoryFactory n m r = RepositoryFactory
    { openRepository :: RepositoryOptions -> m r
    , runRepository  :: forall a. r -> n a -> m a
    }

{- $oids -}
class IsOid o where
    renderOid :: o -> Text
    renderOid = renderObjOid . Tagged

    renderObjOid :: Tagged a o -> Text
    renderObjOid = renderOid . untag

type BlobOid r   = Tagged r (Oid r)
type TreeOid r   = Tagged (Tree r) (Oid r)
type CommitOid r = Tagged (Commit r) (Oid r)
type TagOid r    = Tagged (Tag r) (Oid r)

data ObjectOid r = BlobObjOid   !(BlobOid r)
                 | TreeObjOid   !(TreeOid r)
                 | CommitObjOid !(CommitOid r)
                 | TagObjOid    !(TagOid r)

parseObjOid :: MonadGit r m => forall o. Text -> m (Tagged o (Oid r))
parseObjOid sha = Tagged <$> parseOid sha

copyOid :: (MonadGit r m, MonadGit s n) => Oid r -> n (Oid s)
copyOid = parseOid . renderOid

newtype SHA = SHA { getSHA :: ByteString } deriving (Eq, Ord, Read)

shaToText :: SHA -> Text
shaToText (SHA bs) = T.decodeUtf8 (B16.encode bs)

textToSha :: MonadFail m => Text -> m SHA
textToSha t =
    case B16.decode $ T.encodeUtf8 t of
        (bs, "") -> return (SHA bs)
        _ -> fail "Invalid base16 encoding"

instance IsOid SHA where
    renderOid = shaToText

instance Show SHA where
    show = T.unpack . shaToText

instance Hashable SHA where
    hashWithSalt salt (SHA bs) = hashWithSalt salt bs

{- $blobs -}
data Blob r m = Blob
    { blobOid      :: !(BlobOid r)
    , blobContents :: !(BlobContents m)
    }

type ByteSource m = ConduitT () ByteString m ()

data BlobContents m = BlobString !ByteString
                    | BlobStringLazy !BL.ByteString
                    | BlobStream !(ByteSource m)
                    | BlobSizedStream !(ByteSource m) !Int

data BlobKind = PlainBlob | ExecutableBlob | SymlinkBlob
              deriving (Show, Eq, Enum)

instance Eq (BlobContents m) where
  BlobString str1 == BlobString str2 = str1 == str2
  _ == _ = False

{- $trees -}
newtype TreeT r m a = TreeT { runTreeT :: StateT (TreeBuilder r m) m a }

data TreeEntry r = BlobEntry   { blobEntryOid   :: !(BlobOid r)
                               , blobEntryKind  :: !BlobKind }
                 | TreeEntry   { treeEntryOid   :: !(TreeOid r) }
                 | CommitEntry { commitEntryOid :: !(CommitOid r) }

-- instance Show (TreeEntry r) where
--     show (BlobEntry oid _) = "<BlobEntry " ++ show oid ++ ">"
--     show (TreeEntry oid)   = "<TreeEntry " ++ show oid ++ ">"
--     show (CommitEntry oid) = "<CommitEntry " ++ show oid ++ ">"

treeEntryToOid :: TreeEntry r -> Oid r
treeEntryToOid (BlobEntry boid _) = untag boid
treeEntryToOid (TreeEntry toid)   = untag toid
treeEntryToOid (CommitEntry coid) = untag coid

data TreeBuilder r m = TreeBuilder
    { mtbBaseTreeOid    :: Maybe (TreeOid r)
    , mtbPendingUpdates :: HashMap TreeFilePath (TreeBuilder r m)
    , mtbNewBuilder     :: Maybe (Tree r) -> m (TreeBuilder r m)
    , mtbWriteContents  :: TreeBuilder r m -> m (ModifiedBuilder r m, TreeOid r)
    , mtbLookupEntry    :: TreeFilePath -> m (Maybe (TreeEntry r))
    , mtbEntryCount     :: m Int
    , mtbPutEntry       :: TreeBuilder r m -> TreeFilePath -> TreeEntry r
                        -> m (ModifiedBuilder r m)
    , mtbDropEntry      :: TreeBuilder r m -> TreeFilePath
                        -> m (ModifiedBuilder r m)
    }

data ModifiedBuilder r m = ModifiedBuilder (TreeBuilder r m)
                         | BuilderUnchanged (TreeBuilder r m)

instance Semigroup (ModifiedBuilder r m) where
    BuilderUnchanged _  <> BuilderUnchanged b2 = BuilderUnchanged b2
    ModifiedBuilder b1  <> BuilderUnchanged _  = ModifiedBuilder b1
    BuilderUnchanged _  <> ModifiedBuilder b2  = ModifiedBuilder b2
    ModifiedBuilder _   <> ModifiedBuilder b2  = ModifiedBuilder b2

instance Monoid (ModifiedBuilder r m) where
    mempty = BuilderUnchanged (error "ModifiedBuilder is a semigroup")
    x `mappend` y = x <> y

fromBuilderMod :: ModifiedBuilder r m -> TreeBuilder r m
fromBuilderMod (BuilderUnchanged tb) = tb
fromBuilderMod (ModifiedBuilder tb)  = tb

{- $commits -}
data Commit r = Commit
    { commitOid       :: !(CommitOid r)
    , commitParents   :: ![CommitOid r]
    , commitTree      :: !(TreeOid r)
    , commitAuthor    :: !Signature
    , commitCommitter :: !Signature
    , commitLog       :: !CommitMessage
    , commitEncoding  :: !Text
    }

sourceCommitParents :: MonadGit r m => Commit r -> ConduitT i (Commit r) m ()
sourceCommitParents commit =
    forM_ (commitParents commit) $ yield <=< lift . lookupCommit

lookupCommitParents :: MonadGit r m => Commit r -> m [Commit r]
lookupCommitParents commit = runConduit $ sourceCommitParents commit .| sinkList

data Signature = Signature
    { signatureName  :: !CommitAuthor
    , signatureEmail :: !CommitEmail
    , signatureWhen  :: !ZonedTime
    } deriving Show

defaultSignature :: Signature
defaultSignature = Signature
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

{- $tags -}
data Tag r = Tag
    { tagOid    :: !(TagOid r)
    , tagCommit :: !(CommitOid r)
    }

{- $objects -}
data Object r m = BlobObj   !(Blob r m)
                | TreeObj   !(Tree r)
                | CommitObj !(Commit r)
                | TagObj    !(Tag r)

objectOid :: MonadGit r m => Object r m -> m (Oid r)
objectOid (BlobObj obj)   = return $ untag (blobOid obj)
objectOid (TreeObj obj)   = untag <$> treeOid obj
objectOid (CommitObj obj) = return $ untag (commitOid obj)
objectOid (TagObj obj)    = return $ untag (tagOid obj)

loadObject :: MonadGit r m => ObjectOid r -> m (Object r m)
loadObject (BlobObjOid oid)   = BlobObj   <$> lookupBlob oid
loadObject (TreeObjOid oid)   = TreeObj   <$> lookupTree oid
loadObject (CommitObjOid oid) = CommitObj <$> lookupCommit oid
loadObject (TagObjOid oid)    = TagObj    <$> lookupTag oid

objectToObjOid :: MonadGit r m => Object r m -> m (ObjectOid r)
objectToObjOid (BlobObj obj)   = return $ BlobObjOid (blobOid obj)
objectToObjOid (TreeObj obj)   = TreeObjOid <$> treeOid obj
objectToObjOid (CommitObj obj) = return $ CommitObjOid (commitOid obj)
objectToObjOid (TagObj obj)    = return $ TagObjOid (tagOid obj)

untagObjOid :: ObjectOid r -> Oid r
untagObjOid (BlobObjOid oid)   = untag oid
untagObjOid (TreeObjOid oid)   = untag oid
untagObjOid (CommitObjOid oid) = untag oid
untagObjOid (TagObjOid oid)    = untag oid

{- $references -}
data RefTarget (r :: *) = RefObj !(Oid r) | RefSymbolic !RefName

-- instance Show (RefTarget r) where
--     show (RefObj oid) = "RefObj#" ++ T.unpack (renderOid oid)
--     show (RefSymbolic name) = "RefSymbolic#" ++ T.unpack name

commitRefTarget :: Commit r -> RefTarget r
commitRefTarget = RefObj . untag . commitOid

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

data MergeResult r
    = MergeSuccess
        { mergeCommit    :: CommitOid r
        }
    | MergeConflicted
        { mergeCommit    :: CommitOid r
        , mergeHeadLeft  :: CommitOid r
        , mergeHeadRight :: CommitOid r
        , mergeConflicts ::
            Map TreeFilePath (ModificationKind, ModificationKind)
        }

copyMergeResult :: (MonadGit r m, IsOid (Oid s))
                => MergeResult s -> m (MergeResult r)
copyMergeResult (MergeSuccess mc) =
    MergeSuccess <$> parseObjOid (renderObjOid mc)
copyMergeResult (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> parseObjOid (renderObjOid hl)
                    <*> parseObjOid (renderObjOid hr)
                    <*> parseObjOid (renderObjOid mc)
                    <*> pure cs

-- instance Show (MergeResult r) where
--     show (MergeSuccess mc) =
--         "MergeSuccess (" ++ T.unpack (renderObjOid mc) ++ ")"
--     show (MergeConflicted mc hl hr cs) =
--         "MergeResult"
--      ++ "\n    { mergeCommit    = " ++ T.unpack (renderObjOid mc)
--      ++ "\n    , mergeHeadLeft  = " ++ T.unpack (renderObjOid hl)
--      ++ "\n    , mergeHeadRight = " ++ T.unpack (renderObjOid hr)
--      ++ "\n    , mergeConflicts = " ++ show cs
--      ++ "\n    }"

{- $exceptions -}
-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data GitException
    = BackendError Text
    | GitError Text
    | RepositoryNotExist
    | RepositoryInvalid
    | RepositoryCannotAccess Text
    | BlobCreateFailed Text
    | BlobEmptyCreateFailed
    | BlobEncodingUnknown Text
    | BlobLookupFailed
    | DiffBlobFailed Text
    | DiffPrintToPatchFailed Text
    | DiffTreeToIndexFailed Text
    | IndexAddFailed TreeFilePath Text
    | IndexCreateFailed Text
    | PathEncodingError Text
    | PushNotFastForward Text
    | TagLookupFailed Text
    | TranslationException Text
    | TreeCreateFailed Text
    | TreeBuilderCreateFailed
    | TreeBuilderInsertFailed TreeFilePath
    | TreeBuilderRemoveFailed TreeFilePath
    | TreeBuilderWriteFailed Text
    | TreeLookupFailed
    | TreeCannotTraverseBlob
    | TreeCannotTraverseCommit
    | TreeEntryLookupFailed TreeFilePath
    | TreeUpdateFailed
    | TreeWalkFailed Text
    | TreeEmptyCreateFailed
    | CommitCreateFailed
    | CommitLookupFailed Text
    | ReferenceCreateFailed RefName
    | ReferenceDeleteFailed RefName
    | RefCannotCreateFromPartialOid
    | ReferenceListingFailed Text
    | ReferenceLookupFailed RefName
    | ObjectLookupFailed Text Int
    | ObjectRefRequiresFullOid
    | OidCopyFailed
    | OidParseFailed Text
    | QuotaHardLimitExceeded Int Int
    deriving (Eq, Show, Typeable)

-- jww (2013-02-11): Create a BackendException data constructor of forall
-- e. Exception e => BackendException e, so that each can throw a derived
-- exception.
instance Exception GitException
