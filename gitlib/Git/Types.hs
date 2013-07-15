module Git.Types where

import           Control.Applicative
import qualified Control.Exception.Lifted as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.Binary as Bin
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as B16
import           Data.Conduit
import           Data.Default
import           Data.Function
import           Data.HashMap.Strict (HashMap)
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
import           Data.Typeable
import           Filesystem.Path.CurrentOS hiding (null, concat)
import           Prelude hiding (FilePath)

data RepositoryFacts = RepositoryFacts
    { hasSymbolicReferences :: !Bool
    } deriving Show

type MonadGit m = (Failure GitException m, Applicative m,
                   MonadIO m, MonadBaseControl IO m)

-- | 'Repository' is the central point of contact between user code and Git
--   data objects.  Every object must belong to some repository.
class (Applicative m, Monad m, Failure GitException m, IsOid (Oid m))
      => Repository m where
    type Oid m :: *
    data Tree m :: *
    data Options m :: *

    facts :: m RepositoryFacts
    parseOid :: Text -> m (Oid m)
    deleteRepository :: m ()

    -- References
    createReference :: Text -> RefTarget m -> m ()
    lookupReference :: Text -> m (Maybe (RefTarget m))
    updateReference :: Text -> RefTarget m -> m ()
    deleteReference :: Text -> m ()
    listReferences  :: m [Text]

    -- Object lookup
    lookupCommit  :: CommitOid m -> m (Commit m)
    lookupTree    :: TreeOid m   -> m (Tree m)
    lookupBlob    :: BlobOid m   -> m (Blob m)
    lookupTag     :: TagOid m    -> m (Tag m)
    lookupObject  :: Oid m       -> m (Object m)
    existsObject  :: Oid m       -> m Bool
    sourceObjects :: Maybe (CommitOid m)    -- ^ A commit we may already have
                  -> CommitOid m            -- ^ The commit we need
                  -> Bool                   -- ^ Include commit trees also?
                  -> Source m (ObjectOid m) -- ^ All the objects in between

    -- Working with trees
    newTreeBuilder :: Maybe (Tree m) -> m (TreeBuilder m)

    treeOid   :: Tree m -> TreeOid m
    treeEntry :: Tree m -> Text -> m (Maybe (TreeEntry m))
    listTreeEntries :: Tree m -> m [(Text, TreeEntry m)]

    -- Creating other objects
    hashContents :: BlobContents m -> m (BlobOid m)
    createBlob   :: BlobContents m -> m (BlobOid m)
    createCommit :: [CommitOid m] -> TreeOid m
                 -> Signature -> Signature -> Text -> Maybe Text -> m (Commit m)
    createTag :: CommitOid m -> Signature -> Text -> Text -> m (Tag m)

    -- -- Pack files
    -- buildPackFile :: FilePath -> [Either (CommitOid m) (TreeOid m)]
    --               -> m FilePath
    -- buildPackFile _ _ =
    --     failure (BackendError "Backend does not support building pack files")

    -- buildPackIndex :: FilePath -> ByteString -> m (Text, FilePath, FilePath)
    -- buildPackIndex _ _ =
    --     failure (BackendError "Backend does not support building pack indexes")

    -- writePackFile :: FilePath -> m ()
    -- writePackFile _ =
    --     failure (BackendError "Backend does not support writing  pack files")

    -- -- Git remotes
    -- remoteFetch :: Text {- URI -} -> Text {- fetch spec -} -> m ()

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

{- $oids -}
class (Eq o, Ord o, Show o) => IsOid o where
    renderOid :: o -> Text
    renderOid = renderObjOid . Tagged
    renderObjOid :: Tagged a o -> Text
    renderObjOid = renderOid . untag

type BlobOid m   = Tagged (Blob m) (Oid m)
type TreeOid m   = Tagged (Tree m) (Oid m)
type CommitOid m = Tagged (Commit m) (Oid m)
type TagOid m    = Tagged (Tag m) (Oid m)

data ObjectOid m = BlobObjOid   !(BlobOid m)
                 | TreeObjOid   !(TreeOid m)
                 | CommitObjOid !(CommitOid m)
                 | TagObjOid    !(TagOid m)

parseObjOid :: Repository m => forall o. Text -> m (Tagged o (Oid m))
parseObjOid sha = Tagged <$> parseOid sha

copyOid :: (Repository m, Repository n) => Oid m -> n (Oid n)
copyOid = parseOid . renderOid

newtype SHA = SHA B.ByteString deriving (Eq, Ord, Read)

shaToText :: SHA -> Text
shaToText (SHA bs) = T.decodeUtf8 (B16.encode bs)

textToSha :: Monad m => Text -> m SHA
textToSha t =
    case B16.decode $ T.encodeUtf8 t of
        (bs, "") -> return (SHA bs)
        _ -> fail "Invalid base16 encoding"

instance IsOid SHA where
    renderOid = shaToText

instance Show SHA where
    show = T.unpack . shaToText

instance Bin.Binary SHA where
    put (SHA t) = Bin.put t
    get = SHA <$> Bin.get

instance Hashable SHA where
    hashWithSalt salt (SHA bs) = hashWithSalt salt bs

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
data TreeEntry m = BlobEntry   { blobEntryOid   :: !(BlobOid m)
                               , blobEntryKind  :: !BlobKind }
                 | TreeEntry   { treeEntryOid   :: !(TreeOid m) }
                 | CommitEntry { commitEntryOid :: !(CommitOid m) }

instance Repository m => Show (TreeEntry m) where
    show (BlobEntry oid _) = "<BlobEntry " ++ T.unpack (renderObjOid oid)
    show (TreeEntry oid)   = "<TreeEntry " ++ T.unpack (renderObjOid oid)
    show (CommitEntry oid) = "<CommitEntry " ++ T.unpack (renderObjOid oid)

treeEntryToOid :: Repository m => TreeEntry m -> Oid m
treeEntryToOid (BlobEntry boid _) = untag boid
treeEntryToOid (TreeEntry toid)   = untag toid
treeEntryToOid (CommitEntry coid) = untag coid

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

{- $commits -}
data Commit m = Commit
    { commitOid       :: !(CommitOid m)
    , commitParents   :: ![CommitOid m]
    , commitTree      :: !(TreeOid m)
    , commitAuthor    :: !Signature
    , commitCommitter :: !Signature
    , commitLog       :: !Text
    , commitEncoding  :: !Text
    }

lookupCommitParents :: Repository m => Commit m -> m [Commit m]
lookupCommitParents = mapM lookupCommit . commitParents

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

{- $tags -}
data Tag m = Tag
    { tagOid    :: !(TagOid m)
    , tagCommit :: !(CommitOid m)
    }

{- $objects -}
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

{- $references -}
data RefTarget m = RefObj !(CommitOid m) | RefSymbolic !Text

commitRefTarget :: Commit m -> RefTarget m
commitRefTarget = RefObj . commitOid

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
        , mergeConflicts :: Map Text (ModificationKind, ModificationKind)
        }

copyMergeResult :: (Repository m, MonadGit m, Repository n, MonadGit n)
                => MergeResult m -> n (MergeResult n)
copyMergeResult (MergeSuccess mc) =
    MergeSuccess <$> (Tagged <$> parseOid (renderObjOid mc))
copyMergeResult (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> (Tagged <$> parseOid (renderObjOid hl))
                    <*> (Tagged <$> parseOid (renderObjOid hr))
                    <*> (Tagged <$> parseOid (renderObjOid mc))
                    <*> pure cs

instance Repository m => Show (MergeResult m) where
    show (MergeSuccess mc) = "MergeSuccess (" ++ show mc ++ ")"
    show (MergeConflicted mc hl hr cs) =
        "MergeResult"
     ++ "\n    { mergeCommit    = " ++ show mc
     ++ "\n    , mergeHeadLeft  = " ++ show hl
     ++ "\n    , mergeHeadRight = " ++ show hr
     ++ "\n    , mergeConflicts = " ++ show cs
     ++ "\n    }"

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
                  | TreeEntryLookupFailed Text
                  | TreeUpdateFailed
                  | TreeWalkFailed
                  | TreeEmptyCreateFailed
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
