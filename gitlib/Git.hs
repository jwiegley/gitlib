{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

-- | Interface for working with Git repositories.
module Git where

import           Control.Applicative
import qualified Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit
import           Data.Default
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Typeable
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)

{- $repositories -}
-- | 'RepositoryBase' is the central point of contact between user code and
-- Git data objects.  Every object must belong to some repository.
class (Applicative m, Monad m, Failure Exception m,
       Eq (Oid m), Ord (Oid m), Show (Oid m)) => RepositoryBase m where
    data Oid m
    data Tree m
    data Commit m
    data Tag m

    parseOid  :: Text -> m (Oid m)
    renderOid :: Tagged a (Oid m) -> Text

    -- References
    lookupRef  :: Text -> m (Reference m (Commit m))
    updateRef  :: Text -> RefTarget m (Commit m) -> m (Reference m (Commit m))
    updateRef_ :: Text -> RefTarget m (Commit m) -> m ()
    updateRef_ = (void .) . updateRef
    deleteRef  :: Text -> m ()

    allRefs :: m [Reference m (Commit m)]
    allRefs = mapM lookupRef =<< allRefNames

    allRefNames :: m [Text]
    allRefNames = map refName <$> allRefs

    resolveRef :: Text -> m (ObjRef m (Commit m))
    resolveRef name = lookupRef name >>= \ref ->
        case ref of
            Reference { refTarget = RefObj x } ->
                return x
            Reference { refTarget = RefSymbolic name' } ->
                if name /= name'
                then resolveRef name'
                else failure (ReferenceLookupFailed name)

    -- Lookup
    lookupCommit :: CommitOid m -> m (Commit m)
    lookupTree   :: TreeOid m -> m (Tree m)
    lookupBlob   :: BlobOid m -> m (Blob m)
    lookupTag    :: TagOid m -> m (Tag m)

    lookupObject :: Text -> m (Object m)

    -- Object creation
    newTree :: m (Tree m)
    createBlob :: BlobContents m -> m (BlobOid m)
    createCommit ::
        [ObjRef m (Commit m)] -> ObjRef m (Tree m)
        -> Signature -> Signature -> Text -> Maybe Text -> m (Commit m)
    createTag :: CommitOid m -> Signature -> Text -> Text -> m (Tag m)

{- $exceptions -}
-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data Exception = BackendError Text
               | RepositoryNotExist
               | RepositoryInvalid
               | BlobCreateFailed
               | BlobEmptyCreateFailed
               | BlobEncodingUnknown Text
               | BlobLookupFailed
               | TranslationException Text
               | TreeCreateFailed Text
               | TreeBuilderCreateFailed
               | TreeBuilderInsertFailed
               | TreeBuilderRemoveFailed
               | TreeBuilderWriteFailed
               | TreeLookupFailed
               | TreeCannotTraverseBlob
               | TreeEntryLookupFailed FilePath
               | TreeUpdateFailed
               | TreeWalkFailed
               | CommitCreateFailed
               | CommitLookupFailed
               | ReferenceCreateFailed
               | ReferenceDeleteFailed Text
               | RefCannotCreateFromPartialOid
               | ReferenceListingFailed
               | ReferenceLookupFailed Text
               | ObjectLookupFailed Text Int
               | ObjectRefRequiresFullOid
               | OidCopyFailed
               | OidParseFailed Text
               deriving (Show, Typeable)

instance Exc.Exception Exception

{- $oids -}
type BlobOid m   = Tagged (Blob m) (Oid m)
type TreeOid m   = Tagged (Tree m) (Oid m)
type CommitOid m = Tagged (Commit m) (Oid m)
type TagOid m    = Tagged (Tag m) (Oid m)

{- $references -}
data RefTarget m a = RefObj (ObjRef m a) | RefSymbolic Text

data Reference m a = Reference
    { refName   :: Text
    , refTarget :: RefTarget m a }

{- $objects -}
data ObjRef m a = ByOid (Tagged a (Oid m)) | Known a

type BlobRef m   = ObjRef m (Blob m)
type TreeRef m   = ObjRef m (Tree m)
type CommitRef m = ObjRef m (Commit m)
type TagRef m    = ObjRef m (Tag m)

data Object m = BlobObj   (BlobRef m)
              | TreeObj   (TreeRef m)
              | CommitObj (CommitRef m)
              | TagObj    (TagRef m)

{- $blobs -}
type Blob m = BlobContents m

type ByteSource m = GSource m ByteString

data BlobContents m = BlobString ByteString
                    | BlobStream (ByteSource m)
                    | BlobSizedStream (ByteSource m) Int

instance Eq (BlobContents m) where
  BlobString str1 == BlobString str2 = str1 == str2
  _ == _ = False

{- $trees -}
data TreeEntry m where
    BlobEntry :: BlobOid m -> Bool -> TreeEntry m
    TreeEntry :: ObjRef m (Tree m) -> TreeEntry m

blobEntry :: RepositoryBase m => BlobOid m -> Bool -> TreeEntry m
blobEntry = BlobEntry

treeEntry :: RepositoryBase m => Tree m -> TreeEntry m
treeEntry = TreeEntry . treeRef

-- | A 'Tree' is anything that is "treeish".
--
-- Minimal complete definition: 'modifyTree'.  Note that for some treeish
-- things, like Tags, it should always be an error to attempt to modify the
-- tree in any way.
class RepositoryBase TreeRepository => Treeish t where
    type TreeRepository :: * -> *

    modifyTree :: t           -- the tree to "modify"
               -> FilePath    -- path within the tree
               -> Bool        -- create subtree's leading up to path?
               -> (Maybe (TreeEntry TreeRepository)
                   -> TreeRepository (Maybe (TreeEntry TreeRepository)))
               -> TreeRepository (Maybe (TreeEntry TreeRepository))

    getTreeEntry :: t -> FilePath -> TreeRepository (TreeEntry TreeRepository)
    getTreeEntry t path = do
        entry <- modifyTree t path False return
        maybe (failure (TreeEntryLookupFailed path)) return entry

    putTreeEntry :: t -> FilePath -> TreeEntry TreeRepository
                 -> TreeRepository ()
    putTreeEntry t path =
        void . modifyTree t path True . const . return . Just

    putBlob :: t -> FilePath -> BlobOid TreeRepository
               -> TreeRepository ()
    putBlob t path b = putTreeEntry t path (BlobEntry b False)

    putTree :: t -> FilePath
            -> ObjRef TreeRepository (Tree TreeRepository)
            -> TreeRepository ()
    putTree t path tr = putTreeEntry t path (TreeEntry tr)

    dropFromTree :: t -> FilePath -> TreeRepository ()
    dropFromTree t path =
        void (modifyTree t path False (const (return Nothing)))

    writeTree :: t -> TreeRepository (TreeOid TreeRepository)

treeRef :: Tree m -> ObjRef m (Tree m)
treeRef = Known

treeRefOid :: (RepositoryBase TreeRepository, Treeish (Tree TreeRepository))
           => ObjRef TreeRepository (Tree TreeRepository)
           -> TreeRepository (TreeOid TreeRepository)
treeRefOid (ByOid x) = return x
treeRefOid (Known x) = writeTree x

resolveTree :: RepositoryBase m => ObjRef m (Tree m) -> m (Tree m)
resolveTree objRef = case objRef of
    ByOid oid -> lookupTree oid
    Known obj -> return obj

defaultCommitModifyTree ::
    (Repository m, Commitish t, Treeish t)
    => t
    -> FilePath
    -> Bool
    -> (Maybe (TreeEntry m)
        -> m (Maybe (TreeEntry m)))
    -> m (Maybe (TreeEntry m))
defaultCommitModifyTree c path createIfNotExist f =
    Git.commitTree' c >>= \t -> Git.modifyTree t path createIfNotExist f

defaultCommitWriteTree ::
    (Repository m, Commitish t, Treeish t)
    => t
    -> m (TreeOid m)
defaultCommitWriteTree = commitTree' >=> writeTree

{- $commits -}
data Signature = Signature
    { signatureName  :: Text
    , signatureEmail :: Text
    , signatureWhen  :: UTCTime
    } deriving (Show, Eq)

instance Default Signature where
    def = Signature
        { signatureName  = T.empty
        , signatureEmail = T.empty
        , signatureWhen  =
            UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
                    , utctDayTime = secondsToDiffTime 0 }
        }

class (RepositoryBase CommitRepository, Treeish c) => Commitish c where
    type CommitRepository :: * -> *

    commitOid     :: c -> CommitOid CommitRepository
    commitParents :: c -> [ObjRef CommitRepository (Commit CommitRepository)]
    commitTree    :: c -> ObjRef CommitRepository (Tree CommitRepository)

    commitTree' :: c -> CommitRepository (Tree CommitRepository)
    commitTree' c = case commitTree c of
        Known t   -> return t
        ByOid oid -> lookupTree oid

commitRef :: Commit c -> ObjRef m (Commit c)
commitRef = Known

commitRefTarget :: Commit c -> RefTarget m (Commit c)
commitRefTarget = RefObj . Known

commitRefOid :: (RepositoryBase CommitRepository,
                 Commitish (Commit CommitRepository))
             => ObjRef CommitRepository (Commit CommitRepository)
             -> CommitOid CommitRepository
commitRefOid (ByOid x) = x
commitRefOid (Known x) = commitOid x

resolveCommit :: RepositoryBase m => ObjRef m (Commit m) -> m (Commit m)
resolveCommit objRef = case objRef of
    ByOid oid -> lookupCommit oid
    Known obj -> return obj

{- $tags -}

{- $misc -}
type Repository m = (RepositoryBase m, m ~ TreeRepository, m ~ CommitRepository,
                     Treeish (Tree m), Commitish (Commit m))

-- Repository.hs
