{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}

-- | Interface for working with Git repositories.
module Git where

import           Control.Applicative
import qualified Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Attempt
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Conduit
-- import           Data.Conduit.Internal
import qualified Data.Conduit.List as CList
import           Data.Default
-- import           Data.Dynamic
import           Data.Foldable
import           Data.Function.Pointless
import           Data.Hex
import           Data.Proxy
import           Data.Tagged
-- import           Data.Typeable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.ICU.Convert as U
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Traversable
import           Data.Typeable
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)
import           Text.Printf
import System.IO.Unsafe

{- $repositories -}
-- | A 'Repository' is the central point of contact between user code and Git
-- data objects.  Every object must belong to some repository.
--
-- Minimal complete definition: 'lookupRef', 'updateRef', 'lookupObject'.
class (Applicative m, Monad m, Failure Exception m) => Repository m where
    -- References
    lookupRef  :: Text -> m Reference
    updateRef  :: Text -> Reference -> m Reference
    updateRef_ :: Text -> Reference -> m ()
    updateRef_ = void .: updateRef

    traverseRefs :: Traversable t => (Reference -> m b) -> m (t b)
    traverseRefs _ =
        failure (BackendError "Backend does not allow traversal of references")

    resolveRef :: Text -> m Oid
    resolveRef name = lookupRef name >>= \ref ->
        case ref of
            Reference { refTarget = RefOid oid } -> return oid
            Reference { refTarget = RefSymbolic name' } ->
                if name /= name'
                then resolveRef name'
                else failure (ReferenceLookupFailed name)

    -- Lookup
    -- lookupObject :: Text -> m Dynamic
    -- lookupObject _ =
    --     failure (BackendError "Cannot lookup arbitrary objects in this backend")

    lookupCommit :: Commit c => Oid -> m c
    lookupTree :: Tree t => Oid -> m t
    lookupBlob :: Oid -> m (Blob m)
    lookupTag :: Oid -> m Tag

    lookupCommitRef :: Commit c => Text -> m c
    lookupCommitRef = resolveRef >=> lookupCommit

    lookupTagRef :: Text -> m Tag
    lookupTagRef = resolveRef >=> lookupTag

    -- Object creation
    newTree :: Tree t => m t
    createBlob :: BlobContents m -> m (BlobOid m)
    createCommit ::
        (Commit c, Tree t) =>
        [ObjRef c] -> ObjRef t -> Signature -> Signature -> Text -> m c

{- $exceptions -}
-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data Exception = BackendError Text
               | RepositoryNotExist
               | RepositoryInvalid
               | BlobCreateFailed
               | BlobEmptyCreateFailed
               | TreeCreateFailed
               | TreeBuilderCreateFailed
               | TreeBuilderInsertFailed
               | TreeBuilderWriteFailed
               | TreeLookupFailed
               | TreeCannotTraverseBlob
               | TreeEntryLookupFailed FilePath
               | TreeUpdateFailed
               | CommitCreateFailed
               | CommitLookupFailed
               | ReferenceCreateFailed
               | RefCannotCreateFromPartialOid
               | ReferenceListingFailed
               | ReferenceLookupFailed Text
               | ObjectLookupFailed Text
               | ObjectRefRequiresFullOid
               | OidCopyFailed
               | OidParseFailed Text
               deriving (Show, Typeable)

instance Exc.Exception Exception

{- $oids -}
newtype Oid = Oid ByteString deriving Eq

instance Show Oid where
    show (Oid x) = BC.unpack (hex x)

type BlobOid m = Tagged (Blob m) Oid
type TreeOid   = Tree t => Tagged t Oid
type CommitOid = Commit c => Tagged c Oid
type TagOid    = Tagged Tag Oid

-- | Parse an ASCII hex string into a Git 'Oid'.
--
-- >>> let x = "2506e7fcc2dbfe4c083e2bd741871e2e14126603"
-- >>> assert $ show (parseOid x) == x
parseOid :: Repository m => Text -> m Oid
parseOid oid
    | T.length oid /= 40 = failure (OidParseFailed oid)
    | otherwise =
        -- 'unsafePerformIO' is used to force 'unhex' to run in the 'IO'
        -- monad, so we can catch the exception on failure and repackage it
        -- using 'Control.Failure'.  It were really better if 'unhex' returned
        -- an 'Either' value instead of calling 'fail'.
        let x = unsafePerformIO $
                Exc.catch (Success <$> unhex (T.encodeUtf8 oid))
                          (\e -> return (Failure (e :: Exc.IOException)))
        in case x of
            Success y -> return (Oid y)
            Failure _ -> failure (OidParseFailed oid)

{- $references -}
data RefTarget = RefOid Oid | RefSymbolic Text deriving (Show, Eq)

data Reference = Reference
    { refName   :: Text
    , refTarget :: RefTarget
    } deriving (Show, Eq)

{- $objects -}
class Object o where
    update :: Repository m => o -> m (Tagged o Oid)
    update_ :: Repository m => o -> m ()
    update_ = void . update

data ObjRef a = ByOid (Tagged a Oid) | Known a

instance Object a => Object (ObjRef a) where
    update (ByOid oid) = return (retag oid)
    update (Known obj) = retag <$> update obj

{- $blobs -}
newtype Blob m = Blob { blobContents :: BlobContents m }

-- instance Typeable (Blob m) where
--     typeOf (Blob x) = mkTyConApp (mkTyCon3 "gitlib" "Git" "Blob") [typeOf x]

type ByteSource m = GSource m ByteString

data BlobContents m = BlobString ByteString
                    | BlobStream (ByteSource m)
                    | BlobSizedStream (ByteSource m) Int

instance Eq (BlobContents m) where
  BlobString str1 == BlobString str2 = str1 == str2
  _ == _ = False

-- instance Typeable (BlobContents m) where
--     typeOf (BlobString x) =
--         mkTyConApp (mkTyCon3 "gitlib" "Git" "BlobContents") [typeOf x]
--     typeOf (BlobStream x) =
--         mkTyConApp (mkTyCon3 "gitlib" "Git" "BlobContents") [typeOf x]
--     typeOf (BlobSizedStream x l) =
--         mkTyConApp (mkTyCon3 "gitlib" "Git" "BlobContents") [typeOf x, typeOf l]

blobContentsToByteString :: Repository m => BlobContents m -> m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobContentsToByteString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

blobToByteString :: Repository m => Blob m -> m ByteString
blobToByteString = blobContentsToByteString . blobContents

catBlob :: Repository m => Text -> m ByteString
catBlob = parseOid >=> lookupBlob >=> blobToByteString

catBlobUtf8 :: Repository m => Text -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobRef :: Blob m -> ObjRef (Blob m)
blobRef = Known

{- $trees -}
data TreeEntry m where
    BlobEntry :: Object (Blob m) => ObjRef (Blob m) -> Bool -> TreeEntry m
    TreeEntry :: (Object t, Tree t) => ObjRef t -> TreeEntry m

instance Object (TreeEntry m) where
    update (BlobEntry bl _) = retag <$> update bl
    update (TreeEntry tr)   = retag <$> update tr

blobEntry :: (Repository m, Object (Blob m)) => Blob m -> Bool -> TreeEntry m
blobEntry b exe = BlobEntry (blobRef b) exe

treeEntry :: (Repository m, Object t, Tree t) => t -> TreeEntry m
treeEntry t = TreeEntry (treeRef t)

-- | A 'Tree' is anything that is "treeish".
--
-- Minimal complete definition: 'modifyTree'.  Note that for some treeish
-- things, like Tags, it should always be an error to attempt to modify the
-- tree in any way.
class Tree t where
    modifyTree :: Repository m =>
                  t             -- the tree to "modify"
                  -> FilePath    -- path within the tree
                  -> Bool        -- create subtree's leading up to path?
                  -> (Maybe (TreeEntry m) -> m (Maybe (TreeEntry m)))
                  -> m (Maybe (TreeEntry m))

    getTreeEntry :: Repository m => t -> FilePath -> m (TreeEntry m)
    getTreeEntry t path = do
        entry <- modifyTree t path False return
        maybe (failure (TreeEntryLookupFailed path)) return entry

    putTreeEntry :: Repository m => t -> FilePath -> TreeEntry m -> m ()
    putTreeEntry t path =
        void . modifyTree t path True . const . return . Just

    putBlobInTree :: (Repository m, Object (Blob m)) =>
                     t -> FilePath -> ObjRef (Blob m) -> m ()
    putBlobInTree t path b = putTreeEntry t path (BlobEntry b False)

    putTreeInTree :: (Repository m, Object t) =>
                     t -> FilePath -> ObjRef t -> m ()
    putTreeInTree t path tr = putTreeEntry t path (TreeEntry tr)

    dropFromTree :: Repository m => t -> FilePath -> m ()
    dropFromTree t path =
        void (modifyTree t path False (const (return Nothing)))

treeRef :: Tree t => t -> ObjRef t
treeRef = Known

resolveTreeRef :: (Repository m, Tree t) => ObjRef t -> m t
resolveTreeRef objRef = case objRef of
    ByOid oid -> lookupTree (unTagged oid)
    Known obj -> return obj

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

class Commit c where
    commitParents :: Repository m => c -> m [ObjRef c]
    commitTree    :: (Repository m, Tree t) => c -> m (ObjRef t)
    commitTree'   :: (Repository m, Tree t) => c -> m t

commitRef :: Commit c => c -> ObjRef c
commitRef = Known

resolveCommitRef :: (Repository m, Commit c) => ObjRef c -> m c
resolveCommitRef objRef = case objRef of
    ByOid oid -> lookupCommit (unTagged oid)
    Known obj -> return obj

{- $tags -}
data Tag = Tag
    { tagCommit  :: (Repository m, Commit c) => m (ObjRef c)
    , tagCommit' :: (Repository m, Commit c) => m c
    }

-- Repository.hs
