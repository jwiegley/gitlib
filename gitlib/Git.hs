{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

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
import           Data.Char (toLower)
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
import           System.IO.Unsafe
import           Text.Printf

{- $repositories -}
-- | A 'Repository' is the central point of contact between user code and Git
-- data objects.  Every object must belong to some repository.
class (Applicative m, Monad m, Failure Exception m) => Repository m where
    data Tree m
    data Commit m

    -- References
    lookupRef  :: Text -> m Reference
    updateRef  :: Text -> Reference -> m Reference
    updateRef_ :: Text -> Reference -> m ()
    updateRef_ = void .: updateRef

    traverseRefs :: Traversable tr => (Reference -> m b) -> m (tr b)
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

    lookupCommit :: CommitOid m -> m (Commit m)
    lookupTree   :: TreeOid m -> m (Tree m)
    lookupBlob   :: BlobOid m -> m (Blob m)
    lookupTag    :: TagOid -> m Tag

    lookupObject :: Text -> Int -> m (Object m)

    lookupCommitRef :: Text -> m (Commit m)
    lookupCommitRef = resolveRef >=> lookupCommit . Tagged

    lookupTagRef :: Text -> m Tag
    lookupTagRef = resolveRef >=> lookupTag . Tagged

    -- Object creation
    newTree :: m (Tree m)
    createBlob :: BlobContents m -> m (BlobOid m)
    createCommit :: [CommitOid m] -> ObjRef (Tree m)
                    -> Signature -> Signature -> Text -> Maybe Text
                    -> m (Commit m)
    createTag :: CommitOid m -> Signature -> Signature -> Text -> m Tag

data Object m = BlobRef   (ObjRef (Blob m))
              | TreeRef   (ObjRef (Tree m))
              | CommitRef (ObjRef (Commit m))
              | TagRef    (ObjRef Tag)

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
               | TreeBuilderRemoveFailed
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
               | ObjectLookupFailed Text Int
               | ObjectRefRequiresFullOid
               | OidCopyFailed
               | OidParseFailed Text
               deriving (Show, Typeable)

instance Exc.Exception Exception

{- $oids -}
newtype Oid = Oid ByteString deriving Eq

instance Show Oid where
    show (Oid x) = map toLower (BC.unpack (hex x))

type BlobOid m   = Tagged (Blob m) Oid
type TreeOid m   = Tagged (Tree m) Oid
type CommitOid m = Tagged (Commit m) Oid
type TagOid      = Tagged Tag Oid

-- | Parse an ASCII hex string into a Git 'Oid'.
--
-- >>> let x = "2506e7fcc2dbfe4c083e2bd741871e2e14126603"
-- >>> parseOid (T.pack x)
-- Just 2506e7fcc2dbfe4c083e2bd741871e2e14126603
parseOid :: Text -> Maybe Oid
parseOid oid
    | T.length oid /= 40 = Nothing
    | otherwise =
        -- 'unsafePerformIO' is used to force 'unhex' to run in the 'IO'
        -- monad, so we can catch the exception on failure and repackage it
        -- using 'Maybe'.  Why does 'unhex' have to be in IO at all?
        unsafePerformIO $
        Exc.catch (Just . Oid <$> unhex (T.encodeUtf8 oid))
                  (\x -> (x :: Exc.IOException) `seq` return Nothing)

{- $references -}
data RefTarget = RefOid Oid | RefSymbolic Text deriving (Show, Eq)

data Reference = Reference
    { refName   :: Text
    , refTarget :: RefTarget
    } deriving (Show, Eq)

{- $objects -}
data ObjRef a = ByOid (Tagged a Oid) | Known a

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
catBlob str = do
    if len == 40
        then case parseOid str of
        Nothing  -> failure (OidParseFailed str)
        Just oid -> lookupBlob (Tagged oid) >>= blobToByteString

        else do
        obj <- lookupObject str len
        case obj of
            BlobRef (ByOid oid) -> lookupBlob oid >>= blobToByteString
            _ -> failure (ObjectLookupFailed str len)
  where
    len = T.length str

catBlobUtf8 :: Repository m => Text -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

{- $trees -}
data TreeEntry m where
    BlobEntry :: BlobOid m -> Bool -> TreeEntry m
    TreeEntry :: ObjRef (Tree m) -> TreeEntry m

blobEntry :: Repository m => BlobOid m -> Bool -> TreeEntry m
blobEntry = BlobEntry

treeEntry :: Repository m => Tree m -> TreeEntry m
treeEntry t = TreeEntry (treeRef t)

-- | A 'Tree' is anything that is "treeish".
--
-- Minimal complete definition: 'modifyTree'.  Note that for some treeish
-- things, like Tags, it should always be an error to attempt to modify the
-- tree in any way.
class Repository TreeRepository => Treeish t where
    type TreeRepository :: * -> *

    modifyTree :: t             -- the tree to "modify"
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

    putBlobInTree :: t -> FilePath -> BlobOid TreeRepository
                     -> TreeRepository ()
    putBlobInTree t path b = putTreeEntry t path (BlobEntry b False)

    putTreeInTree :: t -> FilePath -> ObjRef (Tree TreeRepository)
                     -> TreeRepository ()
    putTreeInTree t path tr = putTreeEntry t path (TreeEntry tr)

    dropFromTree :: t -> FilePath -> TreeRepository ()
    dropFromTree t path =
        void (modifyTree t path False (const (return Nothing)))

    writeTree :: t -> TreeRepository (TreeOid TreeRepository)

treeRef :: Tree m -> ObjRef (Tree m)
treeRef = Known

resolveTreeRef :: Repository m => ObjRef (Tree m) -> m (Tree m)
resolveTreeRef objRef = case objRef of
    ByOid oid -> lookupTree oid
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

class (Repository CommitRepository, Treeish c) => Commitish c where
    type CommitRepository :: * -> *

    commitOid     :: c -> CommitOid CommitRepository
    commitParents :: c -> [CommitOid CommitRepository]
    commitTree    :: c -> ObjRef (Tree CommitRepository)

    commitTree' :: c -> CommitRepository (Tree CommitRepository)
    commitTree' c = case commitTree c of
        Known t   -> return t
        ByOid oid -> lookupTree oid

commitRef :: Commit c -> ObjRef (Commit c)
commitRef = Known

resolveCommitRef :: Repository m => ObjRef (Commit m) -> m (Commit m)
resolveCommitRef objRef = case objRef of
    ByOid oid -> lookupCommit oid
    Known obj -> return obj

{- $tags -}
data Tag = Tag
    { tagCommit :: Repository m => CommitOid m }

-- Repository.hs
