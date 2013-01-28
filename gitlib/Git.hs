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
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Foldable
import           Data.Function.Pointless
import           Data.Hex
import           Data.Proxy
import           Data.Tagged
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

    resolveRef :: Text -> m Oid
    resolveRef name = lookupRef name >>= \ref ->
        case ref of
            Reference { refTarget = RefOid oid } -> return oid
            Reference { refTarget = RefSymbolic name' } ->
                if name /= name'
                then resolveRef name'
                else failure (ReferenceLookupFailed name)

    -- Lookup
    lookupCommit :: Oid -> m Commit
    lookupTree :: Oid -> m Tree
    lookupBlob :: Oid -> m (Blob m)
    lookupTag :: Oid -> m Tag

    lookupCommitRef :: Text -> m Commit
    lookupCommitRef = resolveRef >=> lookupCommit

    lookupTagRef :: Text -> m Tag
    lookupTagRef = resolveRef >=> lookupTag

    -- Object creation
    newTree :: m Tree
    createBlob :: BlobContents m -> m (BlobOid m)
    createCommit :: [ObjRef Commit] -> ObjRef Tree -> Signature -> Signature
                    -> Text -> m c

{- $exceptions -}
-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data Exception = RepositoryNotExist
               | RepositoryInvalid
               | BlobCreateFailed
               | BlobEmptyCreateFailed
               | TreeCreateFailed
               | TreeBuilderCreateFailed
               | TreeBuilderInsertFailed
               | TreeBuilderWriteFailed
               | TreeLookupFailed
               | TreeCannotTraverseBlob
               | TreeEntryLookupFailed
               | TreeUpdateFailed
               | CommitCreateFailed
               | CommitLookupFailed
               | ReferenceCreateFailed
               | RefCannotCreateFromPartialOid
               | ReferenceListingFailed
               | ReferenceLookupFailed Text
               | ObjectLookupFailed Oid
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
type TreeOid   = Tagged Tree Oid
type CommitOid = Tagged Commit Oid
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

-- resolveObjRef :: Repository m => ObjRef a -> m a
-- resolveObjRef objRef = case objRef of
--     ByOid oid -> lookupObject (Proxy :: Proxy a) (unTagged oid)
--     Known obj -> return obj

{- $blobs -}
newtype Blob m = Blob { blobContents :: BlobContents m }

type ByteSource m = GSource m ByteString

data BlobContents m = BlobString ByteString
                    | BlobStream (ByteSource m)
                    | BlobSizedStream (ByteSource m) Int

instance Eq (BlobContents m) where
  BlobString str1 == BlobString str2 = str1 == str2
  _ == _ = False

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
data TreeEntry m = BlobEntry (ObjRef (Blob m))
                 | TreeEntry (ObjRef Tree)

data Tree = Tree
    { treeEntry     :: Repository m => FilePath -> m (TreeEntry m)
    , putBlobInTree :: Repository m => FilePath -> ObjRef (Blob m) -> m ()
    , putTreeInTree :: Repository m => FilePath -> ObjRef Tree -> m ()
    , dropFromTree  :: Repository m => FilePath -> m ()
    }

treeRef :: Tree -> ObjRef Tree
treeRef = Known

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

{- $commits -}
data Commit = Commit
    { commitParents :: Repository m => m [ObjRef Commit]
    , commitTree    :: Repository m => m (ObjRef Tree)
    , commitTree'   :: Repository m => m Tree
    }

commitRef :: Commit -> ObjRef Commit
commitRef = Known

{- $tags -}
data Tag = Tag
    { tagCommit  :: Repository m => m (ObjRef Commit)
    , tagCommit' :: Repository m => m Commit
    }

-- Repository.hs
