{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Interface for working with Git repositories.
module Git where

import           Control.Applicative
import qualified Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import           Data.Conduit
import qualified Data.Conduit.List as CList
import           Data.Default
import           Data.Foldable
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

-- | A 'Repository' is the central point of contact between user code and Git
-- data objects.  Every object must belong to some repository.
--
-- Minimal complete definition: 'lookupRef', 'updateRef', 'lookupObject'.
class Repository m where
    -- References
    lookupRef  :: Text -> m Reference
    updateRef  :: Text -> Reference r -> m Reference
    updateRef_ :: Text -> Reference r -> m ()
    updateRef_ repo name ref = void (updateRef repo name ref)

    traverseRefs :: Traversable t => (Reference -> m b) -> m (t b)

    resolveRef :: Text -> m Oid
    resolveRef repo name = lookupRef repo name >>= \ref ->
        case ref of
            Reference { refTarget = RefOid oid } ->
                return oid
            Reference { refTarget = RefSymbolic name' } ->
                if name /= name'
                then resolveRef repo name'
                else failure (ReferenceLookupFailed name)

    -- Lookup
    -- jww (2013-01-27): Allow lookup by partial oid
    lookupObject :: Proxy a -> Oid -> m a

    lookupCommit :: Commit a => Oid -> m a
    lookupCommit = lookupObject (Proxy :: Proxy a)

    lookupCommitFromRef :: Commit a => Text -> m a
    lookupCommitFromRef repo name =
        resolveRef repo name >>= lookupCommit repo

    lookupTree :: Tree a => Oid -> m a
    lookupTree = lookupObject (Proxy :: Proxy a)

    lookupBlob :: Blob a => Oid -> m a
    lookupBlob = lookupObject (Proxy :: Proxy a)

    lookupTag :: Tag a => Oid -> m a
    lookupTag = lookupObject (Proxy :: Proxy a)

    lookupTagFromRef :: Tag a => Text -> m a
    lookupTagFromRef repo name =
        resolveRef repo name >>= lookupTag repo

    -- Object creation
    newTree :: Tree a => m a

    createBlob :: Blob b => ByteString -> m b
    createCommit :: (Commit c, Tree t) => [ObjRef c] -> ObjRef t
                    -> Signature -> Signature -> Text -> m c

-- | There is a separate 'GitException' for each possible failure when
--   interacting with the Git repository.
data Exception = RepositoryNotExist FilePath
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
               | ReferenceLookupFailed Text
               | ObjectLookupFailed Oid
               | ObjectRefRequiresFullOid
               | OidCopyFailed
               | OidParseFailed Text
               deriving (Show, Typeable)

instance Exc.Exception Exception

newtype Oid = Oid ByteString deriving (Eq)

instance Show Oid where
    show x = BC.unpack (hex x)

data RefTarget = RefOid Oid
               | RefSymbolic Text
               deriving (Show, Eq)

data Reference a = Reference { refRepo   :: a
                             , refName   :: Text
                             , refTarget :: RefTarget }
                 deriving (Show, Eq)

class Object o where
    repository :: Repository r => o -> r
    update     :: o -> m o
    oid        :: o -> m (o,Oid)

    update_ :: o -> m ()
    update_ = void . update

data ObjRef a where
    PendingObj :: Repository r => r -> Oid -> ObjRef a
    KnownObj   :: a -> ObjRef a

resolveObjRef :: ObjRef a -> m a
resolveObjRef objRef = case objRef of
    PendingObj repo oid -> lookupObject (Proxy :: Proxy a) repo oid
    KnownObj obj        -> return obj

blobRef :: Blob b => b -> ObjRef b
blobRef = KnownObj

treeRef :: Tree t => t -> ObjRef t
treeRef = KnownObj

commitRef :: Commit c => c -> ObjRef c
commitRef = KnownObj

type ByteSource = GSource IO ByteString

data BlobContents = BlobEmpty
                  | BlobString ByteString
                  | BlobStream ByteSource

instance Eq BlobContents where
  BlobEmpty       == BlobEmpty       = True
  BlobString str1 == BlobString str2 = str1 == str2
  BlobStream src1 == BlobStream src2 = False
  _ == _ = False

blobSourceToString :: BlobContents -> IO ByteString
blobSourceToString BlobEmpty       = return B.empty
blobSourceToString (BlobString bs) = return bs
blobSourceToString (BlobStream bs) = do strs <- bs $$ CList.consume
                                        return (B.concat strs)

class Object b => Blob b where
    blobContents :: b -> m ByteString
    blobLength   :: Integral l => b -> m l

data TreeEntry where
    BlobEntry :: Blob b => ObjRef b -> TreeEntry
    TreeEntry :: Tree t => ObjRef t -> TreeEntry

class Object t => Tree t where
    treeEntry     :: t -> FilePath -> m TreeEntry
    putBlobInTree :: Blob b => t -> FilePath -> ObjRef b -> t
    putTreeInTree :: Tree t2 => t -> FilePath -> ObjRef t2 -> t
    dropFromTree  :: t -> FilePath -> t

data Signature = Signature { signatureName  :: Text
                           , signatureEmail :: Text
                           , signatureWhen  :: UTCTime }
               deriving (Show, Eq)

-- | Convert a time in seconds (from Stripe's servers) to 'UTCTime'. See
--   "Data.Time.Format" for more on working with 'UTCTime'.
fromSeconds :: Integer -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

-- | Convert a 'UTCTime' back to an Integer suitable for use with Stripe's API.
toSeconds :: UTCTime -> Integer
toSeconds = round . utcTimeToPOSIXSeconds

instance Default Signature where
    def = Signature { signatureName  = T.empty
                    , signatureEmail = T.empty
                    , signatureWhen  =
                        UTCTime { utctDay =
                                       ModifiedJulianDay {
                                           toModifiedJulianDay = 0 }
                                , utctDayTime = secondsToDiffTime 0 } }

class Object c => Commit c where
    commitParents :: c -> [ObjRef c]
    commitParents' :: c -> m [c]

    commitTree :: Tree t => c -> ObjRef t
    commitTree' :: Tree t => c -> m t
    commitTree' = resolveObjRef . commitTree

class Commit t => Tag t where
    tagCommit :: Commit c => t -> m (ObjRef c)

parseOid :: Text -> m Oid
parseOid oid
    | len /= 40 = failure (OidParseFailed oid)
    | otherwise = unhex (T.encodeUtf8 oid)
  where len = T.length oid

-- Repository.hs
