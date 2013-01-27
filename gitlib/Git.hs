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
class Repository r where
    -- References
    lookupRef  :: r -> Text -> GitM (Reference r)
    updateRef  :: r -> Text -> Reference r -> GitM (Reference r)
    updateRef_ :: r -> Text -> Reference r -> GitM ()
    updateRef_ repo name ref = void (updateRef repo name ref)

    traverseRefs :: Traversable t => r -> (Reference r -> GitM b) -> GitM (t b)

    resolveRef :: r -> Text -> GitM Oid
    resolveRef repo name = lookupRef repo name >>= \ref ->
        case ref of
            Reference { refTarget = RefOid oid } ->
                return oid
            Reference { refTarget = RefSymbolic name' } ->
                if name /= name'
                then resolveRef repo name'
                else failure (ReferenceLookupFailed name)

    -- Lookup
    lookupObject :: Proxy a -> r -> Oid -> GitM a

    lookupCommit :: Commit a => r -> Oid -> GitM a
    lookupCommit = lookupObject (Proxy :: Proxy a)

    lookupCommitFromRef :: Commit a => r -> Text -> GitM a
    lookupCommitFromRef repo name =
        resolveRef repo name >>= lookupCommit repo

    lookupTree :: Tree a => r -> Oid -> GitM a
    lookupTree = lookupObject (Proxy :: Proxy a)

    lookupBlob :: Blob a => r -> Oid -> GitM a
    lookupBlob = lookupObject (Proxy :: Proxy a)

    lookupTag :: Tag a => r -> Oid -> GitM a
    lookupTag = lookupObject (Proxy :: Proxy a)

    lookupTagFromRef :: Tag a => r -> Text -> GitM a
    lookupTagFromRef repo name =
        resolveRef repo name >>= lookupTag repo

    -- Object creation
    newTree :: Tree a => r -> a

    createBlob :: Blob b => r -> ByteString -> GitM b
    createCommit :: (Commit c, Tree t) => r -> [ObjRef c] -> ObjRef t
                    -> Signature -> Signature -> Text -> GitM c

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

data Oid = FullOid ByteString
         | PartialOid ByteString Int
         deriving (Eq)

instance Show Oid where
    show (FullOid x)      = BC.unpack (hex x)
    show (PartialOid x _) = BC.unpack (hex x)

data RefTarget = RefOid Oid
               | RefSymbolic Text
               deriving (Show, Eq)

data Reference a = Reference { refRepo   :: a
                             , refName   :: Text
                             , refTarget :: RefTarget }
                 deriving (Show, Eq)

type GitM a = (Applicative m, Monad m, Failure Exception m) => m a

class Object o where
    repository :: Repository r => o -> r
    update     :: o -> GitM o
    oid        :: o -> GitM (o,Oid)

    update_ :: o -> GitM ()
    update_ = void . update

data ObjRef a where
    PendingObj :: Repository r => r -> Oid -> ObjRef a
    KnownObj   :: a -> ObjRef a

resolveObjRef :: ObjRef a -> GitM a
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
  BlobEmpty == BlobEmpty = True
  BlobString str1 == BlobString str2 = str1 == str2
  BlobStream src1 == BlobStream src2 = False
  _ == _ = False

blobSourceToString :: BlobContents -> IO ByteString
blobSourceToString BlobEmpty = return B.empty
blobSourceToString (BlobString bs) = return bs
blobSourceToString (BlobStream bs) = do strs <- bs $$ CList.consume
                                        return (B.concat strs)

class Object b => Blob b where
    blobContents :: b -> GitM ByteString
    blobLength   :: Integral l => b -> GitM l

data TreeEntry where
    BlobEntry :: Blob b => ObjRef b -> TreeEntry
    TreeEntry :: Tree t => ObjRef t -> TreeEntry

class Object t => Tree t where
    treeEntry :: t -> FilePath -> GitM TreeEntry
    putBlobInTree :: Blob b => t -> FilePath -> ObjRef b -> t
    putTreeInTree :: Tree t2 => t -> FilePath -> ObjRef t2 -> t
    dropFromTree :: t -> FilePath -> t

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
    commitParents' :: c -> GitM [c]

    commitTree :: Tree t => c -> ObjRef t
    commitTree' :: Tree t => c -> GitM t
    commitTree' = resolveObjRef . commitTree

class Commit t => Tag t where
    tagCommit :: Commit c => t -> GitM (ObjRef c)

parseOid :: Text -> GitM Oid
parseOid oid
    | len > 40 = failure (OidParseFailed oid)
    | otherwise = (if len == 40 then FullOid else flip PartialOid len)
                  <$> unhex (T.encodeUtf8 oid)
  where len = T.length oid

-- Repository.hs
