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

{- $repositories -}
-- | A 'Repository' is the central point of contact between user code and Git
-- data objects.  Every object must belong to some repository.
--
-- Minimal complete definition: 'lookupRef', 'updateRef', 'lookupObject'.
class Repository m where
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
    -- jww (2013-01-27): Allow lookup by partial oid
    lookupObject :: Proxy a -> Oid -> m a

    lookupCommit :: Oid -> m Commit
    lookupCommit = lookupObject (Proxy :: Proxy Commit)

    lookupCommitRef :: Text -> m Commit
    lookupCommitRef = resolveRef >=> lookupCommit

    lookupTree :: Oid -> m Tree
    lookupTree = lookupObject (Proxy :: Proxy Commit)

    lookupBlob :: Oid -> m (Blob m)
    lookupBlob = lookupObject (Proxy :: Proxy Commit)

    lookupTag :: Oid -> m Tag
    lookupTag = lookupObject (Proxy :: Proxy Commit)

    lookupTagRef :: Text -> m Tag
    lookupTagRef = resolveRef >=> lookupTag

    -- Object creation
    newTree :: m Tree

    createBlob :: ByteSource m -> m (BlobOid m)
    createCommit :: [ObjRef Commit] -> ObjRef Tree -> Signature -> Signature
                    -> Text -> m c

{- $exceptions -}
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

{- $oids -}
newtype Oid = Oid ByteString deriving Eq

instance Show Oid where
    show x = BC.unpack (hex x)

newtype BlobOid m = BlobOid (Tagged (Blob m) Oid) deriving Eq
newtype TreeOid   = TreeOid (Tagged Tree Oid)     deriving Eq
newtype CommitOid = CommitOid (Tagged Commit Oid) deriving Eq
newtype TagOid    = TagOid (Tagged Tag Oid)       deriving Eq

parseOid :: Text -> m Oid
parseOid oid
    | len /= 40 = failure (OidParseFailed oid)
    | otherwise = unhex (T.encodeUtf8 oid)
  where len = T.length oid

{- $references -}
data RefTarget = RefOid Oid | RefSymbolic Text deriving (Show, Eq)

data Reference = Reference
    { refName   :: Text
    , refTarget :: RefTarget } deriving (Show, Eq)

{- $objects -}
class Object o where
    update :: Repository m => o -> m (Tagged o Oid)
    update_ :: Repository m => o -> m ()
    update_ = void . update

data ObjRef a = ByOid (Tagged a Oid) | Known a deriving Show

resolveObjRef :: ObjRef a -> m a
resolveObjRef objRef = case objRef of
    ByOid oid -> lookupObject (Proxy :: Proxy a) oid
    Known obj -> return obj

{- $blobs -}
newtype Blob m = Blob (BlobContents m)

type ByteSource m = GSource m ByteString

data BlobContents m = BlobString ByteString
                    | BlobStream (ByteSource m)
                    | BlobSizedStream (ByteSource m) Int

instance Eq (BlobContents m) where
  BlobString str1 == BlobString str2 = str1 == str2
  _ == _ = False

blobSourceToString :: MonadIO m => BlobContents m -> m ByteString
blobSourceToString (BlobString bs) = return bs
blobSourceToString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobSourceToString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

{- $trees -}
data TreeEntry m = BlobEntry (ObjRef (Blob m))
                 | TreeEntry (ObjRef Tree)

data Tree = Tree
    { treeEntry     :: Repository m => FilePath -> m (TreeEntry m)
    , putBlobInTree :: Repository m => FilePath -> ObjRef (Blob m) -> m ()
    , putTreeInTree :: Repository m => FilePath -> ObjRef Tree -> m ()
    , dropFromTree  :: Repository m => FilePath -> m () }

treeRef :: Tree -> ObjRef Tree
treeRef = Known

data Signature = Signature
    { signatureName  :: Text
    , signatureEmail :: Text
    , signatureWhen  :: UTCTime } deriving (Show, Eq)

instance Default Signature where
    def = Signature
        { signatureName  = T.empty
        , signatureEmail = T.empty
        , signatureWhen  =
            UTCTime { utctDay = ModifiedJulianDay { toModifiedJulianDay = 0 }
                    , utctDayTime = secondsToDiffTime 0 } }

{- $commits -}
data Commit = Commit
    { commitParents :: Repository m => m [ObjRef Commit]
    , commitTree    :: Repository m => m (ObjRef Tree)
    , commitTree'   :: Repository m => m Tree }

commitRef :: Commit -> ObjRef Commit
commitRef = Known

{- $tags -}
data Tag = Tag
    { tagCommit  :: Repository m => m (ObjRef Commit)
    , tagCommit' :: Repository m => m Commit }

-- Repository.hs
