{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Git.Utils where

import           Control.Applicative
import           Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as CList
import           Data.Function
import           Data.List
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable
import           Filesystem.Path.CurrentOS hiding (null)
import           Git
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

oid :: Treeish t => t -> TreeRepository t Text
oid t = renderOid <$> writeTree t

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Repository m => Text -> m ByteString
catBlob str = do
    if len == 40
        then do
        oid <- parseOid str
        lookupBlob (Tagged oid) >>= blobToByteString

        else do
        obj <- lookupObject str
        case obj of
            BlobObj (ByOid oid) -> lookupBlob oid >>= blobToByteString
            BlobObj (Known x)   -> blobToByteString x
            _ -> failure (ObjectLookupFailed str len)
  where
    len = T.length str

catBlobUtf8 :: Repository m => Text -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobToByteString :: Repository m => BlobContents m -> m ByteString
blobToByteString (BlobString bs) = return bs
blobToByteString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobToByteString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
commitHistoryFirstParent c =
  case commitParents c of
    []    -> return [c]
    (p:_) -> do ps <- commitHistoryFirstParent c
                return (c:ps)

commitEntry ::
    Repository m => Commit m -> FilePath
    -> TreeRepository (Tree m) (Maybe (TreeEntry (TreeRepository (Tree m))))
commitEntry c path =
  flip lookupEntry path =<< resolveTree (commitTree c)

data PinnedEntry m = PinnedEntry
    { pinnedOid    :: Oid m
    , pinnedCommit :: Commit m
    , pinnedEntry  :: TreeEntry (TreeRepository (Tree m))
    }

identifyEntry :: Repository m => Commit m -> TreeEntry (TreeRepository (Tree m))
              -> m (PinnedEntry m)
identifyEntry co x = do
    oid <- case x of
        BlobEntry oid _ -> return (unTagged oid)
        TreeEntry ref   -> unTagged <$> treeRefOid ref
    return (PinnedEntry oid co x)

commitEntryHistory :: Repository m => Commit m -> FilePath -> m [PinnedEntry m]
commitEntryHistory c path =
    map head . filter (not . null) . groupBy ((==) `on` pinnedOid) <$> go c
  where
    go co = do
        entry <- getEntry co
        rest  <- case commitParents co of
            []    -> return []
            (p:_) -> go =<< resolveCommit p
        return $ maybe rest (:rest) entry

    getEntry co = do
        ce <- commitEntry co path
        case ce of
            Nothing  -> return Nothing
            Just ce' -> Just <$> identifyEntry co ce'

getCommitParents :: Repository m => Commit m -> m [Commit m]
getCommitParents = traverse resolveCommit . commitParents

-- Utils.hs ends here