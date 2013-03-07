{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

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
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable hiding (mapM, sequence)
import           Filesystem.Path.CurrentOS hiding (null)
import           Git
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

oid :: Treeish t => t -> TreeRepository t Text
oid t = renderObjOid <$> writeTree t

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

blobContentsToByteString :: Repository m => BlobContents m -> m ByteString
blobContentsToByteString (BlobString bs) = return bs
blobContentsToByteString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobContentsToByteString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

blobToByteString :: Repository m => Blob m -> m ByteString
blobToByteString (Blob _ contents) = blobContentsToByteString contents

treeBlobEntries :: Repository m => Tree m -> m [(FilePath,TreeEntry m)]
treeBlobEntries tree =
    mconcat <$> traverseEntries tree (\fp e -> case e of
                                           BlobEntry {} -> return [(fp,e)]
                                           _ -> return [])

commitEntry :: Repository m
            => Commit m
            -> FilePath
            -> TreeRepository (Tree m)
                (Maybe (TreeEntry (TreeRepository (Tree m))))
commitEntry c path = flip lookupEntry path =<< resolveTree (commitTree c)

copyOid :: (Repository m1, Repository m2) => Oid m1 -> m1 (m2 (Oid m2))
copyOid oid = do
    let text = renderOid oid
    return $ parseOid text

copyBlob :: (Repository m1, Repository m2) => BlobRef m1 -> m1 (m2 (BlobOid m2))
copyBlob blobr = do
    let oid = unTagged (blobRefOid blobr)
    exists <- existsObject oid
    if exists
        then do
        oid' <- copyOid oid
        return (Tagged <$> oid')
        else do
        blob <- resolveBlob blobr
        contents <- blobToByteString blob
        return (createBlob (BlobString contents))

copyTree :: (Repository m1, Repository m2) => TreeRef m1 -> m1 (m2 (Tree m2))
copyTree treer = do
    oid    <- unTagged <$> treeRefOid treer
    exists <- existsObject oid
    if exists
        then do
        oid' <- copyOid oid
        return (lookupTree =<< (Tagged <$> oid'))
        else do
        return undefined

copyCommit :: (Repository m1, Repository m2)
           => CommitRef m1
           -> Maybe Text
           -> m1 (m2 (Commit m2))
copyCommit cr mref = do
    let oid = unTagged (commitRefOid cr)
    exists <- existsObject oid
    if exists
        then do
        oid' <- copyOid oid
        return (lookupCommit =<< (Tagged <$> oid'))
        else do
        commit  <- resolveCommit cr
        parents <- mapM (flip copyCommit Nothing) (commitParents commit)
        tree    <- copyTree (commitTree commit)
        return $ do
            parents' <- sequence parents
            tree'    <- tree
            createCommit
                (map commitRef parents')
                (treeRef tree')
                (commitAuthor commit)
                (commitCommitter commit)
                (commitLog commit)
                mref

commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
commitHistoryFirstParent c =
  case commitParents c of
    []    -> return [c]
    (p:_) -> do ps <- commitHistoryFirstParent c
                return (c:ps)

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