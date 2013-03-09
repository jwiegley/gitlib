{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}

module Git.Utils where

import           Control.Applicative
import           Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as CList
import           Data.Function
import           Data.List
import           Data.Monoid
import           Data.Proxy
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Traversable hiding (mapM, sequence)
import           Filesystem (removeTree, isDirectory)
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

commitTreeEntry :: Repository m
                => Commit m
                -> FilePath
                -> TreeRepository (Tree m)
                      (Maybe (TreeEntry (TreeRepository (Tree m))))
commitTreeEntry c path = flip lookupEntry path =<< resolveTree (commitTree c)

copyOid :: (Repository m1, Repository m2) => Oid m1 -> m1 (m2 (Oid m2))
copyOid = return . parseOid . renderOid

copyBlob :: (Repository m1, Repository m2) => BlobRef m1 -> m1 (m2 (BlobOid m2))
copyBlob blobr = do
    let oid = unTagged (blobRefOid blobr)
    exists <- existsObject oid
    if exists
        then do oid' <- copyOid oid
                return (Tagged <$> oid')
        else createBlob . BlobString
             <$> (blobToByteString =<< resolveBlob blobr)

copyTree :: (Repository m1, Repository m2) => TreeRef m1 -> m1 (m2 (Tree m2))
copyTree treer = do
    oid    <- unTagged <$> treeRefOid treer
    exists <- existsObject oid
    if exists
        then do oid' <- copyOid oid
                return $ lookupTree =<< (Tagged <$> oid')
        else return undefined           -- jww (2013-03-09): NYI

copyCommit :: (Repository m1, Repository m2)
           => CommitRef m1
           -> Maybe Text
           -> m1 (m2 (Commit m2))
copyCommit cr mref = do
    let oid = unTagged (commitRefOid cr)
    exists <- existsObject oid
    if exists
        then do oid' <- copyOid oid
                return $ lookupCommit =<< (Tagged <$> oid')
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

genericPushRef :: (Repository m1, Repository m2)
               => Reference m1 (Commit m1)
               -> (Text,Text)
               -> Text
               -> m1 (m2 (Maybe (Reference m2 (Commit m2))))
genericPushRef = undefined              -- jww (2013-03-09): NYI

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
        ce <- commitTreeEntry co path
        case ce of
            Nothing  -> return Nothing
            Just ce' -> Just <$> identifyEntry co ce'

getCommitParents :: Repository m => Commit m -> m [Commit m]
getCommitParents = traverse resolveCommit . commitParents

withNewRepository :: (Repository r, MonadIO r, RepositoryFactoryT r IO)
                  => Proxy (r ()) -> FilePath -> r a -> IO a
withNewRepository _ dir action = do
  exists <- isDirectory dir
  when exists $ removeTree dir

  a <- withRepository (Tagged dir :: Tagged (r ()) FilePath) True action
  -- we want exceptions to leave the repo behind

  exists <- isDirectory dir
  when exists $ removeTree dir

  return a

withExistingRepository :: (Repository r, MonadIO r, RepositoryFactoryT r IO)
                       => Proxy (r ()) -> FilePath -> r a -> IO a
withExistingRepository _ dir act =
    withRepository (Tagged dir :: Tagged (r ()) FilePath) True act

sampleCommit :: Repository m => Tree m -> Signature -> m (Commit m)
sampleCommit tr sig =
    createCommit [] (treeRef tr) sig sig "Sample log message.\n" Nothing

-- Utils.hs ends here