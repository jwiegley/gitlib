module Git.Tree where

import           Conduit
import           Control.Failure
import           Control.Monad
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import           Git.Blob
import           Git.Tree.Builder
import           Git.Types

listTreeEntries :: MonadGit r m => Tree r -> m [(TreeFilePath, TreeEntry r)]
listTreeEntries tree = sourceTreeEntries tree $$ sinkList

copyTreeEntry :: (MonadGit r m, MonadGit s (t m), MonadTrans t)
              => TreeEntry r -> HashSet Text -> t m (TreeEntry s, HashSet Text)
copyTreeEntry (BlobEntry oid kind) needed = do
    (b,needed') <- copyBlob oid needed
    unless (renderObjOid oid == renderObjOid b) $
        failure $ BackendError $ "Error copying blob: "
            <> renderObjOid oid <> " /= " <> renderObjOid b
    return (BlobEntry b kind, needed')
copyTreeEntry (CommitEntry oid) needed = do
    coid <- parseOid (renderObjOid oid)
    return (CommitEntry (Tagged coid), needed)
copyTreeEntry (TreeEntry _) _ = error "This should never be called"

copyTree :: (MonadGit r m, MonadGit s (t m), MonadTrans t)
         => TreeOid r -> HashSet Text -> t m (TreeOid s, HashSet Text)
copyTree tr needed = do
    let oid = untag tr
        sha = renderOid oid
    oid2 <- parseOid (renderOid oid)
    if HashSet.member sha needed
        then do
        tree    <- lift $ lookupTree tr
        entries <- lift $ listTreeEntries tree
        (needed', tref) <-
            withNewTree $ foldM doCopyTreeEntry needed entries

        let x = HashSet.delete sha needed'
        return $ tref `seq` x `seq` (tref, x)

        else return (Tagged oid2, needed)
  where
    doCopyTreeEntry :: (MonadGit r m, MonadGit s (t m), MonadTrans t)
                    => HashSet Text
                    -> (TreeFilePath, TreeEntry r)
                    -> TreeT s (t m) (HashSet Text)
    doCopyTreeEntry set (_, TreeEntry {}) = return set
    doCopyTreeEntry set (fp, ent) = do
        (ent2,set') <- lift $ copyTreeEntry ent set
        putEntry fp ent2
        return set'
