module Git.Tree where

import           Control.Monad
import           Control.Monad.Catch
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Monoid
import           Data.Text (pack)
import           Git.Blob
import           Git.DSL
import           Git.Tree.Builder
import           Git.Types
import           Pipes
import qualified Pipes.Prelude as P

listTreeEntries :: Monad m => Tree r -> GitT r m [(TreeFilePath, TreeEntry r)]
listTreeEntries = P.toListM . allTreeEntries

copyTreeEntry :: (MonadThrow m, Repository r, Repository s)
              => TreeEntry s -> HashSet String
              -> GitT r (GitT s m) (TreeEntry r, HashSet String)
copyTreeEntry (BlobEntry oid kind) needed = do
    (b,needed') <- copyBlob oid needed
    unless (show oid == show b) $
        throwM $ BackendError $ "Error copying blob: "
            <> pack (show oid) <> " /= " <> pack (show b)
    return (BlobEntry b kind, needed')
copyTreeEntry (CommitEntry oid) needed = do
    coid <- parseOid (show oid)
    return (CommitEntry coid, needed)
copyTreeEntry (TreeEntry _) _ = error "This should never be called"

copyTree :: (MonadThrow m, Repository r, Repository s)
         => TreeOid s -> HashSet String
         -> GitT r (GitT s m) (TreeOid r, HashSet String)
copyTree oid needed = do
    let sha = show oid
    oid2 <- parseOid (show oid)
    if HashSet.member sha needed
        then do
        tree    <- lift $ lookupTree oid
        entries <- lift $ listTreeEntries tree
        (needed', tref) <-
            withNewTree $ foldM doCopyTreeEntry needed entries

        let x = HashSet.delete sha needed'
        return $ tref `seq` x `seq` (tref, x)

        else return (oid2, needed)
  where
    doCopyTreeEntry :: (MonadThrow m, Repository r, Repository s)
                    => HashSet String
                    -> (TreeFilePath, TreeEntry r)
                    -> TreeT s (GitT r m) (HashSet String)
    doCopyTreeEntry set (_, TreeEntry {}) = return set
    doCopyTreeEntry set (fp, ent) = do
        (ent2,set') <- liftGitT $ copyTreeEntry ent set
        putEntry fp ent2
        return set'
