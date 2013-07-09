module Git.Tree where

import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Function
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.Tagged
import           Data.Text (Text)
import           Git.Blob
import           Git.Tree.Builder
import           Git.Types
import           Prelude hiding (FilePath)

copyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
              => TreeEntry m
              -> HashSet Text
              -> t m (TreeEntry (t m), HashSet Text)
copyTreeEntry (BlobEntry oid kind) needed = do
    (b,needed') <- copyBlob oid needed
    return (BlobEntry b kind, needed')
copyTreeEntry (CommitEntry oid) needed = do
    coid <- parseOid (renderObjOid oid)
    return (CommitEntry (Tagged coid), needed)
copyTreeEntry (TreeEntry _) _ = error "This should never be called"

copyTree :: (Repository m, Repository (t m), MonadTrans t)
         => TreeOid m
         -> HashSet Text
         -> t m (TreeOid (t m), HashSet Text)
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
    doCopyTreeEntry :: (Repository m, Repository (t m), MonadTrans t)
                    => HashSet Text
                    -> (Text, TreeEntry m)
                    -> TreeT (t m) (HashSet Text)
    doCopyTreeEntry set (_, TreeEntry {}) = return set
    doCopyTreeEntry set (fp, ent) = do
        (ent2,set') <- lift $ copyTreeEntry ent set
        putEntry fp ent2
        return set'
