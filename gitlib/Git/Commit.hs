module Git.Commit where

import           Conduit
import           Control.Monad
import           Data.Function
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import           Git.Tree
import           Git.Types
import           Prelude hiding (FilePath)

commitTreeEntry :: MonadGit r m
                => Commit r -> TreeFilePath -> m (Maybe (TreeEntry r))
commitTreeEntry c path = flip treeEntry path =<< lookupTree (commitTree c)

copyCommitOid :: (IsOid (Oid r), MonadGit s n)
              => CommitOid r -> n (CommitOid s)
copyCommitOid = parseObjOid . renderObjOid

copyCommit :: (MonadGit r m, MonadGit s (t m), MonadTrans t)
           => CommitOid r
           -> Maybe RefName
           -> HashSet Text
           -> t m (CommitOid s, HashSet Text)
copyCommit cr mref needed = do
    let oid = untag cr
        sha = renderOid oid
    commit <- lift $ lookupCommit cr
    oid2   <- parseOid sha
    if HashSet.member sha needed
        then do
        let parents = commitParents commit
        (parentRefs,needed') <- foldM copyParent ([],needed) parents
        (tr,needed'') <- copyTree (commitTree commit) needed'
        unless (renderObjOid (commitTree commit) == renderObjOid tr) $
            throwM $ BackendError $ "Error copying tree: "
                <> renderObjOid (commitTree commit)
                <> " /= " <> renderObjOid tr

        commit' <- createCommit (reverse parentRefs) tr
            (commitAuthor commit)
            (commitCommitter commit)
            (commitLog commit)
            mref

        let coid = commitOid commit'
            x    = HashSet.delete sha needed''
        return $ coid `seq` x `seq` (coid, x)

        else return (Tagged oid2, needed)
  where
    copyParent (prefs,needed') cref = do
        (cref2,needed'') <- copyCommit cref Nothing needed'
        unless (renderObjOid cref == renderObjOid cref2) $
            throwM $ BackendError $ "Error copying commit: "
                <> renderObjOid cref <> " /= " <> renderObjOid cref2
        let x = cref2 `seq` (cref2:prefs)
        return $ x `seq` needed'' `seq` (x,needed'')

listCommits :: MonadGit r m
            => Maybe (CommitOid r) -- ^ A commit we may already have
            -> CommitOid r         -- ^ The commit we need
            -> m [CommitOid r]     -- ^ All the objects in between
listCommits mhave need =
    sourceObjects mhave need False
        $= mapMC (\(CommitObjOid c) -> return c)
        $$ sinkList

traverseCommits :: MonadGit r m => (CommitOid r -> m a) -> CommitOid r -> m [a]
traverseCommits f need = mapM f =<< listCommits Nothing need

traverseCommits_ :: MonadGit r m => (CommitOid r -> m ()) -> CommitOid r -> m ()
traverseCommits_ = (void .) . traverseCommits
