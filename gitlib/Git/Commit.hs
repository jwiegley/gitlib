{-# LANGUAGE OverloadedStrings #-}

module Git.Commit where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Class
import           Data.Function
import           Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (pack)
import           Git.DSL
import           Git.Tree
import           Git.Types
import qualified Streaming.Prelude as S
import           Prelude hiding (FilePath)

commitTreeEntry :: Monad m
                => Commit r -> TreeFilePath -> GitT r m (Maybe (TreeEntry r))
commitTreeEntry c path = do
    Just t <- lookupTree (commitTree c)
    getTreeEntry t path

copyCommit :: (MonadThrow m, Repository r, Repository s)
           => CommitOid s
           -> Maybe RefName
           -> HashSet String
           -> GitT r (GitT s m) (CommitOid r, HashSet String)
copyCommit oid mref needed = do
    let sha = show oid
    Just commit <- lift $ lookupCommit oid
    oid2 <- parseOid sha
    if HashSet.member sha needed
        then do
        let parents = commitParents commit
        (parentRefs,needed') <- foldM copyParent ([],needed) parents
        (tr,needed'') <- copyTree (commitTree commit) needed'
        unless (show (commitTree commit) == show tr) $
            throwM $ BackendError $ "Error copying tree: "
                <> pack (show (commitTree commit))
                <> " /= " <> pack (show tr)

        c <- commitObj (reverse parentRefs) tr
            (commitAuthor commit)
            (commitCommitter commit)
            (commitLog commit) "UTF-8" -- jww (2015-06-16): ?
        coid <- createCommit c mref

        let x = HashSet.delete sha needed''
        return $ coid `seq` x `seq` (coid, x)

        else return (oid2, needed)
  where
    copyParent (prefs,needed') cref = do
        (cref2,needed'') <- copyCommit cref Nothing needed'
        unless (show cref == show cref2) $
            throwM $ BackendError $ "Error copying commit: "
                <> pack (show cref) <> " /= " <> pack (show cref2)
        let x = cref2 `seq` (cref2:prefs)
        return $ x `seq` needed'' `seq` (x,needed'')

listCommits :: Monad m
            => Maybe (CommitOid r) -- ^ A commit we may already have
            -> CommitOid r         -- ^ The commit we need
            -> GitT r m [CommitOid r]     -- ^ All the objects in between
listCommits mhave need =
    S.toList_ $ S.mapM (\(CommitObjOid c) -> return c)
              $ allObjects mhave need False

traverseCommits :: Monad m
                => (CommitOid r -> GitT r m a) -> CommitOid r -> GitT r m [a]
traverseCommits f need = mapM f =<< listCommits Nothing need

traverseCommits_ :: Monad m
                 => (CommitOid r -> GitT r m ()) -> CommitOid r -> GitT r m ()
traverseCommits_ = (void .) . traverseCommits
