{-# LANGUAGE OverloadedStrings #-}

module Git.Commit.Push where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Trans.Class
import           Data.Function
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text, pack)
import           Data.Traversable (for)
import           Git.Commit
import           Git.DSL
import           Git.Object
import           Git.Reference
import           Git.Types
import           Prelude

copyCommitOid :: (Monad m, Repository r, Repository s)
              => CommitOid s -> GitT r m (CommitOid r)
copyCommitOid = parseOid . show

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works no matter which two
--   backends are being used.  It should be considered a matter of last
--   resort, or for objects sets that are known to be small.
pushCommit :: (MonadThrow m, Repository r, Repository s)
           => CommitOid s -> Text -> GitT r (GitT s m) (CommitOid r)
pushCommit coid remoteRefName = do
    commits <- mapM copyCommitOid =<< lift (listCommits Nothing coid)
    mrref   <- resolveReference remoteRefName
    mrref'  <- for mrref $ \rref ->
        if rref `elem` commits
        then lift $ copyCommitOid rref
        else throwM $ PushNotFastForward
                    $ "SHA " <> pack (show rref)
                   <> " not found in remote"
    objs <- lift $ listAllObjects mrref' coid
    let shas = HashSet.fromList $ map show objs
    (cref,_) <- copyCommit coid Nothing shas
    unless (show coid == show cref) $
        throwM $ BackendError $ "Error copying commit: "
            <> pack (show coid) <> " /= " <> pack (show cref)
    -- jww (2013-04-18): This is something the user must decide to do
    -- updateReference_ remoteRefName (RefObj cref)
    return cref

copyRepository :: (MonadThrow m, Repository r, Repository s)
                => Maybe (CommitOid s)
                -> Text
                -> GitT r (GitT s m) ()
copyRepository Nothing _ = return ()
copyRepository (Just coid) refName = do
    -- jww (2013-04-24): We don't need do download every object back to
    -- the first commit, but only the commits (and their objects) back to
    -- and including the common ancestor.  The question is, how do we
    -- determine the common ancestor before we've fetched all the contents
    -- of at least one side?
    cref <- pushCommit coid refName

    -- This will always be a fast-forward, since temp.git is empty.  The
    -- resulting HEAD will have the refname as the ref we want to push to
    -- or pull from, and no others.
    createReference refName (RefObj cref)
    createReference "HEAD" (RefSymbolic refName)

    mref <- fmap show <$> resolveReference refName
    unless (maybe False (show coid ==) mref) $
        throwM (BackendError $
                "Could not resolve destination reference '"
                <> refName <> "'in project")
