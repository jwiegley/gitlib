module Git.Commit.Push where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Unlift
import           Control.Monad.Trans.Class
import           Data.Function
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Tagged
import           Data.Text (Text)
import           Data.Traversable (for)
import           Git.Commit
import           Git.Object
import           Git.Reference
import           Git.Repository
import           Git.Types
import           Prelude

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works no matter which two
--   backends are being used.  It should be considered a matter of last
--   resort, or for objects sets that are known to be small.
pushCommit :: (MonadGit r m, MonadGit s (t m), MonadTrans t)
           => CommitOid r -> Text -> t m (CommitOid s)
pushCommit coid remoteRefName = do
    commits <- mapM copyCommitOid =<< lift (listCommits Nothing coid)
    mrref   <- fmap Tagged `liftM` resolveReference remoteRefName
    mrref'  <- for mrref $ \rref ->
        if rref `elem` commits
        then lift $ copyCommitOid rref
        else throwM $ PushNotFastForward
                    $ "SHA " <> renderObjOid rref
                   <> " not found in remote"
    objs <- lift $ listAllObjects mrref' coid
    let shas = HashSet.fromList $ map (renderOid . untagObjOid) objs
    (cref,_) <- copyCommit coid Nothing shas
    unless (renderObjOid coid == renderObjOid cref) $
        throwM $ BackendError $ "Error copying commit: "
            <> renderObjOid coid <> " /= " <> renderObjOid cref
    -- jww (2013-04-18): This is something the user must decide to do
    -- updateReference_ remoteRefName (RefObj cref)
    return cref

copyRepository :: (MonadGit r m, MonadUnliftIO m,
                   MonadGit s (t m), MonadTrans t, MonadUnliftIO (t m))
                => RepositoryFactory (t m) m s
                -> Maybe (CommitOid r)
                -> Text
                -> FilePath
                -> Bool
                -> m ()
copyRepository factory mname refName gitDir isBare =
    withRepository' factory RepositoryOptions
        { repoPath       = gitDir
        , repoWorkingDir = Nothing
        , repoIsBare     = isBare
        , repoAutoCreate = True
        }
        (maybe (return ()) go mname)
  where
    go coid = do
        -- jww (2013-04-24): We don't need do download every object back to
        -- the first commit, but only the commits (and their objects) back to
        -- and including the common ancestor.  The question is, how do we
        -- determine the common ancestor before we've fetched all the contents
        -- of at least one side?
        cref <- pushCommit coid refName

        -- This will always be a fast-forward, since temp.git is empty.  The
        -- resulting HEAD will have the refname as the ref we want to push to
        -- or pull from, and no others.
        updateReference refName (RefObj (untag cref))
        updateReference "HEAD" (RefSymbolic refName)

        mref <- fmap renderOid <$> resolveReference refName
        unless (maybe False (renderObjOid coid ==) mref) $
            throwM (BackendError $
                    "Could not resolve destination reference '"
                    <> refName <> "'in project")
