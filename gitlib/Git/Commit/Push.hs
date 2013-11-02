module Git.Commit.Push where

import           Control.Failure
import           Control.Monad
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
import           Git.Types
import           Prelude hiding (FilePath)

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works no matter which two
--   backends are being used.  It should be considered a matter of last
--   resort, or for objects sets that are known to be small.
pushCommit :: (Repository m, Repository (t m), MonadTrans t)
           => CommitOid m -> Text -> t m (CommitOid (t m))
pushCommit coid remoteRefName = do
    commits <- mapM copyCommitOid =<< lift (listCommits Nothing coid)
    mrref   <- fmap Tagged `liftM` resolveReference remoteRefName
    mrref'  <- for mrref $ \rref ->
        if rref `elem` commits
        then lift $ copyCommitOid rref
        else failure $ PushNotFastForward
                     $ "SHA " <> renderObjOid rref
                    <> " not found in remote"
    objs <- lift $ listAllObjects mrref' coid
    let shas = HashSet.fromList $ map (renderOid . untagObjOid) objs
    (cref,_) <- copyCommit coid Nothing shas
    unless (renderObjOid coid == renderObjOid cref) $
        failure $ BackendError $ "Error copying commit: "
            <> renderObjOid coid <> " /= " <> renderObjOid cref
    -- jww (2013-04-18): This is something the user must decide to do
    -- updateReference_ remoteRefName (RefObj cref)
    return cref
