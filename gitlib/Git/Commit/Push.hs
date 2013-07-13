module Git.Commit.Push where

import           Control.Failure
import           Control.Monad
import           Control.Monad.Trans.Class
import           Data.Function
import qualified Data.HashSet as HashSet
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import           Git.Commit
import           Git.Reference
import           Git.Object
import           Git.Types
import           Prelude hiding (FilePath)

-- | Fast-forward push a reference between repositories using a recursive
--   copy.  This can be extremely slow, but always works no matter which two
--   backends are being used.  It should be considered a matter of last
--   resort, or for objects sets that are known to be small.
pushCommit :: (Repository m, Repository (t m), MonadTrans t)
           => CommitOid m -> Text -> t m (CommitOid (t m))
pushCommit coid remoteRefName = do
    mrref <- resolveReference remoteRefName
    commits1 <- mapM copyCommitOid =<< lift (listCommits Nothing coid)
    fastForward <- case mrref of
        Nothing -> return (Just Nothing)
        Just rref
            | rref `elem` commits1 -> do
                roid <- lift $ copyCommitOid rref
                return $ Just (Just roid)
            | otherwise -> do
                failure (PushNotFastForward $
                         "SHA " <> renderObjOid rref
                                <> " not found in remote")
    case fastForward of
        Nothing -> failure (PushNotFastForward "unexpected")
        Just rref -> do
            objs <- lift $ listAllObjects rref coid
            shas <- mapM (return . renderOid . untagObjOid) objs
            (cref,_) <- copyCommit coid Nothing (HashSet.fromList shas)
            -- jww (2013-04-18): This is something the user must
            -- decide to do
            -- updateReference_ remoteRefName (RefObj cref)
            return cref
