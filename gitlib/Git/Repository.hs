module Git.Repository where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Git.Types
import System.Directory
import UnliftIO.Exception

withNewRepository :: (MonadGit r n, MonadUnliftIO n, MonadUnliftIO m)
                  => RepositoryFactory n m r
                  -> FilePath -> n a -> m a
withNewRepository factory path action = do
    liftIO $ do
        exists <- doesDirectoryExist path
        when exists $ removeDirectoryRecursive path

    -- we want exceptions to leave the repo behind
    a <- withRepository' factory RepositoryOptions
        { repoPath       = path
        , repoWorkingDir = Nothing
        , repoIsBare     = True
        , repoAutoCreate = True
        } action

    liftIO $ do
        exists <- doesDirectoryExist path
        when exists $ removeDirectoryRecursive path

    return a

withNewRepository' :: (MonadGit r n, MonadUnliftIO n, MonadUnliftIO m)
                   => RepositoryFactory n m r -> FilePath -> n a -> m a
withNewRepository' factory path action =
    bracket_ recover recover $
        withRepository' factory RepositoryOptions
            { repoPath       = path
            , repoWorkingDir = Nothing
            , repoIsBare     = True
            , repoAutoCreate = True
            } action
  where
    recover = liftIO $ do
        exists <- doesDirectoryExist path
        when exists $ removeDirectoryRecursive path

withRepository' :: (MonadGit r n, MonadUnliftIO n, MonadUnliftIO m)
                => RepositoryFactory n m r -> RepositoryOptions -> n a -> m a
withRepository' factory opts action = do
    repo <- openRepository factory opts
    runRepository factory repo $ action `finally` closeRepository

withRepository :: (MonadGit r n, MonadUnliftIO n, MonadUnliftIO m)
               => RepositoryFactory n m r -> FilePath -> n a -> m a
withRepository factory path =
    withRepository' factory defaultRepositoryOptions { repoPath = path }
