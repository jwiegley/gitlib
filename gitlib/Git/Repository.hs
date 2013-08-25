module Git.Repository where

import qualified Control.Exception.Lifted as Exc
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Data.Conduit
import           Git.Types
import           System.Directory
import           System.Mem

withNewRepository :: (Repository (t m), MonadGit (t m),
                      MonadBaseControl IO m, MonadIO m, MonadTrans t)
                  => RepositoryFactory t m c
                  -> FilePath -> t m a -> m a
withNewRepository factory path action = do
    liftIO $ do
        exists <- doesDirectoryExist path
        when exists $ removeDirectoryRecursive path

    -- we want exceptions to leave the repo behind
    a <- withRepository' factory (defaultOptions factory)
        { repoPath       = path
        , repoIsBare     = True
        , repoAutoCreate = True
        } action

    liftIO $ do
        exists <- doesDirectoryExist path
        when exists $ removeDirectoryRecursive path

    return a

withNewRepository' :: (Repository (t m), MonadGit (t m),
                       MonadBaseControl IO m, MonadIO m, MonadTrans t)
                   => RepositoryFactory t m c -> FilePath -> t m a -> m a
withNewRepository' factory path action =
    Exc.bracket_ recover recover $
        withRepository' factory (defaultOptions factory)
            { repoPath       = path
            , repoIsBare     = True
            , repoAutoCreate = True
            } action
  where
    recover = liftIO $ do
        exists <- doesDirectoryExist path
        when exists $ removeDirectoryRecursive path

withBackendDo :: (MonadIO m, MonadBaseControl IO m)
              => RepositoryFactory t m a -> m b -> m b
withBackendDo fact f = do
    startupBackend fact
    Exc.finally f (liftIO performGC >> shutdownBackend fact)

withRepository' :: (Repository (t m), MonadTrans t,
                    MonadBaseControl IO m, MonadIO m)
                => RepositoryFactory t m c
                -> RepositoryOptions
                -> t m a
                -> m a
withRepository' factory opts action =
    Exc.bracket
        (openRepository factory opts)
        (closeRepository factory)
        (flip (runRepository factory) action)

withRepository :: (Repository (t m), MonadTrans t,
                   MonadBaseControl IO m, MonadIO m)
               => RepositoryFactory t m c
               -> FilePath
               -> t m a
               -> m a
withRepository factory path =
    withRepository' factory
        (defaultOptions factory) { repoPath = path }
