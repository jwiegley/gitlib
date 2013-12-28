module Git.Working where

import Control.Applicative
import Control.Exception
import Control.Monad.IO.Class
import Control.Monad.Trans.Resource
import Data.Conduit
import Data.Conduit.List as CL
import Git.Blob
import Git.Types
import System.Directory
import System.FilePath

checkoutFiles :: (MonadGit r m, MonadBaseControl IO m, MonadIO m,
                  MonadResource m)
              => FilePath
              -> Tree r
              -> (TreeFilePath -> Either String FilePath)
              -> Bool
              -> m ()
checkoutFiles destPath tree decode cloneSubmodules =
    sourceTreeEntries tree $$ CL.mapM_ $ \(path, entry) -> case entry of
        BlobEntry oid kind -> case kind of
            SymlinkBlob ->
                error "jww (2013-12-27): checkoutFiles not yet implemented"
            _ -> do
                Blob _ contents <- lookupBlob oid
                -- jww (2013-12-26): There is no way to know what a tree's
                -- path has been encoded as.
                case (destPath </>) <$> decode path of
                    Left e -> liftIO $ throwIO (userError e)
                    Right fullPath -> do
                        liftIO $ createDirectoryIfMissing True
                            (takeDirectory fullPath)
                        writeBlob fullPath contents

        -- jww (2013-12-26): Recursively clone submodules?
        CommitEntry {} | cloneSubmodules -> do
            return ()

        _ -> return ()
