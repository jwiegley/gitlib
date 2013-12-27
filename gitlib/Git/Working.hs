module Git.Working where

import           Control.Monad.IO.Class
import           Data.Conduit
import           Data.Conduit.List as CL
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Git.Blob
import           Git.Types
import           System.FilePath

checkoutFiles :: (MonadGit r m, MonadBaseControl IO m, MonadIO m,
                  MonadResource m)
              => FilePath -> Tree r -> Bool -> m ()
checkoutFiles destPath tree cloneSubmodules =
    sourceTreeEntries tree $$ CL.mapM_ $ \(path, entry) -> case entry of
        BlobEntry oid kind -> case kind of
            SymlinkBlob ->
                error "jww (2013-12-27): checkoutFiles not yet implemented"
            _ -> do
                Blob _ contents <- lookupBlob oid
                -- jww (2013-12-26): There is no way to know what a tree's
                -- path has been encoded as.
                writeBlob (destPath </> T.unpack (T.decodeUtf8 path))
                    contents

        -- jww (2013-12-26): Recursively clone submodules?
        CommitEntry {} | cloneSubmodules -> do
            return ()

        _ -> return ()
