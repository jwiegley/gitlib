{-# LANGUAGE CPP #-}

module Git.Working where

import Conduit
import Data.Text as T
import Git.Blob
import Git.Types
import System.Directory
import System.FilePath
#if !mingw32_HOST_OS
import System.Posix.Files
#endif

checkoutFiles :: (MonadGit r m, MonadResource m)
              => FilePath
              -> Tree r
              -> (TreeFilePath -> Either String FilePath)
              -> Bool
              -> m ()
checkoutFiles destPath tree decode cloneSubmodules =
    runConduit $ sourceTreeEntries tree .| (mapM_C $ \(path, entry) ->
        case (destPath </>) <$> decode path of
            Left e ->  decodeError path e
            Right fullPath -> do
                liftIO $ createDirectoryIfMissing True (takeDirectory fullPath)
                case entry of
                    TreeEntry {} -> return ()
                    BlobEntry oid kind -> checkoutBlob oid kind fullPath
                    CommitEntry oid
                        -- jww (2013-12-26): Recursively clone submodules?
                        | cloneSubmodules -> cloneSubmodule oid fullPath
                        | otherwise -> liftIO $ createDirectory fullPath)
  where
    decodeError path e = throwM $ PathEncodingError $
        "Could not decode path " <> T.pack (show path) <> ":" <> T.pack e

    checkoutBlob oid kind fullPath = do
        Blob _ contents <- lookupBlob oid
        case kind of
#if !mingw32_HOST_OS
            SymlinkBlob -> do
                target <- blobContentsToByteString contents
                case decode target of
                    Left e -> decodeError target e
                    Right targetPath ->
                        liftIO $ createSymbolicLink targetPath fullPath
#endif
            _ -> do                  -- PlainBlob | ExecutableBlob
                -- jww (2013-12-26): There is no way to know what a tree's
                -- path has been encoded as.
                writeBlob fullPath contents

    cloneSubmodule =
        error "jww (2013-12-29): Cloning submodule is not yet implemented"
