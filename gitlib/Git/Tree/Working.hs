module Git.Tree.Working where

import           Control.Applicative
import           Control.Concurrent.Async.Lifted
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Control
import qualified Data.ByteString as B (readFile)
import qualified Data.ByteString.Char8 as B8
import           Data.Foldable (foldl')
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import           Data.Tagged
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Git hiding (Options)
import           Prelude hiding (log)
import           System.FilePath.Posix
import           System.Posix.Files

data FileEntry m = FileEntry
    { fileModTime  :: UTCTime
    , fileBlobOid  :: BlobOid m
    , fileBlobKind :: BlobKind
    , fileChecksum :: BlobOid m
    }

type FileTree m = HashMap TreeFilePath (FileEntry m)

readFileTree :: (MonadBaseControl IO m, MonadIO m, MonadGit r m)
             => RefName
             -> FilePath
             -> Bool
             -> m (FileTree r)
readFileTree ref wdir getHash = do
    h <- resolveReference ref
    case h of
        Nothing -> pure Map.empty
        Just h' -> do
            tr <- lookupTree . commitTree =<< lookupCommit (Tagged h')
            readFileTree' tr wdir getHash

readFileTree' :: (MonadBaseControl IO m, MonadIO m, MonadGit r m)
              => Tree r -> FilePath -> Bool
              -> m (FileTree r)
readFileTree' tr wdir getHash = do
    blobs <- treeBlobEntries tr
    stats <- mapConcurrently go blobs
    return $ foldl' (\m (!fp,!fent) -> maybe m (flip (Map.insert fp) m) fent)
        Map.empty stats
  where
    go (!fp,!oid,!kind) = do
        fent <- readModTime wdir getHash (B8.unpack fp) oid kind
        fent `seq` return (fp,fent)

readModTime :: (MonadIO m, MonadGit r m)
            => FilePath
            -> Bool
            -> FilePath
            -> BlobOid r
            -> BlobKind
            -> m (Maybe (FileEntry r))
readModTime wdir getHash fp oid kind = do
    let path = wdir </> fp
    -- debug' $ pack $ "Checking file: " ++ path
    estatus <- liftIO $ try $ getSymbolicLinkStatus path
    case (estatus :: Either SomeException FileStatus) of
        Right status | isRegularFile status ->
            Just <$> (FileEntry
                          <$> pure (posixSecondsToUTCTime
                                    (realToFrac (modificationTime status)))
                          <*> pure oid
                          <*> pure kind
                          <*> if getHash
                              then hashContents . BlobString
                                  =<< liftIO (B.readFile path)
                              else return oid)
        _ -> return Nothing
