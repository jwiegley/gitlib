{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}

module Git.Tree.Working where

import           Control.Applicative
import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe
import qualified Data.ByteString.Streaming as B
import           Data.Foldable (foldl')
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.Maybe
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Git
import           Prelude hiding (log)
import           Streaming
import           System.FilePath.Posix
import           System.Posix.Files

data FileEntry m = FileEntry
    { fileModTime  :: UTCTime
    , fileBlobOid  :: BlobOid m
    , fileBlobKind :: BlobKind
    , fileChecksum :: BlobOid m
    }

type FileTree m = HashMap TreeFilePath (FileEntry m)

readFileTree :: (MonadResource m, MonadIO m)
             => RefName -> FilePath -> Bool -> GitT r m (Maybe (FileTree r))
readFileTree ref wdir getHash = do
    h <- resolveReference ref
    case h of
        Nothing -> pure $ Just Map.empty
        Just h' -> runMaybeT $ do
            c <- MaybeT $ lookupCommit h'
            tr <- MaybeT $ lookupTree (commitTree c)
            lift $ readFileTree' tr wdir getHash

readFileTree' :: (MonadResource m, MonadIO m)
              => Tree r -> FilePath -> Bool -> GitT r m (FileTree r)
readFileTree' tr wdir getHash = do
    blobs <- treeBlobEntries tr
    stats <- mapM go blobs
    return $ foldl' (\m (!fp,!fent) -> maybe m (flip (Map.insert fp) m) fent)
        Map.empty stats
  where
    go (!fp,!oid,!kind) = do
        fent <- readModTime wdir getHash fp oid kind
        fent `seq` return (fp,fent)

readModTime :: (MonadResource m, MonadIO m)
            => FilePath -> Bool -> TreeFilePath -> BlobOid r -> BlobKind
            -> GitT r m (Maybe (FileEntry r))
readModTime wdir getHash (T.unpack . T.decodeUtf8 -> fp) oid kind = do
    let path = wdir </> fp
    -- debug' $ pack $ "Checking file: " ++ path
    estatus <- liftIO $ try $ getSymbolicLinkStatus path
    case (estatus :: Either SomeException FileStatus) of
        Right status | isRegularFile status -> do
            contents <- B.toLazy_ (B.readFile path)
            Just <$> FileEntry
                (posixSecondsToUTCTime (realToFrac (modificationTime status)))
                oid kind
                <$> if getHash
                    then hashContents contents
                    else return oid
        _ -> return Nothing
