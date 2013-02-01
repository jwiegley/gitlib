{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

module Git.Utils where

import           Control.Applicative
import           Control.Exception as Exc
import           Control.Failure
import           Control.Monad
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.Conduit
import qualified Data.Conduit.List as CList
-- import           Data.Hex
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Git
import           System.IO.Unsafe

oid :: Treeish t => t -> TreeRepository Text
oid t = do
    oid' <- writeTree t
    case oid' of
        Nothing -> return (T.pack "<none>")
        Just x  -> return (renderOid x)

createBlobUtf8 :: Repository m => Text -> m (BlobOid m)
createBlobUtf8 = createBlob . BlobString . T.encodeUtf8

catBlob :: Repository m => Text -> m ByteString
catBlob str = do
    if len == 40
        then do
        oid <- parseOid str
        lookupBlob (Tagged oid) >>= blobToByteString

        else do
        obj <- lookupObject str
        case obj of
            BlobObj (ByOid oid) -> lookupBlob oid >>= blobToByteString
            BlobObj (Known x)   -> blobToByteString x
            _ -> failure (ObjectLookupFailed str len)
  where
    len = T.length str

catBlobUtf8 :: Repository m => Text -> m Text
catBlobUtf8 = catBlob >=> return . T.decodeUtf8

blobToByteString :: Repository m => BlobContents m -> m ByteString
blobToByteString (BlobString bs) = return bs
blobToByteString (BlobStream bs) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)
blobToByteString (BlobSizedStream bs _) = do
    strs <- bs $$ CList.consume
    return (B.concat strs)

-- -- | Parse an ASCII hex string into a Git 'Oid'.
-- parseOidToByteString :: Text -> Maybe ByteString
-- parseOidToByteString oid
--     | T.length oid /= 40 = Nothing
--     | otherwise =
--         -- 'unsafePerformIO' is used to force 'unhex' to run in the 'IO'
--         -- monad, so we can catch the exception on failure and repackage it
--         -- using 'Maybe'.  Why does 'unhex' have to be in IO at all?
--         unsafePerformIO $
--         Exc.catch (Just <$> unhex (T.encodeUtf8 oid))
--                   (\x -> (x :: Exc.IOException) `seq` return Nothing)

-- commitHistoryFirstParent :: Commit -> LgRepository [Commit]
-- commitHistoryFirstParent c =
--   case commitParents c of
--     []    -> return [c]
--     (p:_) -> do p' <- loadObject' p c
--                 ps <- commitHistoryFirstParent p'
--                 return (c:ps)

-- commitEntry :: Commit -> FilePath -> LgRepository (Maybe TreeEntry)
-- commitEntry c path =
--   flip lookupTreeEntry path =<< loadObject' (commitTree c) c

-- data PinnedEntry = PinnedEntry { pinnedOid    :: Oid
--                                , pinnedCommit :: Commit
--                                , pinnedEntry  :: TreeEntry }
--                  deriving Show

-- identifyEntry :: Commit -> TreeEntry -> LgRepository PinnedEntry
-- identifyEntry co x = do
--   oid <- case x of
--           BlobEntry blob _ -> objectRefId blob
--           TreeEntry tree   -> objectRefId tree
--   return (PinnedEntry oid co x)

-- commitEntryHistory :: Commit -> FilePath -> LgRepository [PinnedEntry]
-- commitEntryHistory c path = map head
--                             . filter (not . null)
--                             . groupBy ((==) `on` pinnedOid) <$> go c
--   where go co = do
--           entry <- getEntry co
--           rest  <- case commitParents co of
--             []    -> return []
--             (p:_) -> go =<< loadObject' p co
--           return $ maybe rest (:rest) entry

--         getEntry co = do
--           ce <- commitEntry co path
--           case ce of
--             Nothing  -> return Nothing
--             Just ce' -> Just <$> identifyEntry co ce'

-- getCommitParents :: Commit -> LgRepository [Commit]
-- getCommitParents c =
--     traverse (\p -> do parent <- loadObject p c
--                        case parent of
--                            Nothing -> error "Cannot find Git commit"
--                            Just p' -> return p')
--              (lgCommitParents c)

-- Utils.hs ends here