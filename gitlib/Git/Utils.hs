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

