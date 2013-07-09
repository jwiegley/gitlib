module Git.Misc where

-- commitHistoryFirstParent :: Repository m => Commit m -> m [Commit m]
-- commitHistoryFirstParent c =
--     case commitParents c of
--         []    -> return [c]
--         (p:_) -> do ps <- commitHistoryFirstParent =<< lookupCommit p
--                     return (c:ps)

-- data PinnedEntry m = PinnedEntry
--     { pinnedOid    :: Oid m
--     , pinnedCommit :: Commit m
--     , pinnedEntry  :: TreeEntry m
--     }

-- identifyEntry :: Repository m => Commit m -> TreeEntry m -> m (PinnedEntry m)
-- identifyEntry co x = do
--     let oid = case x of
--             BlobEntry oid' _ -> untag oid'
--             TreeEntry oid'   -> untag oid'
--             CommitEntry oid' -> untag oid'
--     return (PinnedEntry oid co x)

-- commitEntryHistory :: Repository m => Commit m -> FilePath -> m [PinnedEntry m]
-- commitEntryHistory c path =
--     map head . filter (not . null) . groupBy ((==) `on` pinnedOid) <$> go c
--   where
--     go co = do
--         entry <- getCommitTreeEntry co
--         rest  <- case commitParents co of
--             []    -> return []
--             (p:_) -> go =<< lookupCommit p
--         return $ maybe rest (:rest) entry

--     getCommitTreeEntry co = do
--         ce <- commitTreeEntry co path
--         case ce of
--             Nothing  -> return Nothing
--             Just ce' -> Just <$> identifyEntry co ce'
