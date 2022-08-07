{-# OPTIONS_GHC -fno-warn-orphans #-}

module Git.Tree.Builder
       ( TreeT
       , TreeBuilder(..)
       , ModifiedBuilder(..)
       , createTree
       , withNewTree
       , mutateTree
       , mutateTreeOid
       , currentTree
       , currentTreeOid
       , withTree
       , withTreeOid
       , dropEntry
       , getEntry
       , putBlob
       , putBlob'
       , putCommit
       , putEntry
       , putTree
       , treeEntry
       , ModifyTreeResult(..)
       , fromModifyTreeResult
       , toModifyTreeResult
       , emptyTreeId
       ) where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import qualified Data.ByteString as B
import           Data.Char
import qualified Data.HashMap.Strict as HashMap
import           Data.Text (Text)
import           Data.Word
import           Git.Types

data ModifyTreeResult r = TreeEntryNotFound
                        | TreeEntryDeleted
                        | TreeEntryPersistent (TreeEntry r)
                        | TreeEntryMutated (TreeEntry r)

fromModifyTreeResult :: ModifyTreeResult r -> Maybe (TreeEntry r)
fromModifyTreeResult TreeEntryNotFound       = Nothing
fromModifyTreeResult TreeEntryDeleted        = Nothing
fromModifyTreeResult (TreeEntryPersistent x) = Just x
fromModifyTreeResult (TreeEntryMutated x)    = Just x

toModifyTreeResult :: (TreeEntry r -> ModifyTreeResult r)
                   -> Maybe (TreeEntry r)
                   -> ModifyTreeResult r
toModifyTreeResult _ Nothing  = TreeEntryNotFound
toModifyTreeResult f (Just x) = f x

instance Functor m => Functor (TreeT r m) where
    fmap f (TreeT t) = TreeT (fmap f t)

instance Monad m => Monad (TreeT r m) where
    return x = TreeT (return x)
    TreeT x >>= f = TreeT (x >>= runTreeT . f)

instance (Functor m, Monad m) => Applicative (TreeT r m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (TreeT r m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus m) => MonadPlus (TreeT r m) where
    mzero       = TreeT mzero
    m `mplus` n = TreeT $ runTreeT m `mplus` runTreeT n

instance (MonadFix m) => MonadFix (TreeT r m) where
    mfix f = TreeT $ mfix $ \ ~a -> runTreeT (f a)

instance MonadTrans (TreeT r) where
    lift m = TreeT $ lift m

instance (MonadIO m) => MonadIO (TreeT r m) where
    liftIO = lift . liftIO

getBuilder :: Monad m => TreeT r m (TreeBuilder r m)
getBuilder = TreeT get

putBuilder :: Monad m => TreeBuilder r m -> TreeT r m ()
putBuilder = TreeT . put

data BuilderAction = GetEntry | PutEntry | DropEntry
    deriving (Eq, Show)

emptyTreeId :: Text
emptyTreeId = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

-- | Perform a query action on a TreeBuilder using the supplied action kind
--   and user function.
--
--   This is a complex algorithm which has been rewritten many times, so I
--   will try to guide you through it as best I can.
queryTreeBuilder :: MonadGit r m
                 => TreeBuilder r m
                 -> TreeFilePath
                 -> BuilderAction
                 -> (Maybe (TreeEntry r) -> ModifyTreeResult r)
                 -> m (TreeBuilder r m, Maybe (TreeEntry r))
queryTreeBuilder builder path kind f = do
    (mtb, mtresult) <- walk (BuilderUnchanged builder) (splitDirectories path)
    return (fromBuilderMod mtb, fromModifyTreeResult mtresult)
  where
    walk _ [] = error "queryTreeBuilder called without a path"
    walk bm (name:names) = do
        let tb = fromBuilderMod bm
        y <- case HashMap.lookup name (mtbPendingUpdates tb) of
            Just x  -> return $ Left (BuilderUnchanged x)
            Nothing -> do
                mentry <- mtbLookupEntry tb name
                case mentry of
                    Nothing
                        | kind == PutEntry && not (null names) ->
                            Left . ModifiedBuilder
                                <$> mtbNewBuilder tb Nothing
                        | otherwise -> return $ Right Nothing
                    Just x -> return $ Right (Just x)
        update bm name names y

    doUpdate GetEntry bm name sbm = do
        (_, tref) <- writeTreeBuilder (fromBuilderMod sbm)
        returnTree bm name $ f (Just (TreeEntry tref))
    doUpdate _ bm name _ = returnTree bm name (f Nothing)

    update bm name [] (Left sbm) = doUpdate kind bm name sbm
    update bm name [] (Right y)  = returnTree bm name (f y)

    update bm _ _ (Right Nothing) = return (bm, TreeEntryNotFound)
    update _ _ _ (Right (Just BlobEntry {})) =
        throwM TreeCannotTraverseBlob
    update _ _ _ (Right (Just CommitEntry {})) =
        throwM TreeCannotTraverseCommit

    update bm name names arg = do
        sbm <- case arg of
            Left sbm' -> return sbm'
            Right (Just (TreeEntry st')) -> do
                tree <- lookupTree st'
                ModifiedBuilder
                    <$> mtbNewBuilder (fromBuilderMod bm) (Just tree)
            _ -> error "queryTreeBuilder encountered the impossible"

        (sbm', z) <- walk sbm names
        let bm' = bm <> postUpdate bm sbm' name
        return $ bm' `seq` (bm', z)

    returnTree bm@(fromBuilderMod -> tb) n z = do
        bm' <- case z of
            TreeEntryNotFound     -> return bm
            TreeEntryPersistent _ -> return bm
            TreeEntryDeleted      -> do
                bm' <- mtbDropEntry tb tb n
                let tb'   = fromBuilderMod bm'
                    upds' = mtbPendingUpdates tb'
                return $ case bm' of
                    ModifiedBuilder _ ->
                        ModifiedBuilder tb'
                            { mtbPendingUpdates = HashMap.delete n upds' }
                    BuilderUnchanged _ ->
                        if HashMap.member n upds'
                        then ModifiedBuilder tb'
                            { mtbPendingUpdates = HashMap.delete n upds' }
                        else bm'
            TreeEntryMutated z'   -> mtbPutEntry tb tb n z'
        let bm'' = bm <> bm'
        return $ bm'' `seq` (bm'', z)

    postUpdate bm (BuilderUnchanged _) _ = bm
    postUpdate (fromBuilderMod -> tb) (ModifiedBuilder sbm) name =
        ModifiedBuilder $ tb
            { mtbPendingUpdates =
                   HashMap.insert name sbm (mtbPendingUpdates tb) }

    pathSeparator :: Word8
    pathSeparator = fromIntegral $ ord '/'

    isPathSeparator :: Word8 -> Bool
    isPathSeparator = (== pathSeparator)

    splitDirectories :: RawFilePath -> [RawFilePath]
    splitDirectories x
        | B.null x = []
        | isPathSeparator (B.head x) = let (root,rest) = B.splitAt 1 x
                                        in root : splitter rest
        | otherwise = splitter x
      where
        splitter = filter (not . B.null) . B.split pathSeparator

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTreeBuilder :: MonadGit r m
                 => TreeBuilder r m -> m (TreeBuilder r m, TreeOid r)
writeTreeBuilder builder = do
    (bm, mtref) <- go (BuilderUnchanged builder)
    tref <- case mtref of
        Nothing   -> parseObjOid emptyTreeId
        Just tref -> return tref
    return (fromBuilderMod bm, tref)
  where
    go bm = do
        let upds = mtbPendingUpdates (fromBuilderMod bm)
        bm' <- if HashMap.size upds == 0
               then return bm
               else do
                   bm' <- foldM update bm $ HashMap.toList upds
                   return $ ModifiedBuilder (fromBuilderMod bm')
                       { mtbPendingUpdates = HashMap.empty }
        let tb' = fromBuilderMod bm'
        cnt <- mtbEntryCount tb'
        if cnt == 0
            then return (bm', Nothing)
            else do
                 (bm'', tref) <- mtbWriteContents tb' tb'
                 return (bm' <> bm'', Just tref)

    update bm (k,v) = do
        let tb = fromBuilderMod bm
        -- The intermediate TreeBuilder will be dropped after this fold is
        -- completed, by setting mtbPendingUpdates to HashMap.empty, above.
        (_,mtref) <- go (BuilderUnchanged v)
        bm' <- case mtref of
            Nothing   -> mtbDropEntry tb tb k
            Just tref -> mtbPutEntry tb tb k (TreeEntry tref)
        return $ bm <> bm'

getEntry :: MonadGit r m => TreeFilePath -> TreeT r m (Maybe (TreeEntry r))
getEntry path = do
    tb <- getBuilder
    snd <$> lift (queryTreeBuilder tb path GetEntry
                  (toModifyTreeResult TreeEntryPersistent))

putEntry :: MonadGit r m => TreeFilePath -> TreeEntry r -> TreeT r m ()
putEntry path ent = do
    tb  <- getBuilder
    tb' <- fst <$> lift (queryTreeBuilder tb path PutEntry
                         (const (TreeEntryMutated ent)))
    putBuilder tb'

dropEntry :: MonadGit r m => TreeFilePath -> TreeT r m ()
dropEntry path = do
    tb  <- getBuilder
    tb' <- fst <$> lift (queryTreeBuilder tb path DropEntry
                         (const TreeEntryDeleted))
    putBuilder tb'

putBlob' :: MonadGit r m
         => TreeFilePath -> BlobOid r -> BlobKind -> TreeT r m ()
putBlob' path b kind = putEntry path (BlobEntry b kind)

putBlob :: MonadGit r m => TreeFilePath -> BlobOid r -> TreeT r m ()
putBlob path b = putBlob' path b PlainBlob

putTree :: MonadGit r m => TreeFilePath -> TreeOid r -> TreeT r m ()
putTree path t = putEntry path (TreeEntry t)

putCommit :: MonadGit r m => TreeFilePath -> CommitOid r -> TreeT r m ()
putCommit path c = putEntry path (CommitEntry c)

doWithTree :: MonadGit r m => Maybe (Tree r) -> TreeT r m a -> m (a, TreeOid r)
doWithTree rtr act =
    fst <$> (runStateT (runTreeT go) =<< newTreeBuilder rtr)
  where
    go = liftM2 (,) act currentTreeOid

withTree :: MonadGit r m => Tree r -> TreeT r m a -> m (a, TreeOid r)
withTree tr = doWithTree (Just tr)

withTreeOid :: MonadGit r m => TreeOid r -> TreeT r m a -> m (a, TreeOid r)
withTreeOid oid action = do
    tree <- lookupTree oid
    doWithTree (Just tree) action

mutateTree :: MonadGit r m => Tree r -> TreeT r m a -> m (TreeOid r)
mutateTree tr action = snd <$> withTree tr action

mutateTreeOid :: MonadGit r m => TreeOid r -> TreeT r m a -> m (TreeOid r)
mutateTreeOid tr action = snd <$> withTreeOid tr action

currentTreeOid :: MonadGit r m => TreeT r m (TreeOid r)
currentTreeOid = do
    tb <- getBuilder
    (tb', toid) <- lift $ writeTreeBuilder tb
    putBuilder tb'
    return toid

currentTree :: MonadGit r m => TreeT r m (Tree r)
currentTree = lift . lookupTree =<< currentTreeOid

withNewTree :: MonadGit r m => TreeT r m a -> m (a, TreeOid r)
withNewTree = doWithTree Nothing

createTree :: MonadGit r m => TreeT r m a -> m (TreeOid r)
createTree action = snd <$> withNewTree action
