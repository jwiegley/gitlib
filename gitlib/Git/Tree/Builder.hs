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
import           Control.Failure
import           Control.Monad
import           Control.Monad.Fix
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Function
import qualified Data.HashMap.Strict as HashMap
import           Data.List
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Git.Types
import           Prelude hiding (FilePath)

data ModifyTreeResult m = TreeEntryNotFound
                        | TreeEntryDeleted
                        | TreeEntryPersistent (TreeEntry m)
                        | TreeEntryMutated (TreeEntry m)

fromModifyTreeResult :: ModifyTreeResult m -> Maybe (TreeEntry m)
fromModifyTreeResult TreeEntryNotFound       = Nothing
fromModifyTreeResult TreeEntryDeleted        = Nothing
fromModifyTreeResult (TreeEntryPersistent x) = Just x
fromModifyTreeResult (TreeEntryMutated x)    = Just x

toModifyTreeResult :: (TreeEntry m -> ModifyTreeResult m)
                   -> Maybe (TreeEntry m)
                   -> ModifyTreeResult m
toModifyTreeResult _ Nothing  = TreeEntryNotFound
toModifyTreeResult f (Just x) = f x

newtype TreeT m a = TreeT { runTreeT :: StateT (TreeBuilder m) m a }

instance Functor m => Functor (TreeT m) where
    fmap f (TreeT t) = TreeT (fmap f t)

instance Monad m => Monad (TreeT m) where
    return x = TreeT (return x)
    TreeT x >>= f = TreeT (x >>= runTreeT . f)

instance (Functor m, Monad m) => Applicative (TreeT m) where
    pure = return
    (<*>) = ap

instance (Functor m, MonadPlus m) => Alternative (TreeT m) where
    empty = mzero
    (<|>) = mplus

instance (MonadPlus m) => MonadPlus (TreeT m) where
    mzero       = TreeT $ mzero
    m `mplus` n = TreeT $ runTreeT m `mplus` runTreeT n

instance (MonadFix m) => MonadFix (TreeT m) where
    mfix f = TreeT $ mfix $ \ ~a -> runTreeT (f a)

instance MonadTrans TreeT where
    lift m = TreeT $ lift m

instance (MonadIO m) => MonadIO (TreeT m) where
    liftIO = lift . liftIO

getBuilder :: Monad m => TreeT m (TreeBuilder m)
getBuilder = TreeT get

putBuilder :: Monad m => TreeBuilder m -> TreeT m ()
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
queryTreeBuilder :: Repository m
                  => TreeBuilder m
                  -> Text
                  -> BuilderAction
                  -> (Maybe (TreeEntry m) -> ModifyTreeResult m)
                  -> m (TreeBuilder m, Maybe (TreeEntry m))
queryTreeBuilder builder path kind f = do
    (mtb, mtresult) <- walk (BuilderUnchanged builder) (splitPath path)
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
        failure TreeCannotTraverseBlob
    update _ _ _ (Right (Just CommitEntry {})) =
        failure TreeCannotTraverseCommit

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

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTreeBuilder :: Repository m
                 => TreeBuilder m -> m (TreeBuilder m, TreeOid m)
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

getEntry :: Repository m => Text -> TreeT m (Maybe (TreeEntry m))
getEntry path = do
    tb <- getBuilder
    snd <$> lift (queryTreeBuilder tb path GetEntry
                  (toModifyTreeResult TreeEntryPersistent))

putEntry :: Repository m => Text -> TreeEntry m -> TreeT m ()
putEntry path ent = do
    tb  <- getBuilder
    tb' <- fst <$> lift (queryTreeBuilder tb path PutEntry
                         (const (TreeEntryMutated ent)))
    putBuilder tb'

dropEntry :: Repository m => Text -> TreeT m ()
dropEntry path = do
    tb  <- getBuilder
    tb' <- fst <$> lift (queryTreeBuilder tb path DropEntry
                         (const TreeEntryDeleted))
    putBuilder tb'

putBlob' :: Repository m => Text -> BlobOid m -> BlobKind -> TreeT m ()
putBlob' path b kind = putEntry path (BlobEntry b kind)

putBlob :: Repository m => Text -> BlobOid m -> TreeT m ()
putBlob path b = putBlob' path b PlainBlob

putTree :: Repository m => Text -> TreeOid m -> TreeT m ()
putTree path t = putEntry path (TreeEntry t)

putCommit :: Repository m => Text -> CommitOid m -> TreeT m ()
putCommit path c = putEntry path (CommitEntry c)

doWithTree :: Repository m => Maybe (Tree m) -> TreeT m a -> m (a, TreeOid m)
doWithTree mtr act =
    fst <$> (runStateT (runTreeT go) =<< newTreeBuilder mtr)
  where
    go = liftM2 (,) act currentTreeOid

withTree :: Repository m => Tree m -> TreeT m a -> m (a, TreeOid m)
withTree tr = doWithTree (Just tr)

withTreeOid :: Repository m => TreeOid m -> TreeT m a -> m (a, TreeOid m)
withTreeOid oid action = do
    tree <- lookupTree oid
    doWithTree (Just tree) action

mutateTree :: Repository m => Tree m -> TreeT m a -> m (TreeOid m)
mutateTree tr action = snd <$> withTree tr action

mutateTreeOid :: Repository m => TreeOid m -> TreeT m a -> m (TreeOid m)
mutateTreeOid tr action = snd <$> withTreeOid tr action

currentTreeOid :: Repository m => TreeT m (TreeOid m)
currentTreeOid = do
    tb <- getBuilder
    (tb', toid) <- lift $ writeTreeBuilder tb
    putBuilder tb'
    return toid

currentTree :: Repository m => TreeT m (Tree m)
currentTree = lift . lookupTree =<< currentTreeOid

withNewTree :: Repository m => TreeT m a -> m (a, TreeOid m)
withNewTree = doWithTree Nothing

createTree :: Repository m => TreeT m a -> m (TreeOid m)
createTree action = snd <$> withNewTree action

splitPath :: Text -> [Text]
splitPath = T.splitOn "/"
