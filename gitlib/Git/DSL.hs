{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}            -- For MonadBaseControl
#if __GLASGOW_HASKELL__ > 707
{-# LANGUAGE AllowAmbiguousTypes #-}
#endif

module Git.DSL where

import Control.Applicative
import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Control.Monad.Trans.State
import Data.ByteString (ByteString)
import Data.HashMap.Strict (HashMap)
import Data.Semigroup
import Git.Types
import Pipes

newtype TreeT r m a = TreeT
    { runTreeT :: StateT (TreeBuilder r m) (GitT r m) a }

liftGitT :: GitT r m a -> TreeT r m a
liftGitT = TreeT . lift

instance Functor m => Functor (TreeT r m) where
    fmap f (TreeT t) = TreeT (fmap f t)

instance Monad m => Monad (TreeT r m) where
    return x = TreeT (return x)
    TreeT x >>= f = TreeT (x >>= runTreeT . f)

instance (Functor m, Monad m) => Applicative (TreeT r m) where
    pure = return
    (<*>) = ap

-- instance (Functor m, MonadPlus m) => Alternative (TreeT r m) where
--     empty = mzero
--     (<|>) = mplus

-- instance (MonadPlus m) => MonadPlus (TreeT r m) where
--     mzero       = TreeT mzero
--     m `mplus` n = TreeT $ runTreeT m `mplus` runTreeT n

instance (MonadFix m) => MonadFix (TreeT r m) where
    mfix f = TreeT $ mfix $ runTreeT . f

instance MonadTrans (TreeT r) where
    lift m = TreeT $ lift (lift m)

instance (MonadIO m) => MonadIO (TreeT r m) where
    liftIO = lift . liftIO

getBuilder :: Monad m => TreeT r m (TreeBuilder r m)
getBuilder = TreeT get

putBuilder :: Monad m => TreeBuilder r m -> TreeT r m ()
putBuilder = TreeT . put

data TreeBuilder r m = TreeBuilder
    { mtbBaseTreeOid    :: Maybe (Oid r)
    , mtbPendingUpdates :: HashMap TreeFilePath (TreeBuilder r m)
    , mtbNewBuilder     :: Maybe (Tree r) -> GitT r m (TreeBuilder r m)
    , mtbWriteContents  :: TreeBuilder r m
                        -> GitT r m (ModifiedBuilder r m, Oid r)
    , mtbLookupEntry    :: TreeFilePath -> GitT r m (Maybe (TreeEntry r))
    , mtbEntryCount     :: GitT r m Int
    , mtbPutEntry       :: TreeBuilder r m -> TreeFilePath -> TreeEntry r
                        -> GitT r m (ModifiedBuilder r m)
    , mtbDropEntry      :: TreeBuilder r m -> TreeFilePath
                        -> GitT r m (ModifiedBuilder r m)
    }

data ModifiedBuilder r m = ModifiedBuilder (TreeBuilder r m)
                         | BuilderUnchanged (TreeBuilder r m)

instance Semigroup (ModifiedBuilder r m) where
    BuilderUnchanged _  <> BuilderUnchanged b2 = BuilderUnchanged b2
    ModifiedBuilder b1  <> BuilderUnchanged _  = ModifiedBuilder b1
    BuilderUnchanged _  <> ModifiedBuilder b2  = ModifiedBuilder b2
    ModifiedBuilder _   <> ModifiedBuilder b2  = ModifiedBuilder b2

instance Monoid (ModifiedBuilder r m) where
    mempty = BuilderUnchanged (error "ModifiedBuilder is a semigroup")
    x `mappend` y = x <> y

fromBuilderMod :: ModifiedBuilder r m -> TreeBuilder r m
fromBuilderMod (BuilderUnchanged tb) = tb
fromBuilderMod (ModifiedBuilder tb)  = tb

-- | 'Repository' is the central point of contact between user code and Git
--   data objects.  Every object must belong to some repository.
data GitExprF r m s
    = M (m s)                   -- some arbitrary action in 'm'
    | forall e. Exception e => Catch s (e -> s)
    | Lifted ((forall a. GitT r m a -> m a) -> m s)

    | ParseOid String (Oid r -> s)

    -- References
    | CreateReference RefName (RefTarget r) s
    | LookupReference RefName (Maybe (RefTarget r) -> s)
    | DeleteReference RefName s
    | AllReferences (Producer RefName m () -> s)

    -- -- Object lookup
    | LookupObject (Oid r) (Object r m -> s)
    | ExistsObject (Oid r) (Maybe (ObjectOid r) -> s)
    | AllObjects
        { haveCommit     :: Maybe (Oid r)
        , needCommit     :: Oid r
        , includeTrees   :: Bool
        , objectProducer :: Producer (ObjectOid r) m () -> s
        }

    | LookupCommit (Oid r) (Commit r -> s)
    | LookupTree   (Oid r) (Tree r -> s)
    | LookupBlob   (Oid r) (Blob r m -> s)
    | LookupTag    (Oid r) (Tag r -> s)

    | ReadIndex  (TreeT r m () -> s)
    | WriteIndex (TreeT r m ()) s

    | NewTreeBuilder (Maybe (Tree r)) (TreeBuilder r m -> s)
    | TreeOid (Tree r) (Oid r -> s)
    | ExistsTreeEntry (Tree r) TreeFilePath (Maybe (TreeEntry r) -> s)
    | AllTreeEntries (Tree r) (Producer (TreeFilePath, TreeEntry r) m () -> s)

    | DiffContentsWithTree
        { pathsToCompare :: Producer (Either TreeFilePath ByteString) m ()
        , basisTree      :: Tree r
        , diffProducer   :: Producer ByteString m () -> s
        }

    | HashContents (BlobContents m) (Oid r -> s)
    | CreateBlob   (BlobContents m) (Oid r -> s)
    | CreateCommit
        { commitTemplate :: Commit r
        , createUnderRef :: Maybe RefName
        , commitResult   :: Oid r -> s
        }
    | CreateTag (Tag r) RefName (Oid r -> s)

newtype GitT r m a = GitT { getGitT :: F (GitExprF r m) a }
    deriving (Functor, Applicative, Monad, MonadFix)

-- instance MonadPlus m => MonadPlus (GitT r m) where
--     mzero       = GitT mzero
--     m `mplus` n = GitT $ getGitT m `mplus` getGitT n

instance MonadTrans (GitT r) where
    lift x = GitT $ F $ \p k -> k (M (liftM p x))

instance MonadIO m => MonadIO (GitT r m) where
    liftIO = lift . liftIO

instance MonadBase b m => MonadBase b (GitT r m) where
    liftBase = lift . liftBase

instance MonadThrow m => MonadThrow (GitT r m) where
    throwM = lift . throwM

instance MonadCatch m => MonadCatch (GitT r m) where
    catch (GitT (F act)) hndlr = GitT $ F $ \p k ->
        k $ Catch (act p k) (\e -> runF (getGitT (hndlr e)) p k)

instance MonadBaseControl b m => MonadBaseControl b (GitT r m) where
    type StM (GitT r m) a = StM m a -- no internal state
    liftBaseWith f = GitT $ F $ \p k -> k $ Lifted $ \c ->
        liftBaseWith $ \runInBase -> liftM p $ f (runInBase . c)
    restoreM = lift . restoreM

gitT :: (forall s. (a -> s) -> GitExprF r m s) -> GitT r m a
gitT f = GitT $ F $ \p k -> k $ f p

gitT_ :: (forall s. s -> GitExprF r m s) -> GitT r m ()
gitT_ f = GitT $ F $ \p k -> k $ f (p ())

parseOid :: String -> GitT r m (Oid r)
parseOid str = gitT $ ParseOid str

-- References
createReference :: RefName -> RefTarget r -> GitT r m ()
createReference name target = gitT_ $ CreateReference name target

lookupReference :: RefName -> GitT r m (Maybe (RefTarget r))
lookupReference name = gitT $ LookupReference name

deleteReference :: RefName -> GitT r m ()
deleteReference name = gitT_ $ DeleteReference name

allReferences' :: GitT r m (Producer RefName m ())
allReferences' = gitT AllReferences

allReferences :: Monad m => Producer RefName (GitT r m) ()
allReferences = hoist lift =<< lift allReferences'

printReferences :: MonadIO m => Effect (GitT r m) ()
printReferences = for allReferences $ liftIO . print

-- -- Object lookup
lookupObject :: Oid r -> GitT r m (Object r m)
lookupObject o = gitT $ LookupObject o

existsObject :: Oid r -> GitT r m (Maybe (ObjectOid r))
existsObject o = gitT $ ExistsObject o

allObjects' :: Maybe (Oid r) -> Oid r -> Bool
            -> GitT r m (Producer (ObjectOid r) m ())
allObjects' mhave need trees = gitT $ AllObjects mhave need trees

allObjects :: Monad m => Maybe (Oid r) -> Oid r -> Bool
           -> Producer (ObjectOid r) (GitT r m) ()
allObjects mhave need trees = hoist lift =<< lift (allObjects' mhave need trees)

lookupCommit :: Oid r -> GitT r m (Commit r)
lookupCommit o = gitT $ LookupCommit o

lookupTree :: Oid r -> GitT r m (Tree r)
lookupTree o = gitT $ LookupTree o

lookupBlob :: Oid r -> GitT r m (Blob r m)
lookupBlob o = gitT $ LookupBlob o

lookupTag :: Oid r -> GitT r m (Tag r)
lookupTag o = gitT $ LookupTag o

readIndex :: GitT r m (TreeT r m ())
readIndex = gitT ReadIndex

writeIndex :: TreeT r m () -> GitT r m ()
writeIndex t = gitT_ $ WriteIndex t

newTreeBuilder :: Maybe (Tree r) -> GitT r m (TreeBuilder r m)
newTreeBuilder mt = gitT $ NewTreeBuilder mt

treeOid :: Tree r -> GitT r m (Oid r)
treeOid t = gitT $ TreeOid t

existsTreeEntry :: Tree r -> TreeFilePath -> GitT r m (Maybe (TreeEntry r))
existsTreeEntry t path = gitT $ ExistsTreeEntry t path

allTreeEntries' :: Tree r
                -> GitT r m (Producer (TreeFilePath, TreeEntry r) m ())
allTreeEntries' t = gitT $ AllTreeEntries t

allTreeEntries :: Monad m => Tree r
               -> Producer (TreeFilePath, TreeEntry r) (GitT r m) ()
allTreeEntries t = hoist lift =<< lift (allTreeEntries' t)

diffContentsWithTree' :: Producer (Either TreeFilePath ByteString) m ()
                      -> Tree r
                      -> GitT r m (Producer ByteString m ())
diffContentsWithTree' contents t = gitT $ DiffContentsWithTree contents t

diffContentsWithTree :: Monad m
                     => Producer (Either TreeFilePath ByteString) m ()
                     -> Tree r
                     -> Producer ByteString (GitT r m) ()
diffContentsWithTree contents t =
    hoist lift =<< lift (diffContentsWithTree' contents t)

hashContents :: BlobContents m -> GitT r m (Oid r)
hashContents contents = gitT $ HashContents contents

createBlob :: BlobContents m -> GitT r m (Oid r)
createBlob contents = gitT $ CreateBlob contents

createCommit :: Commit r -> Maybe RefName -> GitT r m (Oid r)
createCommit c mname = gitT $ CreateCommit c mname

createTag :: Tag r -> RefName -> GitT r m (Oid r)
createTag t name = gitT $ CreateTag t name

-- Utility functions; jww (2015-06-14): these belong elsewhere
copyOid :: Repository r => Oid r -> GitT s m (Oid s)
copyOid = parseOid . show

objectOid :: Monad m => Object r m -> GitT r m (Oid r)
objectOid (BlobObj obj)   = return $ blobOid obj
objectOid (TreeObj obj)   = treeOid obj
objectOid (CommitObj obj) = return $ commitOid obj
objectOid (TagObj obj)    = return $ tagOid obj

copyMergeResult :: Repository s => MergeResult s -> GitT r m (MergeResult r)
copyMergeResult (MergeSuccess mc) =
    MergeSuccess <$> parseOid (show mc)
copyMergeResult (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> parseOid (show hl)
                    <*> parseOid (show hr)
                    <*> parseOid (show mc)
                    <*> pure cs
