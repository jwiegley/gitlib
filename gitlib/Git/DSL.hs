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

import Control.Monad
import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Fix
import Control.Monad.Free.Church
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Control.Monad.Trans.Control
import Data.ByteString (ByteString)
import Git.Types
import Pipes
import Pipes.Safe hiding (liftBase)

-- | 'Repository' is the central point of contact between user code and Git
--   data objects.  Every object must belong to some repository.
data GitExprF r m s
    = M (m s)                   -- some arbitrary action in 'm'
    | forall e. Exception e => Catch s (e -> s)
    | Lifted ((forall a. GitT r m a -> m a) -> m s)

    | ParseOid String (Oid r -> s)

    -- References
    | CreateReference RefName (RefTarget r) s
    | ExistsReference RefName (Maybe (RefTarget r) -> s)
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

-- oid :: String -> GitT r m (Oid r)
-- oid = parseOid

lookupCommit :: Oid r -> GitT r m (Commit r)
lookupCommit o = gitT $ LookupCommit o

lookupTree :: Oid r -> GitT r m (Tree r)
lookupTree o = gitT $ LookupTree o

treeOid :: Tree r -> GitT r m (Oid r)
treeOid t = gitT $ TreeOid t

-- commit :: Getter (GitT r m (Oid r)) (GitT r m (Commit r))
-- commit k o = o <$ k (lookupCommit =<< o)

-- tree :: Getter (GitT r m (Commit r)) (GitT r m (Tree r))
-- tree k c = c <$ k (lookupTree . commitTree =<< c)

-- foo :: GitT r m (Tree r)
-- foo = oid "test" ^. commit.tree
-- foo = parseOid "test" ^. commit.tree.entry "foo"

-- References

createReference :: RefName -> RefTarget r -> GitT r m ()
createReference name target = gitT_ $ CreateReference name target

allReferences' :: GitT r m (Producer RefName m ())
allReferences' = gitT AllReferences

allReferences :: MonadSafe m => Producer RefName (GitT r m) ()
allReferences = hoist lift =<< lift allReferences'

printReferences :: (MonadSafe m, MonadIO m) => Effect (GitT r m) ()
printReferences = for allReferences $ liftIO . print

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
