{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Git.DSL where

import           Control.Monad
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.State (StateT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           Git.Types
import           Streaming.Internal (Stream(..))

type Consumer a m = Stream ((->) a) m

await :: Monad m => Consumer a m a
await = Step pure

data GitExprF r m s
    = ParseOidF String (Oid r -> s)

    | CreateReferenceF RefName (RefTarget r) s
    | LookupReferenceF RefName (Maybe (RefTarget r) -> s)
    | DeleteReferenceF RefName s
    | AllReferencesF (Consumer RefName (GitT r m) s)

    | LookupObjectF (Oid r) (Maybe (Object r) -> s)
    | ExistsObjectF (Oid r) (Maybe (ObjectOid r) -> s)
    | AllObjectsF
        { haveCommit   :: Maybe (CommitOid r)
        , needCommit   :: CommitOid r
        , includeTrees :: Bool
        , objConsumer  :: Consumer (ObjectOid r) (GitT r m) s
        }

    | LookupBlobF   (BlobOid r) (Consumer ByteString (GitT r m) s)
    | LookupCommitF (CommitOid r) (Maybe (Commit r) -> s)
    | LookupTagF    (TagOid r) (Maybe (Tag r) -> s)
    | LookupTreeF   (TreeOid r) (Maybe (Tree r) -> s)
    | TreeOidF      (Tree r) (TreeOid r -> s)

    | CreateBlobF   BL.ByteString (BlobOid r -> s)
    | CreateCommitF
        { commitTemplate :: Commit r
        , createUnderRef :: Maybe RefName
        , commitResult   :: CommitOid r -> s
        }
    | CreateTagF (Tag r) RefName (TagOid r -> s)

    | RunTreeBuilderF (TreeBuilderT r (GitT r m) s)
    deriving Functor

pattern ParseOid s k = Step (ParseOidF s k)

pattern CreateReference r t s = Step (CreateReferenceF r t s)
pattern LookupReference r k   = Step (LookupReferenceF r k)
pattern DeleteReference r s   = Step (DeleteReferenceF r s)
pattern AllReferences k       = Step (AllReferencesF k)

pattern LookupObject o k   = Step (LookupObjectF o k)
pattern ExistsObject o k   = Step (ExistsObjectF o k)
pattern AllObjects x y z k = Step (AllObjectsF x y z k)

pattern LookupBlob o k   = Step (LookupBlobF o k)
pattern LookupCommit o k = Step (LookupCommitF o k)
pattern LookupTag o k    = Step (LookupTagF o k)
pattern LookupTree o k   = Step (LookupTreeF o k)
pattern TreeOid o k      = Step (TreeOidF o k)

pattern CreateBlob c k     = Step (CreateBlobF c k)
pattern CreateCommit c r k = Step (CreateCommitF c r k)
pattern CreateTag t r k    = Step (CreateTagF t r k)

pattern RunTreeBuilder k   = Step (RunTreeBuilderF k)

newtype GitT r m a = GitT { runGitT :: Stream (GitExprF r m) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

type Git r = GitT r Identity

parseOid :: Monad m => String -> GitT r m (Oid r)
parseOid str = GitT $ ParseOid str return

createReference :: Monad m => RefName -> RefTarget r -> GitT r m ()
createReference name target = GitT $ CreateReference name target (pure ())

lookupReference :: Monad m => RefName -> GitT r m (Maybe (RefTarget r))
lookupReference name = GitT $ LookupReference name return

deleteReference :: Monad m => RefName -> GitT r m ()
deleteReference name = GitT $ DeleteReference name (pure ())

allReferences :: Monad m => Consumer RefName (GitT r m) a -> GitT r m a
allReferences m = GitT $ AllReferences (Return <$> m)

printReferences :: MonadIO m => GitT r m ()
printReferences = allReferences $ forever $ await >>= liftIO . print

lookupObject :: Monad m => Oid r -> GitT r m (Maybe (Object r))
lookupObject o = GitT $ LookupObject o return

existsObject :: Monad m => Oid r -> GitT r m (Maybe (ObjectOid r))
existsObject o = GitT $ ExistsObject o return

allObjects :: Monad m
           => Maybe (Oid r)
           -> Oid r
           -> Bool
           -> Consumer (ObjectOid r) (GitT r m) a
           -> GitT r m a
allObjects mhave need trees m = GitT $ AllObjects mhave need trees (Return <$> m)

lookupCommit :: Monad m => CommitOid r -> GitT r m (Maybe (Commit r))
lookupCommit o = GitT $ LookupCommit o return

lookupTree :: Monad m => TreeOid r -> GitT r m (Maybe (Tree r))
lookupTree o = GitT $ LookupTree o return

treeOid :: Monad m => Tree r -> GitT r m (TreeOid r)
treeOid o = GitT $ TreeOid o return

lookupBlob :: Monad m => BlobOid r -> Consumer ByteString (GitT r m) a -> GitT r m a
lookupBlob o m = GitT $ LookupBlob o (Return <$> m)

lookupTag :: Monad m => TagOid r -> GitT r m (Maybe (Tag r))
lookupTag o = GitT $ LookupTag o return

createBlob :: Monad m => BL.ByteString -> GitT r m (BlobOid r)
createBlob contents = GitT $ CreateBlob contents return

emptyTreeId :: String
emptyTreeId = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

commitObj :: Monad m => [CommitOid r]
          -> CommitOid r
          -> Signature
          -> Signature
          -> CommitMessage
          -> Text
          -> GitT r m (Commit r)
commitObj a b c d e f = do
    oid <- parseOid emptyTreeId
    return $ Commit oid a b c d e f

createCommit :: Monad m => Commit r -> Maybe RefName -> GitT r m (CommitOid r)
createCommit c mname = GitT $ CreateCommit c mname return

createTag :: Monad m => Tag r -> RefName -> GitT r m (TagOid r)
createTag t name = GitT $ CreateTag t name return

runTreeBuilder :: Monad m => TreeBuilderT r (GitT r m) a -> GitT r m a
runTreeBuilder builder = GitT $ RunTreeBuilder (Return <$> builder)

-- Utility functions; jww (2015-06-14): these belong elsewhere

copyOid :: (Repository r, Monad m) => Oid r -> GitT s m (Oid s)
copyOid = parseOid . show

objectOid :: Monad m => Object r -> GitT r m (Oid r)
objectOid (BlobObj oid)   = return oid
objectOid (TreeObj obj)   = treeOid obj
objectOid (CommitObj obj) = return $ commitOid obj
objectOid (TagObj obj)    = return $ tagOid obj

copyMergeResult :: (Repository s, Monad m)
                => MergeResult s -> GitT r m (MergeResult r)
copyMergeResult (MergeSuccess mc) = MergeSuccess <$> parseOid (show mc)
copyMergeResult (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> parseOid (show hl)
                    <*> parseOid (show hr)
                    <*> parseOid (show mc)
                    <*> pure cs

data TreeExprF r m s
    = SetBaseOidF (TreeOid r) s
    | GetBaseOidF (Maybe (TreeOid r) -> s)
    | SetBaseTreeF (Tree r) s
    | CurrentTreeF (Tree r -> s)
    | WriteTreeF (TreeOid r -> s)
    | ReadFromIndexF s
    | WriteToIndexF s
    | ClearEntriesF s
    | EntryCountF (Int -> s)
    | PutEntryF TreeFilePath (TreeEntry r) s
    | DropEntryF TreeFilePath s
    | LookupEntryF TreeFilePath (Maybe (TreeEntry r) -> s)
    | AllEntriesF (Consumer (TreeFilePath, TreeEntry r) m s)
    | WithStateMapF (StateT (HashMap TreeFilePath (TreeEntry r)) m s)
    | GetTreeMapF (HashMap TreeFilePath (TreeEntry r) -> s)
    | DiffContentsWithTreeF
        { pathsToCompare :: [Either TreeFilePath ByteString]
        , basisTree      :: Tree r
        , diffStream     :: Consumer ByteString m s
        }
    deriving Functor

newtype TreeBuilderT r m a
    = TreeBuilderT { runTreeBuilderT :: Stream (TreeExprF r m) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

type TreeBuilder r = TreeBuilderT r Identity

instance MonadTrans (TreeBuilderT r) where
    lift = TreeBuilderT . lift
