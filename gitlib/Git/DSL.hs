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

import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.State (StateT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.Functor.Identity
import           Data.HashMap.Strict (HashMap)
import           Data.Text (Text)
import           Git.Types
import           Streaming
import           Streaming.Internal (Stream(..))
import qualified Streaming.Prelude as S

data GitExprF r m s
    = ParseOidF String (Oid r -> s)

    | CreateReferenceF RefName (RefTarget r) s
    | LookupReferenceF RefName (Maybe (RefTarget r) -> s)
    | DeleteReferenceF RefName s
    | AllReferencesF (Stream (Of RefName) (GitT r m) () -> s)

    | LookupObjectF (Oid r) (Maybe (Object r) -> s)
    | ExistsObjectF (Oid r) (Maybe (ObjectOid r) -> s)
    | AllObjectsF
        { haveCommit   :: Maybe (CommitOid r)
        , needCommit   :: CommitOid r
        , includeTrees :: Bool
        , objProducer  :: Stream (Of (ObjectOid r)) (GitT r m) () -> s
        }

    | LookupBlobF   (BlobOid r) (Stream (Of ByteString) (GitT r m) () -> s)
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

    | RunTreeBuilderFromF (TreeOid r) (TreeBuilderT r (GitT r m) s)
    | RunTreeBuilderF (TreeBuilderT r (GitT r m) s)
    deriving Functor

newtype GitT r m a = GitT { runGitT :: Stream (GitExprF r m) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

type Git r = GitT r Identity

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

pattern RunTreeBuilderFrom t k = Step (RunTreeBuilderFromF t k)
pattern RunTreeBuilder k       = Step (RunTreeBuilderF k)

parseOid :: Monad m => String -> GitT r m (Oid r)
parseOid str = GitT $ ParseOid str pure

createReference :: Monad m => RefName -> RefTarget r -> GitT r m ()
createReference name target = GitT $ CreateReference name target (pure ())

lookupReference :: Monad m => RefName -> GitT r m (Maybe (RefTarget r))
lookupReference name = GitT $ LookupReference name pure

deleteReference :: Monad m => RefName -> GitT r m ()
deleteReference name = GitT $ DeleteReference name (pure ())

allReferences :: Monad m => GitT r m (Stream (Of RefName) (GitT r m) ())
allReferences = GitT $ AllReferences pure

printReferences :: MonadIO m => GitT r m ()
printReferences = S.print =<< allReferences

lookupObject :: Monad m => Oid r -> GitT r m (Maybe (Object r))
lookupObject o = GitT $ LookupObject o pure

existsObject :: Monad m => Oid r -> GitT r m (Maybe (ObjectOid r))
existsObject o = GitT $ ExistsObject o pure

allObjects :: Monad m
           => Maybe (Oid r)
           -> Oid r
           -> Bool
           -> GitT r m (Stream (Of (ObjectOid r)) (GitT r m) ())
allObjects mhave need trees = GitT $ AllObjects mhave need trees pure

lookupCommit :: Monad m => CommitOid r -> GitT r m (Maybe (Commit r))
lookupCommit o = GitT $ LookupCommit o pure

lookupTree :: Monad m => TreeOid r -> GitT r m (Maybe (Tree r))
lookupTree o = GitT $ LookupTree o pure

treeOid :: Monad m => Tree r -> GitT r m (TreeOid r)
treeOid o = GitT $ TreeOid o pure

lookupBlob :: Monad m => BlobOid r -> GitT r m (Stream (Of ByteString) (GitT r m) ())
lookupBlob o = GitT $ LookupBlob o pure

lookupTag :: Monad m => TagOid r -> GitT r m (Maybe (Tag r))
lookupTag o = GitT $ LookupTag o pure

createBlob :: Monad m => BL.ByteString -> GitT r m (BlobOid r)
createBlob contents = GitT $ CreateBlob contents pure

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
    pure $ Commit oid a b c d e f

createCommit :: Monad m => Commit r -> Maybe RefName -> GitT r m (CommitOid r)
createCommit c mname = GitT $ CreateCommit c mname pure

createTag :: Monad m => Tag r -> RefName -> GitT r m (TagOid r)
createTag t name = GitT $ CreateTag t name pure

runTreeBuilderFromT :: Monad m => TreeOid r -> TreeBuilderT r (GitT r m) a -> GitT r m a
runTreeBuilderFromT toid m = GitT $ RunTreeBuilderFrom toid (Return <$> m)

runTreeBuilderT :: Monad m => TreeBuilderT r (GitT r m) a -> GitT r m a
runTreeBuilderT m = GitT $ RunTreeBuilder (Return <$> m)

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
    | AllEntriesF
          (Stream (Of (TreeFilePath, TreeEntry r)) (TreeBuilderT r m) () -> s)
    | WithStateMapF (StateT (HashMap TreeFilePath (TreeEntry r))
                            (TreeBuilderT r m) s)
    | GetTreeMapF (HashMap TreeFilePath (TreeEntry r) -> s)
    | DiffContentsWithTreeF
        { pathsToCompare :: [Either TreeFilePath ByteString]
        , basisTree      :: Tree r
        , diffStream     :: Stream (Of ByteString) (TreeBuilderT r m) () -> s
        }
    deriving Functor

newtype TreeBuilderT r m a
    = TreeBuilderT { getTreeBuilderT :: Stream (TreeExprF r m) m a }
    deriving (Functor, Applicative, Monad, MonadIO)

type TreeBuilder r = TreeBuilderT r Identity

instance MonadTrans (TreeBuilderT r) where
    lift = TreeBuilderT . lift

pattern SetBaseOid s k  = Step (SetBaseOidF s k)
pattern GetBaseOid k    = Step (GetBaseOidF k)
pattern SetBaseTree t k = Step (SetBaseTreeF t k)
pattern CurrentTree k   = Step (CurrentTreeF k)
pattern WriteTree k     = Step (WriteTreeF k)
pattern ReadFromIndex s = Step (ReadFromIndexF s)
pattern WriteToIndex s  = Step (WriteToIndexF s)
pattern ClearEntries s  = Step (ClearEntriesF s)
pattern EntryCount k    = Step (EntryCountF k)
pattern PutEntry p e s  = Step (PutEntryF p e s)
pattern DropEntry p s   = Step (DropEntryF p s)
pattern LookupEntry p k = Step (LookupEntryF p k)
pattern AllEntries c    = Step (AllEntriesF c)
pattern WithStateMap s  = Step (WithStateMapF s)
pattern GetTreeMap k    = Step (GetTreeMapF k)

pattern DiffContentsWithTree ps t c = Step (DiffContentsWithTreeF ps t c)

setBaseOid :: Monad m => TreeOid r -> TreeBuilderT r m ()
setBaseOid str = TreeBuilderT $ SetBaseOid str (pure ())

getBaseOid :: Monad m => TreeBuilderT r m (Maybe (TreeOid r))
getBaseOid = TreeBuilderT $ GetBaseOid pure

setBaseTree :: Monad m => Tree r -> TreeBuilderT r m ()
setBaseTree t = TreeBuilderT $ SetBaseTree t (pure ())

currentTree :: Monad m => TreeBuilderT r m (Tree r)
currentTree = TreeBuilderT $ CurrentTree pure

writeTree :: Monad m => TreeBuilderT r m (TreeOid r)
writeTree = TreeBuilderT $ WriteTree pure

readFromIndex :: Monad m => TreeBuilderT r m ()
readFromIndex = TreeBuilderT $ ReadFromIndex (pure ())

writeToIndex :: Monad m => TreeBuilderT r m ()
writeToIndex = TreeBuilderT $ WriteToIndex (pure ())

clearEntries :: Monad m => TreeBuilderT r m ()
clearEntries = TreeBuilderT $ ClearEntries (pure ())

entryCount :: Monad m => TreeBuilderT r m Int
entryCount = TreeBuilderT $ EntryCount pure

putEntry :: Monad m
         => TreeFilePath -> TreeEntry r -> TreeBuilderT r m ()
putEntry p e = TreeBuilderT $ PutEntry p e (pure ())

dropEntry :: Monad m => TreeFilePath -> TreeBuilderT r m ()
dropEntry p = TreeBuilderT $ DropEntry p (pure ())

lookupEntry :: Monad m
            => TreeFilePath -> TreeBuilderT r m (Maybe (TreeEntry r))
lookupEntry p = TreeBuilderT $ LookupEntry p pure

allEntries :: Monad m
           => TreeBuilderT r m (Stream (Of (TreeFilePath, TreeEntry r))
                                       (TreeBuilderT r m) ())
allEntries = TreeBuilderT $ AllEntries pure

withStateMap :: Monad m
             => StateT (HashMap TreeFilePath (TreeEntry r)) (TreeBuilderT r m) a
             -> TreeBuilderT r m a
withStateMap s = TreeBuilderT $ WithStateMap (Return <$> s)

getTreeMap :: Monad m
           => TreeBuilderT r m (HashMap TreeFilePath (TreeEntry r))
getTreeMap = TreeBuilderT $ GetTreeMap pure

diffContentsWithTree :: Monad m
                     => [Either TreeFilePath ByteString]
                     -> Tree r
                     -> TreeBuilderT r m (Stream (Of ByteString) (TreeBuilderT r m) ())
diffContentsWithTree ps t = TreeBuilderT $ DiffContentsWithTree ps t pure

-- Utility functions; jww (2015-06-14): these belong elsewhere

copyOid :: (Monad m, Repository r) => Oid r -> GitT s m (Oid s)
copyOid = parseOid . show

objectOid :: Monad m => Object r -> GitT r m (Oid r)
objectOid (BlobObj oid)   = pure oid
objectOid (TreeObj obj)   = treeOid obj
objectOid (CommitObj obj) = pure $ commitOid obj
objectOid (TagObj obj)    = pure $ tagOid obj

copyMergeResult :: (Repository s, Monad m)
                => MergeResult s -> GitT r m (MergeResult r)
copyMergeResult (MergeSuccess mc) = MergeSuccess <$> parseOid (show mc)
copyMergeResult (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> parseOid (show hl)
                    <*> parseOid (show hr)
                    <*> parseOid (show mc)
                    <*> pure cs
