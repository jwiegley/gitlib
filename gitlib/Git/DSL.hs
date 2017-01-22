{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -Wno-missing-pattern-synonym-signatures #-}

module Git.DSL where

import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Morph (hoist)
import           Control.Monad.Trans.Class (MonadTrans(lift))
import           Control.Monad.Trans.State (StateT)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import           Data.HashMap.Strict (HashMap)
import           Data.Semigroup (Semigroup(..))
import           Data.Text (Text)
import           Streaming.Internal (Stream(..))
import           Streaming.Prelude (Of)
import qualified Streaming.Prelude as S
import           Git.Types

data TreeBuilder r m = TreeBuilder
    { mtbTreeOid       :: Maybe (Oid r)
    , mtbUpdates       :: HashMap TreeFilePath (TreeBuilder r m)
    , mtbNewBuilder    :: Maybe (Tree r) -> GitT r m (TreeBuilder r m)
    , mtbWriteContents :: TreeBuilder r m
                       -> GitT r m (ModifiedBuilder r m, Oid r)
    , mtbLookupEntry   :: TreeFilePath -> GitT r m (Maybe (TreeEntry r))
    , mtbEntryCount    :: GitT r m Int
    , mtbPutEntry      :: TreeBuilder r m -> TreeFilePath -> TreeEntry r
                       -> GitT r m (ModifiedBuilder r m)
    , mtbDropEntry     :: TreeBuilder r m -> TreeFilePath
                       -> GitT r m (ModifiedBuilder r m)
    }

type TreeT r m = StateT (TreeBuilder r m) (GitT r m)

data ModifiedBuilder r m = ModifiedBuilder (TreeBuilder r m)
                         | BuilderUnchanged (TreeBuilder r m)

instance Semigroup (ModifiedBuilder r m) where
    BuilderUnchanged _  <> BuilderUnchanged b2 = BuilderUnchanged b2
    ModifiedBuilder b1  <> BuilderUnchanged _  = ModifiedBuilder b1
    BuilderUnchanged _  <> ModifiedBuilder b2  = ModifiedBuilder b2
    ModifiedBuilder _   <> ModifiedBuilder b2  = ModifiedBuilder b2

fromBuilderMod :: ModifiedBuilder r m -> TreeBuilder r m
fromBuilderMod (BuilderUnchanged tb) = tb
fromBuilderMod (ModifiedBuilder tb)  = tb

data GitF r m s
    = ParseOidF String (Oid r -> s)

    | CreateReferenceF RefName (RefTarget r) s
    | LookupReferenceF RefName (Maybe (RefTarget r) -> s)
    | DeleteReferenceF RefName s
    | AllReferencesF (Stream (Of RefName) m () -> s)

    | LookupObjectF (Oid r) (Maybe (Object r) -> s)
    | ExistsObjectF (Oid r) (Maybe (ObjectOid r) -> s)
    | AllObjectsF
        { haveCommit     :: Maybe (Oid r)
        , needCommit     :: Oid r
        , includeTrees   :: Bool
        , objectProducer :: Stream (Of (ObjectOid r)) m () -> s
        }

    | LookupCommitF (Oid r) (Maybe (Commit r) -> s)
    | LookupTreeF   (Oid r) (Maybe (Tree r) -> s)
    | LookupBlobF   (Oid r) (Maybe (Stream (Of ByteString) m ()) -> s)
    | LookupTagF    (Oid r) (Maybe (Tag r) -> s)

    | ReadIndexF  (TreeT r m () -> s)
    | WriteIndexF (TreeT r m ()) s

    | NewTreeBuilderF (Maybe (Tree r)) (TreeBuilder r m -> s)
    | TreeOidF (Tree r) (Oid r -> s)
    | GetTreeEntryF (Tree r) TreeFilePath (Maybe (TreeEntry r) -> s)
    | AllTreeEntriesF (Tree r) (Stream (Of (TreeFilePath, TreeEntry r)) m () -> s)

    | DiffContentsWithTreeF
        { pathsToCompare :: [Either TreeFilePath ByteString]
        , basisTree      :: Tree r
        , diffStream     :: Stream (Of ByteString) m () -> s
        }

    | HashContentsF BL.ByteString (Oid r -> s)
    | CreateBlobF   BL.ByteString (Oid r -> s)
    | CreateCommitF
        { commitTemplate :: Commit r
        , createUnderRef :: Maybe RefName
        , commitResult   :: Oid r -> s
        }
    | CreateTagF (Tag r) RefName (Oid r -> s)
    deriving Functor

{-
instance Functor m => Functor (GitF r m) where
    fmap f = \case
        EffectF m   -> EffectF (fmap f m)
        ChoiceF s t -> ChoiceF (f s) (f t)
        ThrowF exc  -> ThrowF exc
        CatchF a h  -> CatchF (f a) (fmap f h)
        LiftedF k   -> LiftedF (\g -> fmap f (k g))

        ParseOidF s k -> ParseOidF s (fmap f k)

        CreateReferenceF r t s -> CreateReferenceF r t (f s)
        LookupReferenceF r k   -> LookupReferenceF r (fmap f k)
        DeleteReferenceF r s   -> DeleteReferenceF r (f s)
        AllReferencesF k       -> AllReferencesF (fmap f k)

        LookupObjectF o k   -> LookupObjectF o (fmap f k)
        ExistsObjectF o k   -> ExistsObjectF o (fmap f k)
        AllObjectsF x y z k -> AllObjectsF x y z (fmap f k)

        LookupCommitF o k -> LookupCommitF o (fmap f k)
        LookupTreeF   o k -> LookupTreeF o (fmap f k)
        LookupBlobF   o k -> LookupBlobF o (fmap f k)
        LookupTagF    o k -> LookupTagF o (fmap f k)

        ReadIndexF  k   -> ReadIndexF (fmap f k)
        WriteIndexF t s -> WriteIndexF t (f s)

        NewTreeBuilderF t k -> NewTreeBuilderF t (fmap f k)
        TreeOidF t k        -> TreeOidF t (fmap f k)
        GetTreeEntryF t p k -> GetTreeEntryF t p (fmap f k)
        AllTreeEntriesF t k -> AllTreeEntriesF t (fmap f k)

        DiffContentsWithTreeF p t k -> DiffContentsWithTreeF p t (fmap f k)

        HashContentsF c k   -> HashContentsF c (fmap f k)
        CreateBlobF   c k   -> CreateBlobF c (fmap f k)
        CreateCommitF c r k -> CreateCommitF c r (fmap f k)
        CreateTagF t r k    -> CreateTagF t r (fmap f k)
-}

pattern ParseOid s k = Step (ParseOidF s k)

pattern CreateReference r t s = Step (CreateReferenceF r t s)
pattern LookupReference r k   = Step (LookupReferenceF r k)
pattern DeleteReference r s   = Step (DeleteReferenceF r s)
pattern AllReferences k       = Step (AllReferencesF k)

pattern LookupObject o k   = Step (LookupObjectF o k)
pattern ExistsObject o k   = Step (ExistsObjectF o k)
pattern AllObjects x y z k = Step (AllObjectsF x y z k)

pattern LookupCommit o k = Step (LookupCommitF o k)
pattern LookupTree o k   = Step (LookupTreeF o k)
pattern LookupBlob o k   = Step (LookupBlobF o k)
pattern LookupTag o k    = Step (LookupTagF o k)

pattern ReadIndex k    = Step (ReadIndexF k)
pattern WriteIndex t s = Step (WriteIndexF t s)

pattern NewTreeBuilder t k = Step (NewTreeBuilderF t k)
pattern TreeOid t k        = Step (TreeOidF t k)
pattern GetTreeEntry t p k = Step (GetTreeEntryF t p k)
pattern AllTreeEntries t k = Step (AllTreeEntriesF t k)

pattern DiffContentsWithTree p t k = Step (DiffContentsWithTreeF p t k)

pattern HashContents c k   = Step (HashContentsF c k)
pattern CreateBlob c k     = Step (CreateBlobF c k)
pattern CreateCommit c r k = Step (CreateCommitF c r k)
pattern CreateTag t r k    = Step (CreateTagF t r k)

type GitT r m = Stream (GitF r m) m

parseOid :: Monad m => String -> GitT r m (Oid r)
parseOid str = ParseOid str return

createReference :: Monad m => RefName -> RefTarget r -> GitT r m ()
createReference name target = CreateReference name target (pure ())

lookupReference :: Monad m => RefName -> GitT r m (Maybe (RefTarget r))
lookupReference name = LookupReference name return

deleteReference :: Monad m => RefName -> GitT r m ()
deleteReference name = DeleteReference name (pure ())

allReferences' :: Monad m => GitT r m (Stream (Of RefName) m ())
allReferences' = AllReferences return

allReferences :: Monad m => Stream (Of RefName) (GitT r m) ()
allReferences = hoist lift =<< lift allReferences'

printReferences :: MonadIO m => GitT r m ()
printReferences = S.print allReferences

lookupObject :: Monad m => Oid r -> GitT r m (Maybe (Object r))
lookupObject o = LookupObject o return

existsObject :: Monad m => Oid r -> GitT r m (Maybe (ObjectOid r))
existsObject o = ExistsObject o return

allObjects' :: Monad m => Maybe (Oid r) -> Oid r -> Bool
            -> GitT r m (Stream (Of (ObjectOid r)) m ())
allObjects' mhave need trees = AllObjects mhave need trees return

allObjects :: Monad m
           => Maybe (Oid r) -> Oid r -> Bool
           -> Stream (Of (ObjectOid r)) (GitT r m) ()
allObjects mhave need trees = hoist lift =<< lift (allObjects' mhave need trees)

lookupCommit :: Monad m => Oid r -> GitT r m (Maybe (Commit r))
lookupCommit o = LookupCommit o return

lookupTree :: Monad m => Oid r -> GitT r m (Maybe (Tree r))
lookupTree o = LookupTree o return

lookupBlob :: Monad m => Oid r -> GitT r m (Maybe (Stream (Of ByteString) m ()))
lookupBlob o = LookupBlob o return

lookupTag :: Monad m => Oid r -> GitT r m (Maybe (Tag r))
lookupTag o = LookupTag o return

readIndex :: Monad m => GitT r m (TreeT r m ())
readIndex = ReadIndex return

writeIndex :: Monad m => TreeT r m () -> GitT r m ()
writeIndex t = WriteIndex t (pure ())

newTreeBuilder :: Monad m => Maybe (Tree r) -> GitT r m (TreeBuilder r m)
newTreeBuilder mt = NewTreeBuilder mt return

treeOid :: Monad m => Tree r -> GitT r m (Oid r)
treeOid t = TreeOid t return

getTreeEntry :: Monad m
             => Tree r -> TreeFilePath -> GitT r m (Maybe (TreeEntry r))
getTreeEntry t path = GetTreeEntry t path return

allTreeEntries' :: Monad m
                => Tree r
                -> GitT r m (Stream (Of (TreeFilePath, TreeEntry r)) m ())
allTreeEntries' t = AllTreeEntries t return

allTreeEntries :: Monad m
               => Tree r -> Stream (Of (TreeFilePath, TreeEntry r)) (GitT r m) ()
allTreeEntries t = hoist lift =<< lift (allTreeEntries' t)

diffContentsWithTree' :: Monad m
                      => [Either TreeFilePath ByteString]
                      -> Tree r
                      -> GitT r m (Stream (Of ByteString) m ())
diffContentsWithTree' contents t = DiffContentsWithTree contents t return

diffContentsWithTree :: Monad m
                     => [Either TreeFilePath ByteString]
                     -> Tree r
                     -> Stream (Of ByteString) (GitT r m) ()
diffContentsWithTree contents t =
    hoist lift =<< lift (diffContentsWithTree' contents t)

hashContents :: Monad m => BL.ByteString -> GitT r m (Oid r)
hashContents contents = HashContents contents return

createBlob :: Monad m => BL.ByteString -> GitT r m (Oid r)
createBlob contents = CreateBlob contents return

emptyTreeId :: String
emptyTreeId = "4b825dc642cb6eb9a060e54bf8d69288fbee4904"

commitObj :: Monad m
          => [Oid r]
          -> Oid r
          -> Signature
          -> Signature
          -> CommitMessage
          -> Text
          -> GitT r m (Commit r)
commitObj a b c d e f = do
    oid <- parseOid emptyTreeId
    return $ Commit oid a b c d e f

createCommit :: Monad m => Commit r -> Maybe RefName -> GitT r m (Oid r)
createCommit c mname = CreateCommit c mname return

createTag :: Monad m => Tag r -> RefName -> GitT r m (Oid r)
createTag t name = CreateTag t name return

-- Utility functions; jww (2015-06-14): these belong elsewhere

copyOid :: (Monad m, Repository r) => Oid r -> GitT s m (Oid s)
copyOid = parseOid . show

objectOid :: Monad m => Object r -> GitT r m (Oid r)
objectOid (BlobObj oid)   = return oid
objectOid (TreeObj obj)   = treeOid obj
objectOid (CommitObj obj) = return $ commitOid obj
objectOid (TagObj obj)    = return $ tagOid obj

copyMergeResult :: (Monad m, Repository s)
                => MergeResult s -> GitT r m (MergeResult r)
copyMergeResult (MergeSuccess mc) = MergeSuccess <$> parseOid (show mc)
copyMergeResult (MergeConflicted hl hr mc cs) =
    MergeConflicted <$> parseOid (show hl)
                    <*> parseOid (show hr)
                    <*> parseOid (show mc)
                    <*> pure cs
