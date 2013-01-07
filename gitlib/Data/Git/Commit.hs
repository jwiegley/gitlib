{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Commit
       ( Commit(..)
       , newCommitBase
       , createCommit
       , lookupCommit
       , writeCommit
       , getCommitParents
       , modifyCommitTree
       , removeFromCommitTree
       , updateCommit
       , commitHistoryFirstParent
       , commitEntry
       , commitEntryHistory )
       where

import           Bindings.Libgit2
import qualified Data.ByteString as BS
import           Data.ByteString.Unsafe
import           Data.Git.Common
import           Data.Git.Internal
import           Data.Git.Tree
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as U
import qualified Foreign.Concurrent as FC
import qualified Foreign.ForeignPtr.Unsafe as FU
import           Foreign.Marshal.Array
import qualified Prelude

default (Text)

data Commit = Commit { commitInfo      :: Base Commit
                     , commitAuthor    :: Signature
                     , commitCommitter :: Signature
                     , commitLog       :: Text
                     , commitEncoding  :: Prelude.String
                     , commitTree      :: ObjRef Tree
                     , commitParents   :: [ObjRef Commit]
                     , commitObj       :: ObjPtr C'git_commit }

instance Show Commit where
  show x = case gitId (commitInfo x) of
    Pending _ -> "Commit..."
    Stored y  -> "Commit#" ++ show y ++ " <" ++ show (commitTree x) ++ ">"

instance Updatable Commit where
  getId x        = gitId (commitInfo x)
  objectRepo x   = gitRepo (commitInfo x)
  objectPtr x    = gitObj (commitInfo x)
  update         = writeCommit Nothing
  lookupFunction = lookupCommit

newCommitBase :: Commit -> Base Commit
newCommitBase t =
  newBase (gitRepo (commitInfo t))
          (Pending (doWriteCommit Nothing >=> return . snd)) Nothing

-- | Create a new, empty commit.
--
--   Since empty commits cannot exist in Git, attempting to write out an empty
--   commit is a no-op.
createCommit :: Repository -> Commit
createCommit repo =
  Commit { commitInfo     =
           newBase repo (Pending (doWriteCommit Nothing >=> return . snd))
                   Nothing
         , commitAuthor    = createSignature
         , commitCommitter = createSignature
         , commitTree      = ObjRef (createTree repo)
         , commitParents   = []
         , commitLog       = T.empty
         , commitEncoding  = ""
         , commitObj       = Nothing }

lookupCommit :: Oid -> Repository -> IO (Maybe Commit)
lookupCommit oid repo =
  lookupObject' oid repo c'git_commit_lookup c'git_commit_lookup_prefix $
    \coid obj _ ->
      withForeignPtr obj $ \cobj -> do
        let c = castPtr cobj

        enc   <- c'git_commit_message_encoding c
        encs  <- if enc == nullPtr
                then return "UTF-8"
                else peekCString enc
        conv  <- U.open encs (Just False)

        msg   <- c'git_commit_message c   >>= BS.packCString
        auth  <- c'git_commit_author c    >>= packSignature conv
        comm  <- c'git_commit_committer c >>= packSignature conv
        toid  <- c'git_commit_tree_oid c  >>= wrapOidPtr

        pn    <- c'git_commit_parentcount c
        poids <- traverse wrapOidPtr
                =<< sequence
                      (zipWith ($) (replicate (fromIntegral (toInteger pn))
                                              (c'git_commit_parent_oid c))
                                   [0..pn])

        return Commit { commitInfo      = newBase repo (Stored coid) (Just obj)
                      , commitAuthor    = auth
                      , commitCommitter = comm
                      , commitTree      = toid
                      , commitParents   = poids
                      , commitLog       = U.toUnicode conv msg
                      , commitEncoding  = encs
                      , commitObj       = Just $ unsafeCoerce obj }

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
writeCommit :: Maybe Text -> Commit -> IO Commit
writeCommit _ c@(Commit { commitInfo = Base { gitId = Stored _ } }) =
  return c
writeCommit ref c = fst <$> doWriteCommit ref c

doWriteCommit :: Maybe Text -> Commit -> IO (Commit, COid)
doWriteCommit ref c = do
  coid <- withForeignPtr repo $ \repoPtr -> do
    coid <- mallocForeignPtr
    withForeignPtr coid $ \coid' -> do
      conv <- U.open (commitEncoding c) (Just True)
      BS.useAsCString (U.fromUnicode conv (commitLog c)) $ \message ->
        withRef ref $ \update_ref ->
          withSignature conv (commitAuthor c) $ \author ->
            withSignature conv (commitCommitter c) $ \committer ->
              withEncStr (commitEncoding c) $ \message_encoding ->
                withGitTree (commitTree c) c $ \commit_tree -> do
                  parentPtrs <- getCommitParentPtrs c
                  parents    <- newArray $
                               map FU.unsafeForeignPtrToPtr parentPtrs
                  r <- c'git_commit_create coid' repoPtr
                        update_ref author committer
                        message_encoding message commit_tree
                        (fromIntegral (length (commitParents c)))
                        parents
                  when (r < 0) $ throwIO CommitCreateFailed
                  return coid

  return (c { commitInfo = (commitInfo c) { gitId = Stored (COid coid) } }
         , COid coid)

  where
    repo = fromMaybe (error "Repository invalid")
                     (repoObj (gitRepo (commitInfo c)))

    withRef name =
      if isJust name
      then unsafeUseAsCString (E.encodeUtf8 (fromJust name))
      else flip ($) nullPtr

    withEncStr enc =
      if null enc
      then flip ($) nullPtr
      else withCString enc

getCommitParents :: Commit -> IO [Commit]
getCommitParents c =
  traverse (\p -> do parent <- loadObject p c
                     case parent of
                       Nothing -> error "Cannot find Git commit"
                       Just p' -> return p')
           (commitParents c)

getCommitParentPtrs :: Commit -> IO [ForeignPtr C'git_commit]
getCommitParentPtrs c =
  withForeignPtr (repositoryPtr (objectRepo c)) $ \repoPtr ->
    for (commitParents c) $ \p ->
      case p of
        ObjRef (Commit { commitObj = Just obj }) -> return obj
        _ -> do
          Oid (COid oid) <-
            case p of
              IdRef coid -> return $ Oid coid
              ObjRef x   -> objectId x

          withForeignPtr oid $ \commit_id ->
            alloca $ \ptr -> do
              r <- c'git_commit_lookup ptr repoPtr commit_id
              when (r < 0) $ throwIO CommitLookupFailed
              ptr' <- peek ptr
              FC.newForeignPtr ptr' (c'git_commit_free ptr')

modifyCommitTree
  :: FilePath -> (Maybe TreeEntry -> Either a (Maybe TreeEntry)) -> Bool
  -> Commit -> IO (Either a Commit)
modifyCommitTree path f createIfNotExist c =
  withObject (commitTree c) c $ \tr -> do
    result <- modifyTree path f createIfNotExist tr
    case result of
      Left x    -> return (Left x)
      Right tr' -> return $ Right $ c { commitTree = ObjRef tr' }

removeFromCommitTree :: FilePath -> Commit -> IO Commit
removeFromCommitTree path c =
  withObject (commitTree c) c $ \tr -> do
    tr' <- removeFromTree path tr
    return c { commitTree = ObjRef tr' }

doUpdateCommit :: [Text] -> TreeEntry -> Commit -> IO Commit
doUpdateCommit xs item c = do
  t <- loadObject (commitTree c) c
  case t of
    Nothing -> error "Failed to load tree for commit"
    Just t' -> do
      tr <- doModifyTree xs (const (Right (Just item))) True t'
      case tr of
        Right tr' -> return c { commitTree = ObjRef tr' }
        _ -> undefined

updateCommit :: FilePath -> TreeEntry -> Commit -> IO Commit
updateCommit = doUpdateCommit . splitPath

commitHistoryFirstParent :: Commit -> IO [Commit]
commitHistoryFirstParent c =
  case commitParents c of
    []    -> return [c]
    (p:_) -> do p' <- loadObject' p c
                ps <- commitHistoryFirstParent p'
                return (c:ps)

commitEntry :: FilePath -> Commit -> IO (Maybe TreeEntry)
commitEntry path c = lookupTreeEntry path =<< loadObject' (commitTree c) c

commitEntryD :: FilePath -> Commit -> IO (Maybe TreeEntry)
commitEntryD path c = do
  let tr = commitTree c
  Prelude.putStrLn $ "commitEntryD.1: " ++ show tr
  tr' <- loadObject' tr c
  Prelude.putStrLn $ "commitEntryD.2: " ++ show tr'
  te <- lookupTreeEntry path tr'
  Prelude.putStrLn $ "commitEntryD.3: " ++ show te
  return te

mapMaybeM :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM p xs = catMaybes <$> mapM p xs

identifyEntries :: [TreeEntry] -> IO [(Oid,TreeEntry)]
identifyEntries = mapM go
  where go x = do
          oid <- case x of
                  BlobEntry blob _ -> objectRefId blob
                  TreeEntry tree   -> objectRefId tree
          return (oid,x)

commitEntryHistory :: FilePath -> Commit -> IO [(Oid,TreeEntry)]
commitEntryHistory path c = do
  cs <- commitHistoryFirstParent c
  map head . filter (not . null) . groupBy ((==) `on` fst) <$>
    (identifyEntries =<< mapMaybeM (commitEntry path) cs)

-- Commit.hs
