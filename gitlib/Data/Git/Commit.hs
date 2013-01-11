{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Commit
       ( Commit(..)
       , PinnedEntry(..)
       , newCommitBase
       , createCommit
       , lookupCommit
       , lookupRefCommit
       , addCommitParent
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
import           Data.Git.Reference
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Text.ICU.Convert as U
import qualified Foreign.Concurrent as FC
import qualified Foreign.ForeignPtr.Unsafe as FU
import           Foreign.Marshal.Array
import qualified Prelude

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
  update         = flip writeCommit Nothing
  lookupFunction = lookupCommit
#if defined(PROFILING)
  loadObject' x y =
    maybe (throwIO ObjectLookupFailed) return =<< loadObject x y
#endif

newCommitBase :: Commit -> Base Commit
newCommitBase t =
  newBase (gitRepo (commitInfo t))
          (Pending (flip doWriteCommit Nothing >=> return . snd)) Nothing

-- | Create a new, empty commit.
--
--   Since empty commits cannot exist in Git, attempting to write out an empty
--   commit is a no-op.
createCommit :: Repository -> Signature -> Commit
createCommit repo sig =
  Commit { commitInfo     =
           newBase repo (Pending (flip doWriteCommit Nothing >=> return . snd))
                   Nothing
         , commitAuthor    = sig
         , commitCommitter = sig
         , commitTree      = ObjRef (createTree repo)
         , commitParents   = []
         , commitLog       = T.empty
         , commitEncoding  = ""
         , commitObj       = Nothing }

lookupCommit :: Repository -> Oid -> IO (Maybe Commit)
lookupCommit repo oid =
  lookupObject' repo oid c'git_commit_lookup c'git_commit_lookup_prefix $
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

lookupRefCommit :: Repository -> Text -> IO (Maybe Commit)
lookupRefCommit repo ref = do
  oid <- resolveRef repo ref
  maybe (return Nothing) (lookupCommit repo) oid

addCommitParent :: Commit -> Commit -> Commit
addCommitParent co p = co { commitParents = commitParents co ++ [ObjRef p] }

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
writeCommit :: Commit -> Maybe Text -> IO Commit
writeCommit c@(Commit { commitInfo = Base { gitId = Stored _ } }) _ =
  return c
writeCommit c ref = fst <$> doWriteCommit c ref

doWriteCommit :: Commit -> Maybe Text -> IO (Commit, COid)
doWriteCommit c ref = do
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

    withRef Nothing     = flip ($) nullPtr
    withRef (Just name) = BS.useAsCString (E.encodeUtf8 name)

    withEncStr ""  = flip ($) nullPtr
    withEncStr enc = withCString enc

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
  :: Commit -> FilePath -> (Maybe TreeEntry -> Either a (Maybe TreeEntry)) -> Bool
  -> IO (Either a Commit)
modifyCommitTree c path f createIfNotExist =
  withObject (commitTree c) c $ \tr -> do
    result <- modifyTree path f createIfNotExist tr
    case result of
      Left x    -> return (Left x)
      Right tr' -> return $ Right $ c { commitTree = ObjRef tr' }

removeFromCommitTree :: Commit -> FilePath -> IO Commit
removeFromCommitTree c path =
  withObject (commitTree c) c $ \tr -> do
    tr' <- removeFromTree path tr
    return c { commitTree = ObjRef tr' }

doUpdateCommit :: Commit -> [Text] -> TreeEntry -> IO Commit
doUpdateCommit c xs item = do
  t <- loadObject (commitTree c) c
  case t of
    Nothing -> error "Failed to load tree for commit"
    Just t' -> do
      tr <- doModifyTree xs (const (Right (Just item))) True t'
      case tr of
        Right tr' -> return c { commitTree = ObjRef tr' }
        _ -> undefined

updateCommit :: Commit -> FilePath -> TreeEntry -> IO Commit
updateCommit c = doUpdateCommit c . splitPath

commitHistoryFirstParent :: Commit -> IO [Commit]
commitHistoryFirstParent c =
  case commitParents c of
    []    -> return [c]
    (p:_) -> do p' <- loadObject' p c
                ps <- commitHistoryFirstParent p'
                return (c:ps)

commitEntry :: Commit -> FilePath -> IO (Maybe TreeEntry)
commitEntry c path =
  flip lookupTreeEntry path =<< loadObject' (commitTree c) c

data PinnedEntry = PinnedEntry { pinnedOid    :: Oid
                               , pinnedCommit :: Commit
                               , pinnedEntry  :: TreeEntry }
                 deriving Show

identifyEntry :: Commit -> TreeEntry -> IO PinnedEntry
identifyEntry co x = do
  oid <- case x of
          BlobEntry blob _ -> objectRefId blob
          TreeEntry tree   -> objectRefId tree
  return (PinnedEntry oid co x)

commitEntryHistory :: Commit -> FilePath -> IO [PinnedEntry]
commitEntryHistory c path = map head
                            . filter (not . null)
                            . groupBy ((==) `on` pinnedOid) <$> go c
  where go co = do
          entry <- getEntry co
          rest  <- case commitParents co of
            []    -> return []
            (p:_) -> go =<< loadObject' p co
          return $ maybe rest (:rest) entry

        getEntry co = do
          ce <- commitEntry co path
          case ce of
            Nothing  -> return Nothing
            Just ce' -> Just <$> identifyEntry co ce'

-- Commit.hs
