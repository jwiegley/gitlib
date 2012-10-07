{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Commit where

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

data Commit = Commit { _commitInfo      :: Base Commit
                     , _commitAuthor    :: Signature
                     , _commitCommitter :: Signature
                     , _commitLog       :: Text
                     , _commitEncoding  :: Prelude.String
                     , _commitTree      :: ObjRef Tree
                     , _commitParents   :: [ObjRef Commit]
                     , _commitObj       :: ObjPtr C'git_commit }

makeClassy ''Commit

instance Show Commit where
  show x = case x^.commitInfo.gitId of
    Pending _ -> "Commit"
    Stored y  -> "Commit#" ++ show y

instance Updatable Commit where
  getId x        = x^.commitInfo.gitId
  objectRepo x   = x^.commitInfo.gitRepo
  objectPtr x    = x^.commitInfo.gitObj
  update         = writeCommit Nothing
  lookupFunction = lookupCommit

newCommitBase :: Commit -> Base Commit
newCommitBase t =
  newBase (t^.commitInfo.gitRepo)
          (Pending (doWriteCommit Nothing >=> return . snd)) Nothing

-- | Create a new, empty commit.
--
--   Since empty commits cannot exist in Git, attempting to write out an empty
--   commit is a no-op.
createCommit :: Repository -> Commit
createCommit repo =
  Commit { _commitInfo     =
            newBase repo (Pending (doWriteCommit Nothing >=> return . snd))
                    Nothing
         , _commitAuthor    = createSignature
         , _commitCommitter = createSignature
         , _commitTree      = ObjRef (createTree repo)
         , _commitParents   = []
         , _commitLog       = T.empty
         , _commitEncoding  = ""
         , _commitObj       = Nothing }

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

        return Commit { _commitInfo      = newBase repo (Stored coid) (Just obj)
                      , _commitAuthor    = auth
                      , _commitCommitter = comm
                      , _commitTree      = toid
                      , _commitParents   = poids
                      , _commitLog       = U.toUnicode conv msg
                      , _commitEncoding  = encs
                      , _commitObj       = Just $ unsafeCoerce obj }

withGitCommit :: Updatable b
              => ObjRef Commit -> b -> (Ptr C'git_commit -> IO a) -> IO a
withGitCommit cref obj f = do
  c' <- loadObject cref obj
  case c' of
    Nothing -> error "Cannot find Git commit in repository"
    Just co ->
      case co^.commitInfo.gitObj of
        Nothing  -> error "Cannot find Git commit id"
        Just co' -> withForeignPtr co' (f . castPtr)

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
writeCommit :: Maybe Text -> Commit -> IO Commit
writeCommit _ c@(Commit { _commitInfo = Base { _gitId = Stored _ } }) =
  return c
writeCommit ref c = fst <$> doWriteCommit ref c

doWriteCommit :: Maybe Text -> Commit -> IO (Commit, COid)
doWriteCommit ref c = do
  coid <- withForeignPtr repo $ \repoPtr -> do
    coid <- mallocForeignPtr
    withForeignPtr coid $ \coid' -> do
      conv <- U.open (c^.commitEncoding) (Just True)
      BS.useAsCString (U.fromUnicode conv (c^.commitLog)) $ \message ->
        withRef ref $ \update_ref ->
          withSignature conv (c^.commitAuthor) $ \author ->
            withSignature conv (c^.commitCommitter) $ \committer ->
              withEncStr (c^.commitEncoding) $ \message_encoding ->
                withGitTree (c^.commitTree) c $ \commit_tree -> do
                  parentPtrs <- getCommitParentPtrs c
                  parents    <- newArray $
                               map FU.unsafeForeignPtrToPtr parentPtrs
                  r <- c'git_commit_create coid' repoPtr
                        update_ref author committer
                        message_encoding message commit_tree
                        (fromIntegral (length (c^.commitParents)))
                        parents
                  when (r < 0) $ throwIO CommitCreateFailed
                  return coid

  return (commitInfo.gitId .~ Stored (COid coid) $ c, COid coid)

  where
    repo = fromMaybe (error "Repository invalid")
                     (c^.commitInfo.gitRepo.repoObj)

    withRef refName =
      if isJust refName
      then unsafeUseAsCString (E.encodeUtf8 (fromJust refName))
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
           (c^.commitParents)

getCommitParentPtrs :: Commit -> IO [ForeignPtr C'git_commit]
getCommitParentPtrs c =
  withForeignPtr (repositoryPtr (objectRepo c)) $ \repoPtr ->
    for (c^.commitParents) $ \p ->
      case p of
        ObjRef (Commit { _commitObj = Just obj }) -> return obj
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

doUpdateCommit :: [Text] -> TreeEntry -> Commit -> IO Commit
doUpdateCommit xs item c = do
  t <- loadObject (c^.commitTree) c
  case t of
    Nothing -> error "Failed to load tree for commit"
    Just t' -> do
      tr <- doModifyTree xs (const (Right (Just item))) True t'
      case tr of
        Right tr' -> return $ commitTree .~ ObjRef tr' $ c
        _ -> undefined

updateCommit :: FilePath -> TreeEntry -> Commit -> IO Commit
updateCommit = doUpdateCommit . splitPath

-- Commit.hs
