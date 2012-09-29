{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Commit where

import           Bindings.Libgit2
import           Control.Lens
import qualified Data.ByteString as BS
import           Data.Git.Common
import           Data.Git.Internal
import           Data.Git.Tree
import qualified Data.Text.ICU.Convert as U
import qualified Prelude

default (Text)

data Commit = Commit { _commitInfo    :: Base Commit
                     , _commitWho     :: WhoWhen
                     , _commitLog     :: Text
                     , _commitTree    :: Tree
                     , _commitParents :: [ObjRef Commit]
                     , _commitObj     :: ObjPtr C'git_commit }

makeClassy ''Commit

instance Show Commit where
  show x = case x^.commitInfo.gitId of
    Pending _ -> "Commit"
    Stored y  -> "Commit#" ++ show y

instance Updatable Commit where
  getId x        = x^.commitInfo.gitId
  objectRepo x   = x^.commitInfo.gitRepo
  objectPtr x    = x^.commitInfo.gitObj
  update         = writeCommit
  lookupFunction = lookupCommit


newCommitBase :: Commit -> Base Commit
newCommitBase t =
  newBase (t^.commitInfo.gitRepo)
          (Pending (doWriteCommit >=> return . snd)) Nothing

-- | Create a new, empty commit.
--
--   Since empty commits cannot exist in Git, attempting to write out an empty
--   commit is a no-op.
createCommit :: Repository -> Commit
createCommit repo =
  Commit { _commitInfo     =
            newBase repo (Pending (doWriteCommit >=> return . snd)) Nothing }

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
        msg   <- c'git_commit_message c >>= BS.packCString

        tm    <- c'git_commit_time c
        toff  <- c'git_commit_time_offset c
        comm  <- c'git_commit_committer c
        auth  <- c'git_commit_author c
        toid  <- c'git_commit_tree_oid c

        pn    <- c'git_commit_parentcount c
        poids <- (sequence $
                 zipWith ($) (replicate (fromIntegral (toInteger pn))
                                        (c'git_commit_parent_oid c)) [0..pn])
                >>= traverse wrapOidPtr

        return Commit { _commitInfo =
                         newBase repo (Stored coid) (Just obj)
                      , _commitParents = poids
                      , _commitLog     = U.toUnicode conv msg }

-- | Write out a commit to its repository.  If it has already been written,
--   nothing will happen.
writeCommit :: Commit -> IO Commit
writeCommit t@(Commit { _commitInfo = Base { _gitId = Stored _ } }) = return t
writeCommit t = fst <$> doWriteCommit t

doWriteCommit :: Commit -> IO (Commit, COid)
doWriteCommit c = do
  coid <- withForeignPtr repo $ \repoPtr -> do
    coid <- mallocForeignPtr
    withForeignPtr coid $ \coid' -> do
      r <- c'git_commit_create coid' repoPtr
            undefined                     -- const char *update_ref
            undefined                     -- const git_signature *author
            undefined                     -- const git_signature *committer
            undefined                     -- const char *message_encoding
            undefined                     -- const char *message
            undefined                     -- const git_tree *tree
            undefined                     -- int parent_count
            undefined                     -- const git_commit *parents[]
      when (r < 0) $ throwIO CommitCreateFailed
      return coid

  return (commitInfo.gitId .~ Stored (COid coid) $ c, COid coid)

  where
    repo = fromMaybe (error "Repository invalid") $
                     c^.commitInfo.gitRepo.repoObj

-- Commit.hs
