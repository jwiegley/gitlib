{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git where

import Bindings.Libgit2
import Control.Applicative
import Control.Exception
import Control.Lens
import Control.Monad
import Data.ByteString as B hiding (map)
import Data.ByteString.Unsafe
import Data.Either
import Data.Map as M hiding (map)
import Data.Maybe
import Data.Text as T hiding (map)
import Data.Time
import Data.Typeable
import Filesystem.Path.CurrentOS
import Foreign.C.String
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.Storable
import Prelude hiding (FilePath)
import Unsafe.Coerce

default (Text)

type Hash     = C'git_oid
type Ident a  = Either (a -> IO C'git_oid) Hash
type ObjPtr a = Maybe (ForeignPtr a)

data Author = Author { _authorName  :: Text
                     , _authorEmail :: Text }
            deriving (Show, Eq)

makeClassy ''Author

data WhoWhen = WhoWhen { _whoAuthor        :: Author
                       , _whoAuthorDate    :: UTCTime
                       , _whoCommitter     :: Author
                       , _whoCommitterDate :: UTCTime }
           deriving (Show, Eq)

makeClassy ''WhoWhen

data Repository = Repository { _repoPath :: FilePath
                             , _repoObj  :: ObjPtr C'git_repository }

makeClassy ''Repository

instance Show Repository where
  show x = T.unpack $
           T.append "Repository " (either id id (toText (x^.repoPath)))

data GitException = RepositoryNotExist String
                  | RepositoryInvalid
                  | BlobCreateFailed
                  deriving (Show, Typeable)

instance Exception GitException

data Base a = Base { _gitId   :: Ident a
                   , _gitRepo :: Repository }

makeClassy ''Base

instance Show (Base a) where
  show x = case x^.gitId of
    Left _  -> "Base"
    Right y -> "Base#" ++ show y

data Blob = Blob { _blobInfo     :: Base Blob
                 , _blobContents :: B.ByteString }

makeClassy ''Blob

instance Show Blob where
  show x = case x^.blobInfo^.gitId of
    Left _  -> "Blob"
    Right y -> "Blob#" ++ show y

type TreeOrBlob = Either Blob Tree
type TreeMap    = Map Text TreeOrBlob

data Tree = Tree { _treeInfo     :: Base Tree
                 , _treeContents :: TreeMap
                 , _treeObj      :: ObjPtr C'git_tree }

makeClassy ''Tree

instance Show Tree where
  show x = case x^.treeInfo^.gitId of
    Left _  -> "Tree"
    Right y -> "Tree#" ++ show y

data Commit = Commit { _commitInfo :: Base Commit
                     , _commitWho  :: WhoWhen
                     , _commitLog  :: Text
                     , _commitTree :: Tree
                     , _commitObj  :: ObjPtr C'git_commit }

makeClassy ''Commit

instance Show Commit where
  show x = case x^.commitInfo^.gitId of
    Left _  -> "Commit"
    Right y -> "Commit#" ++ show y

data Tag = Tag { _tagInfo :: Base Tag
               , _tagRef  :: Hash }

makeClassy ''Tag

instance Show Tag where
  show x = case x^.tagInfo^.gitId of
    Left _  -> "Tag"
    Right y -> "Tag#" ++ show y

data Object = BlobO   Blob
            | TreeO   Tree
            | CommitO Commit
            | TagO    Tag

openRepository :: FilePath -> IO Repository
openRepository path = alloca $ \ptr ->
  case T.unpack <$> toText path of
    Left p  -> throwIO (RepositoryNotExist (T.unpack p))
    Right p ->
      withCString p $ \str -> do
        r <- c'git_repository_open ptr str
        when (r < 0) $ throwIO (RepositoryNotExist p)
        ptr' <- peek ptr
        fptr <- newForeignPtr p'git_repository_free ptr'
        return Repository { _repoPath = path
                          , _repoObj  = Just fptr }

newBase :: Repository -> (a -> IO Hash) -> Base a
newBase repo f = Base { _gitId   = Left f
                      , _gitRepo = repo }

newBlobBase :: Blob -> Base Blob
newBlobBase b = newBase (b^.blobInfo^.gitRepo) doWriteBlob

-- | Create a new blob, starting it with the contents at the given path.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one.
createBlob :: Repository -> B.ByteString -> Blob
createBlob repo text = Blob { _blobInfo     = newBase repo doWriteBlob
                            , _blobContents = text }

writeBlob :: Blob -> IO Blob
writeBlob b = do
  hash <- doWriteBlob b
  return Blob { _blobInfo =
                   Base { _gitId   = Right hash
                        , _gitRepo = b^.blobInfo^.gitRepo }
              , _blobContents = B.empty }

doWriteBlob :: Blob -> IO Hash
doWriteBlob b = alloca $ \ptr -> do
  r <- withForeignPtr
         (fromMaybe (error "Repository invalid")
                    (b^.blobInfo^.gitRepo^.repoObj))
         (\repo ->
             unsafeUseAsCStringLen (b^.blobContents) $
               uncurry (\cstr len ->
                         c'git_blob_create_frombuffer
                           ptr repo (unsafeCoerce cstr :: Ptr ())
                           (fromIntegral len)))
  when (r < 0) $ throwIO BlobCreateFailed
  peek ptr

newTreeBase :: Tree -> Base Tree
newTreeBase t = newBase (t^.treeInfo^.gitRepo) doWriteTree

-- | Create a new tree, starting it with the contents at the given path.
--
--   Note that since empty trees cannot exist in Git, no means is provided for
--   creating one.
createTree :: Repository -> FilePath -> TreeOrBlob -> Tree
createTree repo path item = updateTree path item (emptyTree repo)

doWriteTree :: Tree -> IO Hash
doWriteTree = undefined
{-
  alloca $ \ptr ->
  tb <- c'git_treebuilder_create ptr str
        when (r < 0) $ throwIO (RepositoryNotExist p)
        ptr' <- peek ptr
        let finalizer = newForeignPtr p'git_repository_free ptr'
        return $ Repository { _repoPath = path
                            , _repoObj  = finalizer }
-}

emptyTree :: Repository -> Tree
emptyTree repo = Tree { _treeInfo     = newBase repo doWriteTree
                      , _treeContents = M.empty
                      , _treeObj      = Nothing }

doUpdateTree :: [Text] -> TreeOrBlob -> Tree -> Tree
doUpdateTree (x:xs) item t =
  treeInfo     .~ newTreeBase t $
  treeContents .~ update' xs    $ t

  where repo       = t^.treeInfo^.gitRepo
        treeMap    = t^.treeContents
        update' [] = insert x item treeMap
        update' _  = insert x subTree treeMap
        subTree    = Right $ doUpdateTree xs item tree'
        tree'      = case M.lookup x treeMap of
                       Just (Right m) -> m
                       _ -> emptyTree repo
doUpdateTree [] _ _ = undefined

updateTree :: FilePath -> TreeOrBlob -> Tree -> Tree
updateTree = doUpdateTree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = splitOn "/" text
  where text = case toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

-- Git.hs
