{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Internal
       ( Oid
       , Ident
       , ObjPtr

       , Base(..), gitId, gitRepo
       , newBase

       , Repository
       , HasRepository(..)

       , openRepository
       , createRepository
       , openOrCreateRepository
       , repositoryPtr

       , lookupObject'

       , module X )
       where

import Bindings.Libgit2 as X
import Control.Applicative as X
import Control.Exception as X
import Control.Lens as X hiding((<.>))
import Control.Monad as X hiding (mapM, mapM_, sequence, sequence_,
                                  forM, forM_, msum)
import Data.Either as X
import Data.Foldable as X
import Data.Git.Errors as X
import Data.Maybe as X
import Data.Monoid as X
import Data.Text as T hiding (map)
import Data.Traversable as X
import Filesystem as X
import Filesystem.Path.CurrentOS as X hiding (empty, concat)
import Foreign.C.String as X
import Foreign.C.Types as X
import Foreign.ForeignPtr as X
import Foreign.Marshal.Alloc as X
import Foreign.Marshal.Utils as X
import Foreign.Ptr as X
import Foreign.StablePtr as X
import Foreign.Storable as X
import Prelude hiding (FilePath, mapM, mapM_, sequence, sequence_)
import Unsafe.Coerce as X

default (Text)

type Oid      = ForeignPtr C'git_oid
type Ident a  = Either (a -> IO Oid) Oid
type ObjPtr a = Maybe (ForeignPtr a)

data Repository = Repository { _repoPath :: FilePath
                             , _repoObj  :: ObjPtr C'git_repository }

makeClassy ''Repository

instance Show Repository where
  show x = T.unpack $
           T.append "Repository " (either id id (toText (x^.repoPath)))

data Base a = Base { _gitId   :: Ident a
                   , _gitRepo :: Repository }

makeLenses ''Base

instance Show (Base a) where
  show x = case x^.gitId of
    Left _  -> "Base"
    Right y -> "Base#" ++ show y

newBase :: Repository -> (a -> IO Oid) -> Base a
newBase repo f = Base { _gitId   = Left f
                      , _gitRepo = repo }

repositoryPtr :: Repository -> ForeignPtr C'git_repository
repositoryPtr repo = fromMaybe (error "Repository invalid") (repo^.repoObj)

openRepository :: FilePath -> IO Repository
openRepository path =
  openRepositoryWith path c'git_repository_open

createRepository :: FilePath -> Bool -> IO Repository
createRepository path bare =
  openRepositoryWith path (\x y -> c'git_repository_init x y (fromBool bare))

openOrCreateRepository :: FilePath -> Bool -> IO Repository
openOrCreateRepository path bare = do
  b <- isDirectory path
  if b
    then openRepository path
    else createRepository path bare

openRepositoryWith :: FilePath
                   -> (Ptr (Ptr C'git_repository) -> CString -> IO CInt)
                   -> IO Repository
openRepositoryWith path fn = alloca $ \ptr ->
  case T.unpack <$> toText path of
    Left p  -> throwIO (RepositoryNotExist (T.unpack p))
    Right p ->
      withCString p $ \str -> do
        r <- fn ptr str
        when (r < 0) $ throwIO (RepositoryNotExist p)
        ptr' <- peek ptr
        fptr <- newForeignPtr p'git_repository_free ptr'
        return Repository { _repoPath = path
                          , _repoObj  = Just fptr }

lookupObject'
  :: Repository -> Oid
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (ForeignPtr C'git_object -> Ptr C'git_object -> IO b)
  -> IO (Maybe b)
lookupObject' repo oid lookupFn createFn = alloca $ \ptr -> do
  r <- withForeignPtr (repositoryPtr repo) $ \repoPtr ->
         withForeignPtr oid $ \oidPtr ->
           lookupFn (castPtr ptr) repoPtr oidPtr
  if (r < 0)
    then return Nothing
    else do
      ptr' <- peek ptr
      fptr <- newForeignPtr p'git_object_free ptr'
      Just <$> createFn fptr ptr'

-- Internal.hs
