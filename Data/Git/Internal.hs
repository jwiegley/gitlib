{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Git.Internal
       ( ObjPtr

       , Updatable(..)

       , Base(..), gitId, gitRepo, gitObj
       , newBase

       , Repository
       , HasRepository(..)

       , openRepository
       , createRepository
       , openOrCreateRepository
       , repositoryPtr

       , lookupObject'

       , module F
       , module X )
       where

import           Bindings.Libgit2 as X
import           Control.Applicative as X
import           Control.Exception as X
import           Control.Lens as X
import           Control.Monad as X hiding (mapM, mapM_, sequence, sequence_,
                                            forM, forM_, msum)
import           Data.Either as X hiding (lefts, rights)
import           Data.Foldable as X
import           Data.Git.Errors as X
import           Data.Git.Oid as X
import           Data.Maybe as X
import           Data.Monoid as X
import           Data.Stringable as X
import           Data.Text as T hiding (map)
import           Data.Traversable as X
import           Filesystem as X hiding (createTree)
import qualified Filesystem.Path.CurrentOS as F hiding((<.>))
import           Filesystem.Path.CurrentOS as X hiding ((<.>), empty, concat,
                                                        toText, fromText)
import           Foreign.C.String as X
import           Foreign.C.Types as X
import           Foreign.ForeignPtr as X
import           Foreign.Marshal.Alloc as X
import           Foreign.Marshal.Utils as X
import           Foreign.Ptr as X
import           Foreign.StablePtr as X
import           Foreign.Storable as X
import           Prelude hiding (FilePath, mapM, mapM_, sequence, sequence_)
import           Unsafe.Coerce as X

default (Text)

type ObjPtr a = Maybe (ForeignPtr a)

class Updatable a where
  getId      :: a -> Ident a
  objectRepo :: a -> Repository
  objectPtr  :: a -> ObjPtr C'git_object

  update  :: a -> IO a
  update_ :: a -> IO ()
  update_ = void . update

  objectId :: a -> IO Oid
  objectId x = case getId x of
    Pending f -> Oid <$> f x
    Stored y  -> return $ Oid y

  maybeObjectId :: a -> Maybe Oid
  maybeObjectId x = case getId x of
    Pending _ -> Nothing
    Stored y  -> Just (Oid y)

  lookupFunction :: Repository -> Oid -> IO (Maybe a)

  loadObject :: Updatable b => ObjRef a -> b -> IO (Maybe a)
  loadObject (IdRef coid) y = lookupFunction (objectRepo y) (Oid coid)
  loadObject (ObjRef x) _ = return (Just x)

  getObject :: ObjRef a -> Maybe a
  getObject (IdRef _)  = Nothing
  getObject (ObjRef x) = Just x

data Repository = Repository { _repoPath :: FilePath
                             , _repoObj  :: ObjPtr C'git_repository }

makeClassy ''Repository

instance Show Repository where
  show x = "Repository " <> toString (x^.repoPath)

data Base a = Base { _gitId   :: Ident a
                   , _gitRepo :: Repository
                   , _gitObj  :: ObjPtr C'git_object }

makeLenses ''Base

instance Show (Base a) where
  show x = case x^.gitId of
    Pending _ -> "Base"
    Stored y  -> "Base#" ++ show y

newBase :: Repository -> Ident a -> ObjPtr C'git_object -> Base a
newBase repo oid obj = Base { _gitId   = oid
                            , _gitRepo = repo
                            , _gitObj  = obj }

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
  case F.toText path of
    Left p  -> doesNotExist p
    Right p ->
      withCStringable p $ \str -> do
        r <- fn ptr str
        when (r < 0) $ doesNotExist p
        ptr' <- peek ptr
        fptr <- newForeignPtr p'git_repository_free ptr'
        return Repository { _repoPath = path
                          , _repoObj  = Just fptr }

  where doesNotExist = throwIO . RepositoryNotExist . toString

lookupObject'
  :: Repository -> Oid
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> CUInt -> IO CInt)
  -> (COid -> ForeignPtr C'git_object -> Ptr C'git_object -> IO b)
  -> IO (Maybe b)
lookupObject' repo oid lookupFn lookupPrefixFn createFn = alloca $ \ptr -> do
  r <- withForeignPtr (repositoryPtr repo) $ \repoPtr ->
         case oid of
           Oid (COid oid') ->
             withForeignPtr oid' $ \oidPtr ->
               lookupFn (castPtr ptr) repoPtr oidPtr
           PartialOid (COid oid') len ->
             withForeignPtr oid' $ \oidPtr ->
               lookupPrefixFn (castPtr ptr) repoPtr oidPtr (fromIntegral len)
  if r < 0
    then return Nothing
    else do
      ptr'     <- peek ptr
      coid     <- c'git_object_id ptr'
      coidCopy <- mallocForeignPtr
      withForeignPtr coidCopy $ flip c'git_oid_cpy coid

      fptr <- newForeignPtr p'git_object_free ptr'
      Just <$> createFn (COid coidCopy) fptr ptr'

-- Internal.hs
