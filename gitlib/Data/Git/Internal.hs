{-# OPTIONS_HADDOCK hide, prune, ignore-exports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Data.Git.Internal
       ( ObjPtr
       , ByteSource

       , Updatable(..)

       , Base(..), newBase

       , Repository(..)
       , openRepository
       , createRepository
       , openOrCreateRepository
       , repositoryPtr

       , lookupObject'
       , withObject
       , withObjectPtr

       , RefTarget(..)
       , Reference(..)

       , module X )
       where

import           Bindings.Libgit2 as X
import           Control.Applicative as X
import           Control.Category as X
import           Control.Exception as X
import           Control.Lens as X
import           Control.Monad as X hiding (mapM, mapM_, sequence, sequence_,
                                            forM, forM_, msum, unless, guard)
import           Data.Bool as X
import           Data.ByteString as B hiding (map)
import           Data.Conduit
import           Data.Either as X hiding (lefts, rights)
import           Data.Foldable as X
import           Data.Function as X hiding ((.), id)
import           Data.Git.Error as X
import           Data.Git.Oid as X
import           Data.List as X hiding (foldl, foldl', foldl1, foldr1, foldl1',
                                        foldr, concat, maximum, minimum,
                                        product, sum, all, and, any, concatMap,
                                        elem, notElem, or, find, mapAccumL,
                                        mapAccumR, maximumBy, minimumBy)
import           Data.Maybe as X
import           Data.Monoid as X
import           Data.Stringable as X hiding (length)
import           Data.Text as X (Text)
import           Data.Tuple as X
import           Data.Traversable as X
import           Filesystem as X hiding (createTree)
import qualified Filesystem.Path.CurrentOS as F
import           Filesystem.Path.CurrentOS as X (FilePath)
import           Foreign.C.String as X
import           Foreign.C.Types as X
import           Foreign.ForeignPtr as X
import           Foreign.Marshal.Alloc as X
import           Foreign.Marshal.Utils as X
import           Foreign.Ptr as X
import           Foreign.StablePtr as X
import           Foreign.Storable as X
import           Prelude as X (undefined, error, otherwise, IO, Show, show,
                               Enum, Eq, Ord, (<), (==), (/=), round, Int,
                               Integer, fromIntegral, fromInteger, toInteger,
                               putStrLn, (-), (+))
import           Unsafe.Coerce as X

type ObjPtr a = Maybe (ForeignPtr a)

type ByteSource = GSource IO B.ByteString

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

  objectRef :: a -> IO (ObjRef a)
  objectRef x = do
    oid <- objectId x
    case oid of
      Oid coid -> return (IdRef coid)
      PartialOid _ _ -> error "Did not expect to see a PartialOid"

  objectRefId :: ObjRef a -> IO Oid
  objectRefId (IdRef coid) = return (Oid coid)
  objectRefId (ObjRef x)   = objectId x

  maybeObjectId :: a -> Maybe Oid
  maybeObjectId x = case getId x of
    Pending _ -> Nothing
    Stored y  -> Just (Oid y)

  lookupFunction :: Repository -> Oid -> IO (Maybe a)

  loadObject :: Updatable b => ObjRef a -> b -> IO (Maybe a)
  loadObject (IdRef coid) y = lookupFunction (objectRepo y) (Oid coid)
  loadObject (ObjRef x) _   = return (Just x)

  loadObject' :: Updatable b => ObjRef a -> b -> IO a
  loadObject' x y =
    maybe (throwIO ObjectLookupFailed) return =<< loadObject x y

  getObject :: ObjRef a -> Maybe a
  getObject (IdRef _)  = Nothing
  getObject (ObjRef x) = Just x

data Repository = Repository
    { repoPath :: FilePath
    -- jww (2013-01-21): These two callbacks are a temporary workaround until
    -- libgit2 supports full virtualization of repositories.  See:
    -- https://github.com/libgit2/libgit2/issues/1213
    , repoBeforeReadRef :: [Repository -> Text -> IO ()]
    , repoOnWriteRef    :: [Repository -> Reference -> IO ()]
    , repoObj           :: ForeignPtr C'git_repository
    }

instance Eq Repository where
  x == y = repoPath x == repoPath y && repoObj x == repoObj y

instance Show Repository where
  show x = "Repository " <> toString (repoPath x)

data RefTarget = RefTargetId Oid
               | RefTargetSymbolic Text
               deriving (Show, Eq)

data Reference = Reference { refRepo   :: Repository
                           , refName   :: Text
                           , refTarget :: RefTarget
                           , refObj    :: ObjPtr C'git_reference }
               deriving (Show, Eq)

data Base a = Base { gitId   :: Ident a
                   , gitRepo :: Repository
                   , gitObj  :: ObjPtr C'git_object }
            deriving Eq

instance Show (Base a) where
  show x = case gitId x of
             Pending _ -> "Base..."
             Stored y  -> "Base#" ++ show y

newBase :: Repository -> Ident a -> ObjPtr C'git_object -> Base a
newBase repo oid obj = Base { gitId   = oid
                            , gitRepo = repo
                            , gitObj  = obj }

repositoryPtr :: Repository -> ForeignPtr C'git_repository
repositoryPtr repo = repoObj repo

openRepository :: FilePath -> IO Repository
openRepository path =
  openRepositoryWith path c'git_repository_open

createRepository :: FilePath -> Bool -> IO Repository
createRepository path bare =
  openRepositoryWith path (\x y -> c'git_repository_init x y (fromBool bare))

openOrCreateRepository :: FilePath -> Bool -> IO Repository
openOrCreateRepository path bare = do
  p <- isDirectory path
  if p
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
        return Repository { repoPath          = path
                          , repoBeforeReadRef = []
                          , repoOnWriteRef    = []
                          , repoObj           = fptr
                          }
  where doesNotExist = throwIO . RepositoryNotExist . toString

withObject :: (Updatable a, Updatable b) => ObjRef a -> b -> (a -> IO c) -> IO c
withObject objRef parent f = do
  obj <- loadObject objRef parent
  case obj of
    Nothing   -> error "Cannot find Git object in repository"
    Just obj' -> f obj'

withObjectPtr :: (Updatable a, Updatable b)
              => ObjRef a -> b -> (Ptr c -> IO d) -> IO d
withObjectPtr objRef parent f =
  withObject objRef parent $ \obj ->
    case objectPtr obj of
      Nothing     -> error "Cannot find Git object id"
      Just objPtr -> withForeignPtr objPtr (f . castPtr)

lookupObject'
  :: Repository -> Oid
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> CUInt -> IO CInt)
  -> (COid -> ForeignPtr C'git_object -> Ptr C'git_object -> IO b)
  -> IO (Maybe b)
lookupObject' repo oid lookupFn lookupPrefixFn createFn =
  alloca $ \ptr -> do
    r <- withForeignPtr (repositoryPtr repo) $ \repoPtr ->
           case oid of
             Oid (COid oid') ->
               withForeignPtr oid' $ \oidPtr ->
                 lookupFn ptr repoPtr oidPtr
             PartialOid (COid oid') len ->
               withForeignPtr oid' $ \oidPtr ->
                 lookupPrefixFn ptr repoPtr oidPtr (fromIntegral len)
    if r < 0
      then return Nothing
      else do
        ptr'     <- castPtr <$> peek ptr
        coid     <- c'git_object_id ptr'
        coidCopy <- mallocForeignPtr
        withForeignPtr coidCopy $ flip c'git_oid_cpy coid

        fptr <- newForeignPtr p'git_object_free ptr'
        Just <$> createFn (COid coidCopy) fptr ptr'

-- Internal.hs
