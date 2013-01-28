{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module Git.Libgit2.Internal where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.ByteString
import           Data.Stringable
import           Data.Text (Text)
import qualified Data.Text as T
import           Filesystem
import           Filesystem.Path.CurrentOS (FilePath)
import qualified Filesystem.Path.CurrentOS as F
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Utils
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git
import           Git.Libgit2.Types
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

withOpenLgRepository :: Repository -> LgRepository a -> IO a
withOpenLgRepository repo action =
    evalStateT (runLgRepository action) repo

withLgRepository :: FilePath -> Bool -> LgRepository a -> IO a
withLgRepository path bare action = do
    repo <- openOrCreateLgRepository path bare
    withOpenLgRepository repo action

openLgRepository :: FilePath -> IO Repository
openLgRepository path =
  openRepositoryWith path c'git_repository_open

createLgRepository :: FilePath -> Bool -> IO Repository
createLgRepository path bare =
  openRepositoryWith path (\x y -> c'git_repository_init x y (fromBool bare))

openOrCreateLgRepository :: FilePath -> Bool -> IO Repository
openOrCreateLgRepository path bare = do
  p <- liftIO $ isDirectory path
  if p
    then openLgRepository path
    else createLgRepository path bare

openRepositoryWith :: FilePath
                   -> (Ptr (Ptr C'git_repository) -> CString -> IO CInt)
                   -> IO Repository
openRepositoryWith path fn = do
    fptr <- liftIO $ alloca $ \ptr ->
        case F.toText path of
            Left p  -> error $ "Repository does not exist: " ++ T.unpack p
            Right p -> withCStringable p $ \str -> do
                r <- fn ptr str
                when (r < 0) $
                    error $ "Repository does not exist: " ++ T.unpack p
                ptr' <- peek ptr
                newForeignPtr p'git_repository_free ptr'
    return Repository { repoPath = path
                      , repoObj  = fptr }

type ObjPtr a = Maybe (ForeignPtr a)

data Base = Base { gitId  :: Maybe COid
                 , gitObj :: ObjPtr C'git_object }
          deriving Eq

instance Show Base where
  show x = case gitId x of
             Nothing -> "Base..."
             Just y  -> "Base#" ++ show y

-- | 'COid' is a type wrapper for a foreign pointer to libgit2's 'git_oid'
--   structure.  Users should not have to deal with this type.
type COid = ForeignPtr C'git_oid

coidToOid :: COid -> Git.Oid
coidToOid coid =
    Git.Oid (unsafePerformIO $ withForeignPtr coid $ packCString . castPtr)

coidPtrToOid :: Ptr C'git_oid -> Git.Oid
coidPtrToOid coidptr =
    Git.Oid (unsafePerformIO (packCString (castPtr coidptr)))

oidToCoid :: Git.Oid -> COid
oidToCoid (Git.Oid oid) = unsafePerformIO $ do
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr ->
        useAsCString oid (c'git_oid_fromraw ptr . castPtr)
    return fptr

lookupObject'
  :: Git.Oid
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> CUInt -> IO CInt)
  -> (COid -> ForeignPtr C'git_object -> Ptr C'git_object -> IO b)
  -> LgRepository b
lookupObject' oid lookupFn lookupPrefixFn createFn = do
    repo <- lgGet
    liftIO $ alloca $ \ptr -> do
      r <- withForeignPtr (repoObj repo) $ \repoPtr ->
          withForeignPtr (oidToCoid oid) $ \oidPtr ->
              lookupFn ptr repoPtr oidPtr
               -- PartialOid (COid oid') len ->
               --   withForeignPtr oid' $ \oidPtr ->
               --     lookupPrefixFn ptr repoPtr oidPtr (fromIntegral len)
      if r < 0
        then error "lookupObject' failed"
        else do
        ptr'     <- castPtr <$> peek ptr
        coid     <- c'git_object_id ptr'
        coidCopy <- mallocForeignPtr
        withForeignPtr coidCopy $ flip c'git_oid_cpy coid

        fptr <- newForeignPtr p'git_object_free ptr'
        createFn coidCopy fptr ptr'

-- Blob.hs
