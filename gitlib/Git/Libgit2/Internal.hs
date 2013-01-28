{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}

module Git.Libgit2.Internal where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.ByteString
import           Data.Dynamic
import           Data.Stringable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as U
import           Data.Time
import           Data.Time.Clock
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
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

data Base = Base { gitId  :: Maybe (Git.Oid LgRepository)
                 , gitObj :: ObjPtr C'git_object }

-- instance Show Base where
--   show x = case gitId x of
--              Nothing -> "Base..."
--              Just y  -> "Base#" ++ show y

-- -- | 'COid' is a type wrapper for a foreign pointer to libgit2's 'git_oid'
-- --   structure.  Users should not have to deal with this type.
-- type COid = ForeignPtr C'git_oid

-- coidToOid :: COid -> Oid
-- coidToOid coid =
--     Oid (unsafePerformIO $ withForeignPtr coid $ packCString . castPtr)

-- coidPtrToOid :: Ptr C'git_oid -> Oid
-- coidPtrToOid coidptr =
--     Oid (unsafePerformIO (packCString (castPtr coidptr)))

-- oidToCoid :: Oid -> COid
-- oidToCoid (Oid oid) = unsafePerformIO $ do
--     fptr <- mallocForeignPtr
--     withForeignPtr fptr $ \ptr ->
--         useAsCString oid (c'git_oid_fromraw ptr . castPtr)
--     return fptr

coidPtrToOid :: Ptr C'git_oid -> IO (ForeignPtr C'git_oid)
coidPtrToOid coidptr = do
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr ->
        c'git_oid_cpy ptr coidptr
    return fptr

oidToStr :: Ptr C'git_oid -> IO String
oidToStr = c'git_oid_allocfmt >=> peekCString

lookupObject'
  :: ForeignPtr C'git_oid -> Int
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> CUInt -> IO CInt)
  -> (ForeignPtr C'git_oid
      -> ForeignPtr C'git_object -> Ptr C'git_object -> IO b)
  -> LgRepository b
lookupObject' oid len lookupFn lookupPrefixFn createFn = do
    repo <- lgGet
    liftIO $ alloca $ \ptr -> do
      r <- withForeignPtr (repoObj repo) $ \repoPtr ->
          withForeignPtr oid $ \oidPtr ->
              if len == 40
              then lookupFn ptr repoPtr oidPtr
              else lookupPrefixFn ptr repoPtr oidPtr (fromIntegral len)
      if r < 0
        then error "lookupObject' failed"
        else do
        ptr'     <- castPtr <$> peek ptr
        coid     <- c'git_object_id ptr'
        coidCopy <- mallocForeignPtr
        withForeignPtr coidCopy $ flip c'git_oid_cpy coid

        fptr <- newForeignPtr p'git_object_free ptr'
        createFn coidCopy fptr ptr'

-- lgLookupObject :: Text -> LgRepository Dynamic
-- lgLookupObject str
--     | len > 40 = failure (Git.ObjectLookupFailed str)
--     | otherwise = do
--         fptr <- liftIO $ do
--             fptr <- mallocForeignPtr
--             withForeignPtr fptr $ \ptr ->
--                 withCStringable str $ \cstr -> do
--                     r <- c'git_oid_fromstrn ptr cstr (fromIntegral len)
--                     return $ if r < 0
--                              then Nothing
--                              else Just fptr
--         case fptr of
--             Nothing -> failure (Git.ObjectLookupFailed str)
--             Just x  ->
--                 lookupObject' (coidToOid x) len
--                   (\x y z -> c'git_object_lookup x y z c'GIT_OBJ_ANY)
--                   (\x y z l ->
--                     c'git_object_lookup_prefix x y z l c'GIT_OBJ_ANY)
--                   (\coid x y ->
--                     c'git_object_type y >>= createObject coid x)
--   where
--     len = T.length str

-- createObject :: COid -> ForeignPtr C'git_object -> C'git_otype -> IO Dynamic
-- createObject coid obj typ
--   | typ == c'GIT_OBJ_BLOB = undefined
--     -- return $ toDyn Git.Blob { Git.blobContents = Git.BlobString "" }

--   | typ == c'GIT_OBJ_TREE = undefined
--     -- return $ toDyn Git.Tree { treeInfo =
--     --                                newBase repo (Stored coid) (Just obj)
--     --                         , treeContents = M.empty }

--   | otherwise = return undefined

-- | Convert a time in seconds (from Stripe's servers) to 'UTCTime'. See
--   "Data.Time.Format" for more on working with 'UTCTime'.
fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger

-- | Convert a 'UTCTime' back to an Integer suitable for use with Stripe's API.
toSeconds :: UTCTime -> Integer
toSeconds  = round . utcTimeToPOSIXSeconds

peekGitTime :: Ptr (C'git_time) -> IO UTCTime
peekGitTime time =
  -- jww (2012-09-29): Handle offset here
  return . fromSeconds . toInteger . c'git_time'time =<< peek time

packGitTime :: UTCTime -> C'git_time
packGitTime utcTime =
  C'git_time { c'git_time'time   = fromIntegral (toSeconds utcTime)
             , c'git_time'offset = 0 } -- jww (2012-09-29): NYI

packSignature :: U.Converter -> Ptr C'git_signature -> IO Git.Signature
packSignature conv sig = do
  name  <- peek (p'git_signature'name sig)  >>= packCString
  email <- peek (p'git_signature'email sig) >>= packCString
  time  <- peekGitTime (p'git_signature'when sig)
  return $
    Git.Signature { Git.signatureName  = U.toUnicode conv name
                  , Git.signatureEmail = U.toUnicode conv email
                  , Git.signatureWhen  = time }

withSignature :: U.Converter -> Git.Signature
              -> (Ptr C'git_signature -> IO a) -> IO a
withSignature conv sig f =
  useAsCString (U.fromUnicode conv (Git.signatureName sig)) $ \nameCStr ->
  useAsCString (U.fromUnicode conv (Git.signatureEmail sig)) $ \emailCStr ->
  alloca $ \ptr -> do
      poke ptr (C'git_signature nameCStr emailCStr
                (packGitTime (Git.signatureWhen sig)))
      f ptr

-- Internal.hs
