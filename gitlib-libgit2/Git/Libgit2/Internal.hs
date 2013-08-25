{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Git.Libgit2.Internal (module Git.Libgit2.Internal, oidToStr) where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Data.ByteString
import           Data.Tagged
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as U
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
import           Foreign.C.String
import           Foreign.C.Types
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Ptr
import           Foreign.Storable
import qualified Git
import           Git.Libgit2.Backend
import           Git.Libgit2.Trace
import           Git.Libgit2.Types
import           System.FilePath.Posix

type ObjPtr a = Maybe (ForeignPtr a)

data Base m a b = Base
    { gitId  :: Maybe (Tagged a (Git.Oid (LgRepository m)))
    , gitObj :: ObjPtr b
    }

addTracingBackend :: Git.MonadGit m => LgRepository m ()
addTracingBackend = do
    repo <- lgGet
    liftIO $ withCString (repoPath repo </> "objects") $ \objectsDir ->
        alloca $ \loosePtr -> do
            r <- c'git_odb_backend_loose loosePtr objectsDir (-1) 0
            when (r < 0) $
                error "Failed to create loose objects backend"

            loosePtr' <- peek loosePtr
            backend   <- traceBackend loosePtr'
            void $ odbBackendAdd repo backend 3
            return ()

coidPtrToOid :: Ptr C'git_oid -> IO (ForeignPtr C'git_oid)
coidPtrToOid coidptr = do
    fptr <- mallocForeignPtr
    withForeignPtr fptr $ \ptr ->
        c'git_oid_cpy ptr coidptr
    return fptr

lookupObject'
  :: Git.MonadGit m
  => ForeignPtr C'git_oid -> Int
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> IO CInt)
  -> (Ptr (Ptr a) -> Ptr C'git_repository -> Ptr C'git_oid -> CSize -> IO CInt)
  -> (ForeignPtr C'git_oid -> ForeignPtr a -> Ptr a -> IO b)
  -> LgRepository m b
lookupObject' oid len lookupFn lookupPrefixFn createFn = do
    repo <- lgGet
    result <- liftIO $ alloca $ \ptr -> do
        r <- withForeignPtr (repoObj repo) $ \repoPtr ->
            withForeignPtr oid $ \oidPtr ->
                if len == 40
                then lookupFn ptr repoPtr oidPtr
                else lookupPrefixFn ptr repoPtr oidPtr (fromIntegral len)
        if r < 0
            then do
              err    <- c'giterr_last
              errmsg <- peekCString . c'git_error'message =<< peek err
              oidStr <- withForeignPtr oid oidToStr
              return $ Left $
                  T.concat [ "Could not lookup ", T.pack oidStr
                           , ": ", T.pack errmsg ]
            else do
              ptr'     <- peek ptr
              coid     <- c'git_object_id (castPtr ptr')
              coidCopy <- mallocForeignPtr
              withForeignPtr coidCopy $ flip c'git_oid_cpy coid

              fptr <- newForeignPtr p'git_object_free (castPtr ptr')
              Right <$> createFn coidCopy (castForeignPtr fptr) ptr'
    either (failure . Git.BackendError) return result

-- lgLookupObject :: Text -> LgRepository Dynamic
-- lgLookupObject str
--     | len > 40 = failure (Git.ObjectLookupFailed str)
--     | otherwise = do
--         fptr <- liftIO $ do
--             fptr <- mallocForeignPtr
--             withForeignPtr fptr $ \ptr ->
--                 withCString str $ \cstr -> do
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

-- -- | Convert a time in seconds (from Stripe's servers) to 'UTCTime'. See
-- --   "Data.Time.Format" for more on working with 'UTCTime'.
-- fromSeconds :: Integer -> ZonedTime
-- fromSeconds  = posixSecondsToUTCTime . fromInteger

-- -- | Convert a 'UTCTime' back to an Integer suitable for use with Stripe's API.
-- toSeconds :: ZonedTime -> Integer
-- toSeconds  = round . utcTimeToPOSIXSeconds

peekGitTime :: Ptr C'git_time -> IO ZonedTime
peekGitTime tptr = do
    moment <- peek tptr
    return (utcToZonedTime
            (minutesToTimeZone (fromIntegral (c'git_time'offset moment)))
            (posixSecondsToUTCTime (fromIntegral (c'git_time'time moment))))

packGitTime :: ZonedTime -> C'git_time
packGitTime zt = C'git_time
    { c'git_time'time   =
           floor (utcTimeToPOSIXSeconds (zonedTimeToUTC zt))
    , c'git_time'offset = fromIntegral (timeZoneMinutes (zonedTimeZone zt))
    }

packSignature :: U.Converter -> Ptr C'git_signature -> IO Git.Signature
packSignature conv sig = do
  name  <- peek (p'git_signature'name sig)  >>= packCString
  email <- peek (p'git_signature'email sig) >>= packCString
  time  <- peekGitTime (p'git_signature'when sig)
  return Git.Signature
      { Git.signatureName  = U.toUnicode conv name
      , Git.signatureEmail = U.toUnicode conv email
      , Git.signatureWhen  = time
      }

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
