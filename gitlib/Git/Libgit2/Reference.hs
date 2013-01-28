{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Git.Libgit2.Reference
       ( lgResolveRef
       , lgLookupRef
       , lgTraverseRefs
       , lgWriteRef )
       where

import           Bindings.Libgit2
import           Data.ByteString
import qualified Git
import           Git.Libgit2.Internal
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Foreign.Marshal.Array
import qualified Prelude
import           Prelude ((+),(-))

lgLookupRef :: Text -> LgRepository Git.Reference
lgLookupRef name = do
    repo <- lgGet
    targ <- liftIO $ alloca $ \ptr -> do
        r <- withForeignPtr (repositoryPtr repo) $ \repoPtr ->
              withCStringable name $ \namePtr ->
                c'git_reference_lookup ptr repoPtr namePtr
        if r < 0
            then failure (Git.ReferenceLookupFailed name)
            else do
            ref  <- peek ptr
            fptr <- newForeignPtr p'git_reference_free ref
            typ  <- c'git_reference_type ref
            if typ == c'GIT_REF_OID
                then do oidPtr <- c'git_reference_oid ref
                        return (Git.RefOid (coidPtrToOid oidPtr))
                else do targName <- c'git_reference_target ref
                        packCString targName
                            >>= return . Git.RefSymbolic . E.decodeUtf8
    return $ Git.Reference { Git.refName   = name
                           , Git.refTarget = targ }

lgWriteRef :: Git.Reference -> LgRepository ()
lgWriteRef ref = do
    repo <- lgGet
    liftIO $ alloca $ \ptr ->
        withForeignPtr (repoObj repo) $ \repoPtr ->
        withCStringable (Git.refName ref) $ \namePtr -> do
            r <- case Git.refTarget ref of
                Git.RefOid oid ->
                    withForeignPtr (oidToCoid oid) $ \coidPtr ->
                        c'git_reference_create_oid ptr repoPtr namePtr
                                                   coidPtr (fromBool True)

                Git.RefSymbolic symName ->
                  withCStringable symName $ \symPtr ->
                    c'git_reference_create_symbolic ptr repoPtr namePtr
                                                    symPtr (fromBool True)
            when (r < 0) $ failure Git.ReferenceCreateFailed

-- int git_reference_name_to_oid(git_oid *out, git_repository *repo,
--   const char *name)

lgResolveRef :: Text -> LgRepository Git.Oid
lgResolveRef name = do
    repo <- lgGet
    oid <- liftIO $ alloca $ \ptr ->
        withCStringable name $ \namePtr ->
        withForeignPtr (repoObj repo) $ \repoPtr -> do
            r <- c'git_reference_name_to_oid ptr repoPtr namePtr
            return $ if r < 0
                     then Nothing
                     else Just (coidPtrToOid ptr)
    maybe (failure (Git.ReferenceLookupFailed name)) return oid

-- int git_reference_rename(git_reference *ref, const char *new_name,
--   int force)

--renameRef = c'git_reference_rename

-- int git_reference_delete(git_reference *ref)

--deleteRef = c'git_reference_delete

-- int git_reference_packall(git_repository *repo)

--packallRefs = c'git_reference_packall

data ListFlags = ListFlags { listFlagInvalid  :: Bool
                           , listFlagOid      :: Bool
                           , listFlagSymbolic :: Bool
                           , listFlagPacked   :: Bool
                           , listFlagHasPeel  :: Bool }
               deriving (Show, Eq)

allRefsFlag :: ListFlags
allRefsFlag = ListFlags { listFlagInvalid  = False
                        , listFlagOid      = True
                        , listFlagSymbolic = True
                        , listFlagPacked   = True
                        , listFlagHasPeel  = False }

-- symbolicRefsFlag :: ListFlags
-- symbolicRefsFlag = ListFlags { listFlagInvalid  = False
--                              , listFlagOid      = False
--                              , listFlagSymbolic = True
--                              , listFlagPacked   = False
--                              , listFlagHasPeel  = False }

-- oidRefsFlag :: ListFlags
-- oidRefsFlag = ListFlags { listFlagInvalid  = False
--                         , listFlagOid      = True
--                         , listFlagSymbolic = False
--                         , listFlagPacked   = True
--                         , listFlagHasPeel  = False }

-- looseOidRefsFlag :: ListFlags
-- looseOidRefsFlag = ListFlags { listFlagInvalid  = False
--                              , listFlagOid      = True
--                              , listFlagSymbolic = False
--                              , listFlagPacked   = False
--                              , listFlagHasPeel  = False }

gitStrArray2List :: Ptr C'git_strarray -> IO [Text]
gitStrArray2List gitStrs = do
  count <- fromIntegral <$> ( peek $ p'git_strarray'count gitStrs )
  strings <- peek $ p'git_strarray'strings gitStrs

  r0 <- Foreign.Marshal.Array.peekArray count strings
  r1 <- sequence $ fmap peekCString r0
  return $ fmap T.pack r1

flagsToInt :: ListFlags -> CUInt
flagsToInt flags = (if listFlagOid flags      then 1 else 0)
                 + (if listFlagSymbolic flags then 2 else 0)
                 + (if listFlagPacked flags   then 4 else 0)
                 + (if listFlagHasPeel flags  then 8 else 0)

-- listRefNames :: ListFlags -> LgRepository [Text]
-- listRefNames flags = do
--     repo <- lgGet
--     refs <- liftIO $ alloca $ \c'refs ->
--       withForeignPtr (repositoryPtr repo) $ \repoPtr -> do
--         r <- c'git_reference_list c'refs repoPtr (flagsToInt flags)
--         if r < 0
--             then return Nothing
--             else do refs <- gitStrArray2List c'refs
--                     c'git_strarray_free c'refs
--                     return (Just refs)
--     maybe (failure Git.ReferenceListingFailed) return refs

foreachRefCallback :: CString -> Ptr () -> IO CInt
foreachRefCallback name payload = do
  (callback,results) <- peek (castPtr payload) >>= deRefStablePtr
  result <- packCString name >>= callback . E.decodeUtf8
  modifyIORef results (\xs -> result:xs)
  return 0

foreign export ccall "foreachRefCallback"
  foreachRefCallback :: CString -> Ptr () -> IO CInt
foreign import ccall "&foreachRefCallback"
  foreachRefCallbackPtr :: FunPtr (CString -> Ptr () -> IO CInt)

lgTraverseRefs :: (Git.Reference -> LgRepository a) -> LgRepository [a]
lgTraverseRefs cb = do
    repo <- lgGet
    liftIO $ do
        ioRef <- newIORef []
        bracket
            (newStablePtr (cb,ioRef))
            deRefStablePtr
            (\ptr -> with ptr $ \pptr ->
              withForeignPtr (repoObj repo) $ \repoPtr -> do
                  _ <- c'git_reference_foreach
                           repoPtr (flagsToInt allRefsFlag)
                           foreachRefCallbackPtr (castPtr pptr)
                  readIORef ioRef)

-- mapAllRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapAllRefs repo = mapRefs repo allRefsFlag
-- mapOidRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapOidRefs repo = mapRefs repo oidRefsFlag
-- mapLooseOidRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapLooseOidRefs repo = mapRefs repo looseOidRefsFlag
-- mapSymbolicRefs :: (Text -> LgRepository a) -> LgRepository [a]
-- mapSymbolicRefs repo = mapRefs repo symbolicRefsFlag

-- int git_reference_is_packed(git_reference *ref)

--refIsPacked = c'git_reference_is_packed

-- int git_reference_reload(git_reference *ref)

--reloadRef = c'git_reference_reload

-- int git_reference_cmp(git_reference *ref1, git_reference *ref2)

--compareRef = c'git_reference_cmp

-- Refs.hs
