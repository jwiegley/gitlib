{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Data.Git.Reference
       ( RefTarget(..)
       , Reference(..)
       , createRef
       , resolveRef
       , lookupRef
       , listRefNames
       , ListFlags
       , allRefsFlag
       , oidRefsFlag
       , looseOidRefsFlag
       , symbolicRefsFlag
       , mapRefs
       , mapAllRefs
       , mapOidRefs
       , mapLooseOidRefs
       , mapSymbolicRefs
       , lookupId
       , writeRef
       , writeRef_ )
       where

import           Bindings.Libgit2
import           Data.ByteString.Unsafe
import           Data.Git.Internal
import           Data.IORef
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import           Foreign.Marshal.Array
import qualified Prelude
import           Prelude ((+),(-))

-- int git_reference_lookup(git_reference **reference_out,
--   git_repository *repo, const char *name)

createRef :: Text -> RefTarget -> Repository -> Reference
createRef name target repo =
  Reference { refRepo   = repo
            , refName   = name
            , refTarget = target
            , refObj    = Nothing }

resolveRef :: Text -> Repository -> IO (Maybe Oid)
resolveRef name repo = do
  ref <- lookupRef name repo
  case refTarget <$> ref of
    Nothing                           -> return Nothing
    Just (RefTargetSymbolic nextName) -> resolveRef nextName repo
    Just (RefTargetId oid)            -> return (Just oid)

lookupRef :: Text -> Repository -> IO (Maybe Reference)
lookupRef name repo = alloca $ \ptr -> do
  r <- withForeignPtr (repositoryPtr repo) $ \repoPtr ->
        withCStringable name $ \namePtr ->
          c'git_reference_lookup ptr repoPtr namePtr
  if r < 0
    then return Nothing
    else do
    ref  <- peek ptr
    fptr <- newForeignPtr p'git_reference_free ref
    typ  <- c'git_reference_type ref
    targ <- if typ == c'GIT_REF_OID
            then do oidPtr <- c'git_reference_oid ref
                    newForeignPtr_ oidPtr
                      >>= return . RefTargetId . Oid . COid
            else do targName <- c'git_reference_target ref
                    unsafePackCString targName
                      >>= return . RefTargetSymbolic . E.decodeUtf8
    return $ Just Reference { refRepo   = repo
                            , refName   = name
                            , refTarget = targ
                            , refObj    = Just fptr }

writeRef :: Reference -> IO Reference
writeRef ref = alloca $ \ptr -> do
  withForeignPtr repo $ \repoPtr ->
    withCStringable (refName ref) $ \namePtr -> do
      r <- case refTarget ref of
        RefTargetId (PartialOid {}) ->
          throwIO RefCannotCreateFromPartialOid

        RefTargetId (Oid (COid coid)) ->
          withForeignPtr coid $ \coidPtr ->
            c'git_reference_create_oid ptr repoPtr namePtr
                                       coidPtr (fromBool True)

        RefTargetSymbolic symName ->
          withCStringable symName $ \symPtr ->
            c'git_reference_create_symbolic ptr repoPtr namePtr
                                            symPtr (fromBool True)
      when (r < 0) $ throwIO ReferenceCreateFailed

  fptr <- newForeignPtr_ =<< peek ptr
  let ref' = ref { refObj = Just fptr }
  mapM_ ($ ref') (repoOnWriteRef (refRepo ref'))
  return ref'

  where
    repo = fromMaybe (error "Repository invalid") (repoObj (refRepo ref))

writeRef_ :: Reference -> IO ()
writeRef_ = void . writeRef

-- int git_reference_name_to_oid(git_oid *out, git_repository *repo,
--   const char *name)

lookupId :: Text -> Repository -> IO Oid
lookupId name repos = alloca $ \ptr ->
  withCStringable name $ \namePtr ->
    withForeignPtr repo $ \repoPtr -> do
      r <- c'git_reference_name_to_oid ptr repoPtr namePtr
      when (r < 0) $ throwIO ReferenceLookupFailed
      Oid <$> COid <$> newForeignPtr_ ptr

  where
    repo = fromMaybe (error "Repository invalid") (repoObj repos)

-- int git_reference_create_symbolic(git_reference **ref_out,
--   git_repository *repo, const char *name, const char *target, int force)

--createSymbolicRef = c'git_reference_create_symbolic

-- int git_reference_create_oid(git_reference **ref_out, git_repository *repo,
--   const char *name, const git_oid *id, int force)

--createNamedRef = c'git_reference_create_oid

-- const git_oid * git_reference_oid(git_reference *ref)

--refId = c'git_reference_oid

-- const char * git_reference_target(git_reference *ref)

--refTarget = c'git_reference_target

-- git_ref_t git_reference_type(git_reference *ref)

--refType = c'git_reference_type

-- const char * git_reference_name(git_reference *ref)

--refName = c'git_reference_name

-- int git_reference_resolve(git_reference **resolved_ref, git_reference *ref)

--resolveRef = c'git_reference_resolve

-- int git_reference_set_target(git_reference *ref, const char *target)

--setRefTarget = c'git_reference_set_target

-- int git_reference_set_oid(git_reference *ref, const git_oid *id)

--setRefId = c'git_reference_set_oid

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

symbolicRefsFlag :: ListFlags
symbolicRefsFlag = ListFlags { listFlagInvalid  = False
                             , listFlagOid      = False
                             , listFlagSymbolic = True
                             , listFlagPacked   = False
                             , listFlagHasPeel  = False }

oidRefsFlag :: ListFlags
oidRefsFlag = ListFlags { listFlagInvalid  = False
                        , listFlagOid      = True
                        , listFlagSymbolic = False
                        , listFlagPacked   = True
                        , listFlagHasPeel  = False }

looseOidRefsFlag :: ListFlags
looseOidRefsFlag = ListFlags { listFlagInvalid  = False
                             , listFlagOid      = True
                             , listFlagSymbolic = False
                             , listFlagPacked   = False
                             , listFlagHasPeel  = False }

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

listRefNames :: Repository -> ListFlags -> IO [Text]
listRefNames repo flags =
  alloca $ \c'refs ->
    withForeignPtr (repositoryPtr repo) $ \repoPtr -> do
      r <- c'git_reference_list c'refs repoPtr (flagsToInt flags)
      when (r < 0) $ throwIO ReferenceLookupFailed

      refs <- gitStrArray2List c'refs
      c'git_strarray_free c'refs
      return refs

--listRefs = c'git_reference_list

-- int git_reference_foreach(git_repository *repo, unsigned int list_flags,
--   int (*callback)(const char *, void *), void *payload)

foreachRefCallback :: CString -> Ptr () -> IO CInt
foreachRefCallback name payload = do
  (callback,results) <- peek (castPtr payload) >>= deRefStablePtr
  result <- unsafePackCString name >>= callback . E.decodeUtf8
  modifyIORef results (\xs -> result:xs)
  return 0

foreign export ccall "foreachRefCallback"
  foreachRefCallback :: CString -> Ptr () -> IO CInt
foreign import ccall "&foreachRefCallback"
  foreachRefCallbackPtr :: FunPtr (CString -> Ptr () -> IO CInt)

mapRefs :: Repository -> ListFlags -> (Text -> IO a) -> IO [a]
mapRefs repo flags callback = do
  ioRef <- newIORef []
  bracket
    (newStablePtr (callback,ioRef))
    deRefStablePtr
    (\ptr -> with ptr $ \pptr ->
      withForeignPtr (repositoryPtr repo) $ \repoPtr -> do
        _ <- c'git_reference_foreach repoPtr (flagsToInt flags)
                                    foreachRefCallbackPtr (castPtr pptr)
        readIORef ioRef)

mapAllRefs :: Repository -> (Text -> IO a) -> IO [a]
mapAllRefs repo = mapRefs repo allRefsFlag
mapOidRefs :: Repository -> (Text -> IO a) -> IO [a]
mapOidRefs repo = mapRefs repo oidRefsFlag
mapLooseOidRefs :: Repository -> (Text -> IO a) -> IO [a]
mapLooseOidRefs repo = mapRefs repo looseOidRefsFlag
mapSymbolicRefs :: Repository -> (Text -> IO a) -> IO [a]
mapSymbolicRefs repo = mapRefs repo symbolicRefsFlag

{-
foreachRefCallbackThunk :: Storable a =>
                           ForeachRefCallback a -> CString -> Ptr () -> IO CInt
foreachRefCallbackThunk callback name payload = do
  nameText <- E.decodeUtf8 <$> unsafePackCString name
  payloadValue <- peek (castPtr payload)
  maybe (-1) (const 0) <$> callback nameText payloadValue

mapRefsWith ::
  Storable a => Repository -> ListFlags -> a -> ForeachRefCallback a -> IO ()
mapRefsWith repo flags payload cb =
    withForeignPtr (repositoryPtr repo) $ \repoPtr -> do
      fun <- mk'git_reference_foreach_callback (foreachRefCallbackThunk cb)
      with payload $ \payloadPtr -> do
        r <- c'git_reference_foreach repoPtr (flagsToInt flags) fun
                                    (castPtr payloadPtr)
        -- jww (2012-12-14): Does the return type mean anything here?
        when (r < 0) $ return ()
        return ()
-}

-- int git_reference_is_packed(git_reference *ref)

--refIsPacked = c'git_reference_is_packed

-- int git_reference_reload(git_reference *ref)

--reloadRef = c'git_reference_reload

-- void git_reference_free(git_reference *ref)

--freeRef = c'git_reference_free

-- int git_reference_cmp(git_reference *ref1, git_reference *ref2)

--compareRef = c'git_reference_cmp

-- Refs.hs
