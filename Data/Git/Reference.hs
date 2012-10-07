{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Reference
       ( RefTarget(..)
       , Reference(..), HasReference(..)

       , createRef
       , lookupRef
       , lookupId
       , writeRef )
       where

import           Bindings.Libgit2
import           Data.Git.Common
import           Data.Git.Internal
import           Data.Git.Object
import qualified Prelude

default (Text)

data RefTarget = RefTargetId Oid
               | RefTargetSymbolic Text
               deriving (Show, Eq)

data Reference = Reference { _refRepo   :: Repository
                           , _refName   :: Text
                           , _refTarget :: RefTarget
                           , _refObj    :: ObjPtr C'git_reference }
               deriving Show

makeClassy ''Reference

-- int git_reference_lookup(git_reference **reference_out,
--   git_repository *repo, const char *name)

createRef :: Text -> RefTarget -> Repository -> Reference
createRef name target repo =
  Reference { _refRepo   = repo
            , _refName   = name
            , _refTarget = target
            , _refObj    = Nothing }

lookupRef :: Text -> Repository -> IO (Maybe Reference)
lookupRef name repo = alloca $ \ptr -> do
  r <- withForeignPtr (repositoryPtr repo) $ \repoPtr ->
    withCStringable name $ \namePtr ->
      c'git_reference_lookup ptr repoPtr namePtr
  if (r < 0)
    then return Nothing
    else do
    ref  <- peek ptr
    fptr <- newForeignPtr p'git_reference_free ref
    return $ Just $ Reference { _refRepo   = repo
                              , _refName   = name
                              , _refTarget = undefined
                              , _refObj    = Just fptr }

writeRef :: Reference -> IO Reference
writeRef ref = alloca $ \ptr -> do
  withForeignPtr repo $ \repoPtr ->
    withCStringable (ref^.refName) $ \namePtr -> do
      r <- case ref^.refTarget of
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
  return $ refObj .~ Just fptr $ ref

  where
    repo = fromMaybe (error "Repository invalid") (ref^.refRepo.repoObj)

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
    repo = fromMaybe (error "Repository invalid") (repos^.repoObj)

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

data ListFlags = ListFlags { _invalid :: Bool
                           , _oid :: Bool
                           , _symbolic :: Bool
                           , _packed :: Bool
                           , _has_peel :: Bool }
               deriving Show

listAll = ListFlags {
  _invalid=False,
  _oid=True,
  _symbolic=True,
  _packed=True,
  _has_peel=False }

makeClassy ''ListFlags

-- int git_reference_list(git_strarray *array, git_repository *repo,
--   unsigned int list_flags)

-- use peekArray

listRefNames :: Repository -> ListFlags -> IO [ Text ]
listRefNames repo flags = undefined -- alloca $

--listRefs = c'git_reference_list

-- int git_reference_foreach(git_repository *repo, unsigned int list_flags,
--   int (*callback)(const char *, void *), void *payload)

--foreachRef = c'git_reference_foreach

-- int git_reference_is_packed(git_reference *ref)

--refIsPacked = c'git_reference_is_packed

-- int git_reference_reload(git_reference *ref)

--reloadRef = c'git_reference_reload

-- void git_reference_free(git_reference *ref)

--freeRef = c'git_reference_free

-- int git_reference_cmp(git_reference *ref1, git_reference *ref2)

--compareRef = c'git_reference_cmp

-- Refs.hs
