{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

-- | Interface for opening and creating repositories.  Repository objects are
--   immutable, and serve only to refer to the given repository.  Any data
--   associated with the repository — such as the list of branches — is
--   queried as needed.
module Git.Libgit2
       ( LgRepository(..)
       , withLgRepository
       , withOpenLgRepository
       , openLgRepository
       , createLgRepository
       , openOrCreateLgRepository
       ) where

import           Bindings.Libgit2
import           Control.Applicative
import           Control.Concurrent.ParallelIO
import           Control.Exception
import           Control.Failure
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Either
import           Control.Monad.Trans.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import           Data.ByteString.Unsafe
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Monoid
import           Data.Stringable
import           Data.Tagged
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
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
import qualified Git as Git
import           Git.Libgit2.Blob
import           Git.Libgit2.Internal
import           Git.Libgit2.Types
import           Prelude hiding (FilePath)
import           System.IO.Unsafe

instance Git.Repository LgRepository where
    -- lookupRef :: Text -> m Reference
    lookupRef = undefined
    -- updateRef :: Text -> Reference -> m Reference
    updateRef = undefined
    -- traverseRefs :: Traversable t => (Reference -> m b) -> m (t b)
    traverseRefs = undefined
    -- -- lookupObject :: Text -> m Dynamic
    -- lookupObject = lgLookupObject
    -- lookupCommit :: Oid -> m Commit
    lookupCommit = undefined
    -- lookupTree :: Oid -> m Tree
    lookupTree = lgLookupTree
    -- lookupBlob :: Oid -> m (Blob m)
    lookupBlob = lgLookupBlob
    -- lookupTag :: Oid -> m Tag
    lookupTag = undefined
    -- newTree :: m Tree
    newTree = lgNewTree
    -- createBlob :: BlobContents m -> m (BlobOid m)
    createBlob = lgCreateBlob
    -- createCommit :: [ObjRef Commit] -> ObjRef Tree -> Signature -> Signature
    --                 -> Text -> m c
    createCommit = undefined

instance Git.Object (Git.Blob LgRepository) where
    update blob = lgCreateBlob (Git.blobContents blob)

-- | Create a new blob in the 'Repository', with 'ByteString' as its contents.
--
--   Note that since empty blobs cannot exist in Git, no means is provided for
--   creating one; if the give string is 'empty', it is an error.
lgCreateBlob ::
    Git.BlobContents LgRepository -> LgRepository (Git.BlobOid LgRepository)
lgCreateBlob b = do
    repo <- lgGet
    ptr  <- liftIO $ mallocForeignPtr
    r    <- Git.blobContentsToByteString b
            >>= \bs -> liftIO $ createBlobFromByteString repo ptr bs
    when (r < 0) $ failure Git.BlobCreateFailed
    return (Tagged (coidToOid ptr))

  where
    createBlobFromByteString repo coid bs =
        unsafeUseAsCStringLen bs $
            uncurry (\cstr len ->
                      withForeignPtr coid $ \coid' ->
                      withForeignPtr (repoObj repo) $ \repoPtr ->
                        c'git_blob_create_frombuffer
                          coid' repoPtr (castPtr cstr) (fromIntegral len))

lgLookupBlob :: Git.Oid -> LgRepository (Git.Blob m)
lgLookupBlob oid =
    lookupObject' oid 40 c'git_blob_lookup c'git_blob_lookup_prefix
    $ \coid obj _ ->
    withForeignPtr obj $ \ptr -> do
        size <- c'git_blob_rawsize (castPtr ptr)
        buf  <- c'git_blob_rawcontent (castPtr ptr)
        -- The lifetime of buf is tied to the lifetime of the blob object in
        -- libgit2, which this Blob object controls, so we can use
        -- unsafePackCStringLen to refer to its bytes.
        bstr <- curry unsafePackCStringLen (castPtr buf)
                      (fromIntegral size)
        return (Git.Blob (Git.BlobString bstr))

type TreeEntry = Git.TreeEntry LgRepository
type TreeMap = Map Text TreeEntry

data Tree = Tree { treeInfo     :: Base
                 , treeContents :: TreeMap }

-- instance Show Tree where
--     show x = case gitId (treeInfo x) of
--         Nothing -> "Tree..."
--         Just y  -> "Tree#" ++ show y

-- instance Show (Git.Blob LgRepository) where
--     show _ = "Blob"

-- instance Show a => Show (Git.ObjRef a) where
--     show (Git.ByOid oid) = show oid
--     show (Git.Known x) = show x

-- instance Show TreeEntry where
--     show (Git.BlobEntry blob _) = "BlobEntry (" ++ show blob ++ ")"
--     show (Git.TreeEntry tree)   = "TreeEntry (" ++ show tree ++ ")"

instance Git.Tree Tree where
    modifyTree = lgModifyTree

instance Git.Object Tree where
    update = writeTree

newTreeBase :: Tree -> Base
newTreeBase t = Base Nothing Nothing

-- | Create a new, empty tree.
--
--   Since empty trees cannot exist in Git, attempting to write out an empty
--   tree is a no-op.
lgNewTree :: LgRepository Tree
lgNewTree = return Tree { treeInfo     = Base Nothing Nothing
                        , treeContents = M.empty }

lgLookupTree :: Git.Oid -> LgRepository Tree
lgLookupTree oid =
    -- jww (2013-01-28): Verify the oid here
  lookupObject' oid 40 c'git_tree_lookup c'git_tree_lookup_prefix $
    \coid obj _ -> do
      entriesAList <- withForeignPtr obj $ \treePtr -> do
        entryCount <- c'git_tree_entrycount (castPtr treePtr)
        foldM
          (\m idx -> do
              entry <- c'git_tree_entry_byindex (castPtr treePtr)
                                                 (fromIntegral idx)
              when (entry == nullPtr) $ error "ObjectLookupFailed"

              entryId <- c'git_tree_entry_id entry
              coid    <- mallocForeignPtr
              withForeignPtr coid $ \coid' ->
                c'git_oid_cpy coid' entryId

              entryName  <- c'git_tree_entry_name entry
                            >>= peekCString >>= return . T.pack
              entryAttrs <- c'git_tree_entry_attributes entry
              entryType  <- c'git_tree_entry_type entry

              let entryObj =
                      if entryType == c'GIT_OBJ_BLOB
                         -- jww (2013-01-28): What about the executable
                         -- bit??
                      then Git.BlobEntry (Git.ByOid (Tagged (coidToOid coid))) False
                      else Git.TreeEntry (Git.ByOid (Tagged (coidToOid coid)))

              return ((entryName,entryObj):m))
          [] [0..(entryCount-1)]
      return Tree { treeInfo     = Base (Just coid) (Just obj)
                  , treeContents = M.fromList entriesAList }

doLookupTreeEntry :: Tree -> [Text] -> LgRepository (Maybe TreeEntry)
doLookupTreeEntry t [] = return (Just (Git.treeEntry t))
doLookupTreeEntry t (name:names) = do
  -- Lookup the current name in this tree.  If it doesn't exist, and there are
  -- more names in the path and 'createIfNotExist' is True, create a new Tree
  -- and descend into it.  Otherwise, if it exists we'll have @Just (TreeEntry
  -- {})@, and if not we'll have Nothing.

  -- Prelude.putStrLn $ "Tree: " ++ show t
  -- Prelude.putStrLn $ "Tree Entries: " ++ show (treeContents t)
  -- Prelude.putStrLn $ "Lookup: " ++ toString name
  let y = M.lookup name (treeContents t)

  -- Prelude.putStrLn $ "Result: " ++ show y
  -- Prelude.putStrLn $ "Names: " ++ show names
  if null names
      then return y
      else case y of
      Just (Git.BlobEntry {}) -> failure Git.TreeCannotTraverseBlob
      Just (Git.TreeEntry t') -> do t'' <- Git.resolveTreeRef t'
                                    doLookupTreeEntry t'' names
      _ -> return Nothing

lookupTreeEntry :: Tree -> FilePath -> LgRepository TreeEntry
lookupTreeEntry tr path = do
    entry <- doLookupTreeEntry tr (splitPath path)
    maybe (failure (Git.TreeEntryLookupFailed path)) return entry

-- withGitTree :: Updatable b
--             => ObjRef Tree -> b -> (Ptr C'git_tree -> IO a) -> IO a
-- withGitTree tref obj f =
--   withForeignPtr (repositoryPtr (objectRepo obj)) $ \repoPtr ->
--     case tref of
--       IdRef (COid oid) -> withGitTreeOid repoPtr oid

--       ObjRef (Tree { treeInfo = Base { gitId = Stored (COid oid) } }) ->
--         withGitTreeOid repoPtr oid

--       ObjRef (Tree { treeInfo = Base { gitObj = Just t } }) ->
--         withForeignPtr t (f . castPtr)

--       ObjRef t -> do t' <- update t
--                      withGitTree (ObjRef t') obj f

--   where withGitTreeOid repoPtr oid =
--           withForeignPtr oid $ \tree_id ->
--             alloca $ \ptr -> do
--               r <- c'git_tree_lookup ptr repoPtr tree_id
--               when (r < 0) $ throwIO TreeLookupFailed
--               f =<< peek ptr

-- | Write out a tree to its repository.  If it has already been written,
--   nothing will happen.
writeTree :: Tree -> LgRepository (Tagged Tree Git.Oid)
writeTree t@(Tree { treeInfo = Base { gitId = Just coid } }) =
    return (Tagged (coidToOid coid))
writeTree t = Tagged . coidToOid . snd <$> doWriteTree t

doWriteTree :: Tree -> LgRepository (Tree, COid)
doWriteTree t = do
    repo <- lgGet
    ptr <- liftIO malloc         -- jww (2013-01-28): memory leak!!!
    either failure return =<< runEitherT (go ptr (repoObj repo))
  where
    go :: Ptr (Ptr C'git_treebuilder) -> ForeignPtr C'git_repository
          -> EitherT Git.Exception LgRepository (Tree,COid)
    go ptr repo = do
        r <- liftIO $ c'git_treebuilder_create ptr nullPtr
        when (r < 0) $ left Git.TreeBuilderCreateFailed
        builder <- liftIO $ peek ptr

        -- jww (2012-10-14): With the loose object backend, there should be no
        -- race conditions here as there will never be a request to access the
        -- same file by multiple threads.  If that ever does happen, or if
        -- this code is changed to write to the packed object backend, simply
        -- change the function 'parallel' to 'sequence' here.
        oids <- liftIO $ parallel $
            flip map (M.toList (treeContents t)) $ \(k, v) ->
                case v of
                    x@(Git.BlobEntry _ exe) -> do
                        return (k, x, if exe then 0o100755 else 0o100644)
                    x@(Git.TreeEntry _) -> do
                        return (k, x, 0o040000)

        newList <- for oids $ \(k, entry, flags) -> do
            coid <- oidToCoid . untag <$> lift (Git.update entry)
            insertObject builder k coid flags
            return (k, entry)

        (r3,coid) <- liftIO $ do
            coid <- mallocForeignPtr
            withForeignPtr coid $ \coid' ->
                withForeignPtr repo $ \repoPtr -> do
                    r3 <- c'git_treebuilder_write coid' repoPtr builder
                    return (r3,coid)
        if r3 < 0
            then left Git.TreeBuilderWriteFailed
            else return (t { treeInfo     = (treeInfo t) { gitId = Just coid }
                           , treeContents = M.fromList newList }, coid)

    insertObject :: (CStringable a)
                 => Ptr C'git_treebuilder -> a -> COid -> CUInt
                    -> EitherT Git.Exception LgRepository ()
    insertObject builder key coid attrs = do
      r2 <- liftIO $ withForeignPtr coid $ \coid' ->
          withCStringable key $ \name ->
              c'git_treebuilder_insert nullPtr builder name coid' attrs
      if r2 < 0
          then left Git.TreeBuilderInsertFailed
          else return ()

doModifyTree :: Tree
             -> [Text]
             -> Bool
             -> (Maybe TreeEntry -> LgRepository (Maybe TreeEntry))
             -> LgRepository (Tree,Maybe TreeEntry)
doModifyTree t [] _ _ = return (t, Just (Git.TreeEntry (Git.Known t)))
doModifyTree t (name:names) createIfNotExist f = do
    repo <- lgGet

    -- Lookup the current name in this tree.  If it doesn't exist, and there
    -- are more names in the path and 'createIfNotExist' is True, create a new
    -- Tree and descend into it.  Otherwise, if it exists we'll have @Just
    -- (TreeEntry {})@, and if not we'll have Nothing.
    y <- case M.lookup name (treeContents t) of
            Nothing -> if createIfNotExist && not (null names)
                       then Just . Git.TreeEntry . Git.Known <$> Git.newTree
                       else return Nothing
            j -> return j

    if null names
        then do
        -- If there are no further names in the path, call the transformer
        -- function, f.  It receives a @Maybe TreeEntry@ to indicate if there
        -- was a previous entry at this path.  It should return a 'Left' value
        -- to propagate out a user-defined error, or a @Maybe TreeEntry@ to
        -- indicate whether the entry at this path should be deleted or
        -- replaced with something new.
        --
        -- NOTE: There is no provision for leaving the entry unchanged!  It is
        -- assumed to always be changed, as we have no reliable method of
        -- testing object equality that is not O(n).
        ze <- f y
        returnTree t name ze

        else
          -- If there are further names in the path, descend them now.  If
          -- 'createIfNotExist' was False and there is no 'Tree' under the
          -- current name, or if we encountered a 'Blob' when a 'Tree' was
          -- required, throw an exception to avoid colliding with user-defined
          -- 'Left' values.
          case y of
              Just (Git.BlobEntry {}) -> failure Git.TreeCannotTraverseBlob
              Just (Git.TreeEntry st) -> do
                  st' <- Git.resolveTreeRef st
                  (st'',ze) <- doModifyTree st' names createIfNotExist f
                  returnTree t name $ if M.null (treeContents st'')
                                      then Nothing
                                      else ze
  where
    returnTree t n z =
        case z of
            Nothing ->
                return (t { treeContents = M.delete n (treeContents t) }, z)
            Just z' ->
                return (t { treeContents = M.insert n z' (treeContents t) }, z)

lgModifyTree
  :: Tree -> FilePath -> Bool
     -> (Maybe TreeEntry -> LgRepository (Maybe TreeEntry))
     -> LgRepository (Maybe TreeEntry)
lgModifyTree t path createIfNotExist f =
    snd <$> doModifyTree t (splitPath path) createIfNotExist f

splitPath :: FilePath -> [Text]
splitPath path = T.splitOn "/" text
  where text = case F.toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

-- Libgit2.hs
