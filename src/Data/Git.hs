{-# LANGUAGE OverloadedStrings #-}

import           Data.ByteString.Lazy as BL hiding (map)
import           Data.DateTime
import           Data.Maybe
import           Data.Map as M hiding (map)
import           Data.Text as T hiding (map)
import           Filesystem.Path.CurrentOS
import           Prelude hiding (FilePath)

default (Text)

type GitHash = Integer

data GitAuthor = GitAuthor { authorName  :: Text
                           , authorEmail :: Text
                           }
               deriving (Eq, Show)

data GitInfo = GitInfo { infoAuthor        :: GitAuthor
                       , infoAuthorDate    :: DateTime
                       , infoCommitter     :: GitAuthor
                       , infoCommitterDate :: DateTime
                       }
             deriving (Eq, Show)

data GitBlob = GitBlob { blobHash     :: Maybe GitHash
                       , blobContents :: ByteString
                       }
             deriving (Eq, Show)

type GitTreeOrBlob = Either GitBlob GitTree
type GitTreeMap = Map Text GitTreeOrBlob

data GitTree = GitTree { treeHash     :: Maybe GitHash
                       , treeContents :: GitTreeMap
                       }
             deriving (Eq, Show)

data GitCommit = GitCommit { commitHash :: Maybe GitHash
                           , commitInfo :: GitInfo
                           , commitLog  :: Text
                           , commitTree :: GitTree
                           }
               deriving (Eq, Show)

data GitTag = GitTag { tagHash :: Maybe GitHash
                     , tagRef  :: GitHash
                     }
            deriving (Eq, Show)

data GitObject = Blob   GitBlob
               | Tree   GitTree
               | Commit GitCommit
               | Tag    GitTag
               deriving (Eq, Show)

data GitRepository = GitRepository { repositoryPath :: FilePath }

createBlob :: ByteString -> GitBlob
createBlob = undefined

writeBlob :: GitRepository -> GitBlob -> IO GitBlob
writeBlob = undefined

-- | Create a new tree, starting it with the contents at the given path.
--
--   Note that since empty trees cannot exist in Git, no means is provided for
--   creating one.
createTree :: FilePath -> GitTreeOrBlob -> GitTree
createTree path item = updateTree path item emptyTree

emptyTree :: GitTree
emptyTree = GitTree { treeHash     = Nothing
                    , treeContents = M.empty }

doUpdateTree :: [Text] -> GitTreeOrBlob -> GitTree -> GitTree
doUpdateTree (x:[]) item tree = tree { treeHash     = Nothing
                                     , treeContents = insert x item treeMap }
  where treeMap = treeContents tree

doUpdateTree (x:xs) item tree = tree { treeHash     = Nothing
                                     , treeContents = insert x subTree treeMap }
  where treeMap = treeContents tree
        subTree = Right $ doUpdateTree xs item tree'
        tree'   = case M.lookup x treeMap of
                    Nothing        -> emptyTree
                    Just (Left _)  -> emptyTree
                    Just (Right m) -> m

doUpdateTree [] _ _ = undefined

updateTree :: FilePath -> GitTreeOrBlob -> GitTree -> GitTree
updateTree = doUpdateTree . splitPath

splitPath :: FilePath -> [Text]
splitPath path = splitOn "/" text
  where text = case toText path of
                 Left x  -> error $ "Invalid path: " ++ T.unpack x
                 Right y -> y

writeTree :: GitRepository -> GitTree -> IO GitTree
writeTree = undefined

-- Git.hs
