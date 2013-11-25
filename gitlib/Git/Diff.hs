module Git.Diff where

-- import           Control.Applicative
-- import           Control.Monad
-- import           Control.Monad.Trans.Class
import           Data.ByteString as B
-- import           Data.Conduit
-- import qualified Data.Conduit.List as CList
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
-- import           Data.Tagged
-- import           Data.Text as T
-- import           Data.Text.Encoding as T
import           Git.Types

diffContentsWithTree :: HashMap TreeFilePath (BlobContents m) -> Tree m
                     -> m ByteString
diffContentsWithTree contents tree = undefined
