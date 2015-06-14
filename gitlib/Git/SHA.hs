{-# LANGUAGE OverloadedStrings #-}

module Git.SHA where

import           Data.ByteString
import qualified Data.ByteString.Base16 as B16
import           Data.Hashable
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

newtype SHA = SHA { getSHA :: ByteString } deriving (Eq, Ord, Read)

shaToText :: SHA -> Text
shaToText (SHA bs) = T.decodeUtf8 (B16.encode bs)

textToSha :: Monad m => Text -> m SHA
textToSha t =
    case B16.decode $ T.encodeUtf8 t of
        (bs, "") -> return (SHA bs)
        _ -> fail "Invalid base16 encoding"

instance Show SHA where
    show = T.unpack . shaToText

instance Hashable SHA where
    hashWithSalt salt (SHA bs) = hashWithSalt salt bs
