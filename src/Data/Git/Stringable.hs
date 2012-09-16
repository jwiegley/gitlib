{-# LANGUAGE FlexibleInstances #-}

module Data.Git.Stringable
       ( Stringable(..)
       , CStringable(..) )
       where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Unsafe as BU
import           Data.Either
import qualified Data.Text as T hiding (map)
import qualified Data.Text.Encoding as E
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as EL
import           Filesystem.Path.CurrentOS hiding (empty, concat)
import           Foreign.C.String
import           Prelude hiding (FilePath)

class Stringable a where
  toString   :: a -> String
  fromString :: String -> a
  lengthStr  :: a -> Int

instance Stringable String where
  toString   = id
  fromString = id
  lengthStr  = length

instance Stringable T.Text where
  toString   = T.unpack
  fromString = T.pack
  lengthStr  = T.length

instance Stringable TL.Text where
  toString   = TL.unpack
  fromString = TL.pack
  lengthStr  = undefined

instance Stringable B.ByteString where
  toString   = T.unpack . E.decodeUtf8
  fromString = E.encodeUtf8 . T.pack
  lengthStr  = B.length

instance Stringable BL.ByteString where
  toString   = TL.unpack . EL.decodeUtf8
  fromString = EL.encodeUtf8 . TL.pack
  lengthStr  = undefined

instance Stringable FilePath where
  toString   = toString . either id id . toText
  fromString = fromText . T.pack
  lengthStr  = undefined

class Stringable a => CStringable a where
  withCStringable :: a -> (CString -> IO b) -> IO b
  withCStringable = withCString . toString

  withCStringLenable :: a -> (CString -> Int -> IO b) -> IO b
  withCStringLenable str f = withCStringLen (toString str) (uncurry f)

instance CStringable String where
  withCStringable = withCString

withByteString :: B.ByteString -> (CString -> IO a) -> IO a
withByteString = BU.unsafeUseAsCString

withByteStringLen :: B.ByteString -> (CString -> Int -> IO a) -> IO a
withByteStringLen str f = BU.unsafeUseAsCStringLen str (uncurry f)

instance CStringable T.Text where
  withCStringable    = withCStringable . E.encodeUtf8
  withCStringLenable = withCStringLenable . E.encodeUtf8

instance CStringable TL.Text where
  withCStringable    = withCStringable . EL.encodeUtf8
  withCStringLenable = withCStringLenable . EL.encodeUtf8

instance CStringable B.ByteString where
  withCStringable    = withByteString
  withCStringLenable = withByteStringLen

instance CStringable BL.ByteString where
  withCStringable    = withByteString . B.concat . BL.toChunks
  withCStringLenable = withByteStringLen . B.concat . BL.toChunks

-- Stringable.hs
