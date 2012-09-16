{-# LANGUAGE FlexibleInstances #-}

module Data.Git.Oid
       ( Oid(..)
       , COid(..)
       , Ident
       , compareCOid
       , compareCOidLen
       , equalCOid
       , stringToOid )
       where

import Bindings.Libgit2.Oid
import Control.Exception
import Control.Monad
import Data.ByteString.Unsafe
import Data.Git.Errors
import Data.Stringable as S
import Foreign.ForeignPtr
import System.IO.Unsafe

newtype COid = COid (ForeignPtr C'git_oid)

instance Show COid where
  show (COid x) =
    toString $ unsafePerformIO $
      withForeignPtr x (unsafePackMallocCString <=< c'git_oid_allocfmt)

type Ident a = Either (a -> IO COid) COid

data Oid = Oid COid
         | PartialOid COid Int
         deriving Show

compareCOid :: COid -> COid -> Ordering
(COid x) `compareCOid` (COid y) =
  let c = unsafePerformIO $
            withForeignPtr x $ \x' ->
              withForeignPtr y $ \y' ->
                c'git_oid_cmp x' y'
  in c `compare` 0

compareCOidLen :: COid -> COid -> Int -> Ordering
compareCOidLen (COid x) (COid y) l =
  let c = unsafePerformIO $
            withForeignPtr x $ \x' ->
              withForeignPtr y $ \y' ->
                c'git_oid_ncmp x' y' (fromIntegral l)
  in c `compare` 0

instance Ord COid where
  compare = compareCOid

equalCOid :: Ord a => a -> a -> Bool
x `equalCOid` y = (x `compare` y) == EQ

instance Eq COid where
  (==) = equalCOid

instance Eq Oid where
  (Oid x) == (Oid y) = x `equalCOid` y
  (PartialOid x xl) == (PartialOid y yl) =
    xl == yl && compareCOidLen x y xl == EQ
  _ == _ = False

stringToOid :: CStringable a => a -> IO (Maybe Oid)
stringToOid str
  | len > 40 = throwIO ObjectIdTooLong
  | otherwise = do
    oid <- mallocForeignPtr
    withCStringable str $ \cstr ->
      withForeignPtr oid $ \ptr -> do
        r <- if len == 40
             then c'git_oid_fromstr ptr cstr
             else c'git_oid_fromstrn ptr cstr (fromIntegral len)
        if r < 0
          then return Nothing
          else return . Just $ if len == 40
                               then Oid (COid oid)
                               else PartialOid (COid oid) len
  where len = S.length str

-- Oid.hs
