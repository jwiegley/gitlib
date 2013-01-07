{-# LANGUAGE FlexibleInstances #-}

-- | The 'Oid' type represents a Git hash value, whether partial or full.  In
--   general users do not interact with this type much, as all the top-level
--   functions which expect hash values typically expect strings.
module Data.Git.Oid
       ( Oid(..)
       , COid(..)
       , oidToStr
       , ObjRef(..)
       , Ident(..)
       , wrapOidPtr
       , compareCOid
       , compareCOidLen
       , equalCOid
       , stringToOid )
       where

import           Bindings.Libgit2.Oid
import           Control.Applicative
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Unsafe
import           Data.Git.Error
import           Data.Stringable as S
import           Foreign.C.String
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           System.IO.Unsafe

-- | 'COid' is a type wrapper for a foreign pointer to libgit2's 'git_oid'
--   structure.  Users should not have to deal with this type.
newtype COid = COid (ForeignPtr C'git_oid)

oidToStr :: Ptr C'git_oid -> IO String
oidToStr = c'git_oid_allocfmt >=> peekCString

instance Show COid where
  show (COid coid) = unsafePerformIO $ withForeignPtr coid oidToStr

-- | 'ObjRef' refers to either a Git object of a particular type (if it has
--   already been loaded into memory), or it refers to the 'COid' of that
--   object in the repository.  This permits deferred loading of objects
--   within potentially very large structures, such as trees and commits.
--   However, it also means that every access to a sub-object must use
--   loadObject from the type class 'Updatable'.
data ObjRef a = IdRef COid
              | ObjRef a
              deriving Show

wrapOidPtr :: Ptr C'git_oid -> IO (ObjRef a)
wrapOidPtr = newForeignPtr_ >=> return . IdRef . COid

-- | An 'Ident' abstracts the fact that some objects won't have an identifier
--   until they are written to disk -- even if sufficient information exists
--   to determine that hash value.  If construct as a 'Pending' value, it is
--   an 'IO' action that writes the object to disk and retrieves the resulting
--   hash value from Git; if 'Stored', it is a 'COid' that is known to the
--   repository.
data Ident a = Pending (a -> IO COid)
             | Stored COid

instance Show (Ident a) where
  show (Pending _) = "Ident..."
  show (Stored coid) = show coid

-- | 'Oid' represents either a full or partial SHA1 hash code used to identify
--   Git objects.
data Oid = Oid COid
         | PartialOid COid Int

instance Show Oid where
  show (Oid x) = show x
  show (PartialOid x l) = take l $ show x

-- | Compare two 'COid' values for equality.  More typical is to use 'compare'.
compareCOid :: COid -> COid -> Ordering
(COid x) `compareCOid` (COid y) =
  let c = unsafePerformIO $
            withForeignPtr x $ \x' ->
              withForeignPtr y $ \y' ->
                c'git_oid_cmp x' y'
  in c `compare` 0

-- | Compare two 'COid' values for equality, but only up to a certain length.
compareCOidLen :: COid -> COid -> Int -> Ordering
compareCOidLen (COid x) (COid y) l =
  let c = unsafePerformIO $
            withForeignPtr x $ \x' ->
              withForeignPtr y $ \y' ->
                c'git_oid_ncmp x' y' (fromIntegral l)
  in c `compare` 0

instance Ord COid where
  compare = compareCOid

-- | 'True' if two 'COid' values are equal.
equalCOid :: Ord a => a -> a -> Bool
x `equalCOid` y = (x `compare` y) == EQ

instance Eq COid where
  (==) = equalCOid

instance Eq Oid where
  (Oid x) == (Oid y) = x `equalCOid` y
  (PartialOid x xl) == (PartialOid y yl) =
    xl == yl && compareCOidLen x y xl == EQ
  _ == _ = False

-- | Convert a hash string to a 'Maybe' 'Oid' value.  If the string is less
--   than 40 hexadecimal digits, the result will be of type 'PartialOid'.
--
-- >>> stringToOid "a143ecf"
-- Just a143ecf
-- >>> stringToOid "a143ecf" >>= (\(Just (PartialOid _ l)) -> return $ l == 7)
-- True
--
-- >>> let hash = "6cfc2ca31732fb6fa6b54bae6e586a57a0611aab"
-- >>> stringToOid hash
-- Just 6cfc2ca31732fb6fa6b54bae6e586a57a0611aab
-- >>> stringToOid hash >>= (\(Just (Oid _)) -> return True)
-- True
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
