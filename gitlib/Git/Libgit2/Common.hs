{-# LANGUAGE OverloadedStrings #-}

module Git.Libgit2.Common
       ( packSignature
       , withSignature )
       where

import qualified Data.ByteString as BS
import qualified Git
import           Git.Libgit2.Internal
import qualified Data.Text as T
import qualified Data.Text.ICU.Convert as U
import           Data.Time
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime,
                                        utcTimeToPOSIXSeconds)
import qualified Prelude

-- | Convert a time in seconds (from Stripe's servers) to 'UTCTime'. See
--   "Data.Time.Format" for more on working with 'UTCTime'.
fromSeconds :: Integer -> UTCTime
fromSeconds  = posixSecondsToUTCTime . fromInteger

-- | Convert a 'UTCTime' back to an Integer suitable for use with Stripe's API.
toSeconds :: UTCTime -> Integer
toSeconds  = round . utcTimeToPOSIXSeconds

peekGitTime :: Ptr (C'git_time) -> IO UTCTime
peekGitTime time =
  -- jww (2012-09-29): Handle offset here
  return . fromSeconds . toInteger . c'git_time'time =<< peek time

packGitTime :: UTCTime -> C'git_time
packGitTime utcTime =
  C'git_time { c'git_time'time   = fromIntegral (toSeconds utcTime)
             , c'git_time'offset = 0 } -- jww (2012-09-29): NYI

packSignature :: U.Converter -> Ptr C'git_signature -> IO Git.Signature
packSignature conv sig = do
  name  <- peek (p'git_signature'name sig)  >>= BS.packCString
  email <- peek (p'git_signature'email sig) >>= BS.packCString
  time  <- peekGitTime (p'git_signature'when sig)
  return $
    Git.Signature { Git.signatureName  = U.toUnicode conv name
                  , Git.signatureEmail = U.toUnicode conv email
                  , Git.signatureWhen  = time }

withSignature :: U.Converter -> Git.Signature
              -> (Ptr C'git_signature -> IO a) -> IO a
withSignature conv sig f =
  BS.useAsCString (U.fromUnicode conv (Git.signatureName sig)) $ \nameCStr ->
  BS.useAsCString (U.fromUnicode conv (Git.signatureEmail sig)) $ \emailCStr ->
  alloca $ \ptr -> do
      poke ptr (C'git_signature nameCStr emailCStr
                (packGitTime (Git.signatureWhen sig)))
      f ptr

-- Common.hs
