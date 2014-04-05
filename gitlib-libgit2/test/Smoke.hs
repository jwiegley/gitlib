module Main where

import qualified Git.Libgit2 as Lg
import qualified Git.Smoke as Git
import           Test.Hspec.HUnit ()
import           Test.Hspec.Runner
import           Control.Monad.Catch
import           Control.Monad.Logger

instance MonadCatch m => MonadCatch (LoggingT m) where
  catch (LoggingT m) c =
      LoggingT $ \r -> m r `catch` \e -> runLoggingT (c e) r
  mask a = LoggingT $ \e -> mask $ \u -> runLoggingT (a $ q u) e
    where q u (LoggingT b) = LoggingT (u . b)
  uninterruptibleMask a =
    LoggingT $ \e -> uninterruptibleMask $ \u -> runLoggingT (a $ q u) e
      where q u (LoggingT b) = LoggingT (u . b)

instance MonadCatch m => MonadCatch (NoLoggingT m) where
    catch (NoLoggingT m) c =
        NoLoggingT $ m `catch` \e -> runNoLoggingT (c e)
    mask a = NoLoggingT $ mask $ \u -> runNoLoggingT (a $ q u)
      where q u (NoLoggingT b) = NoLoggingT $ u b
    uninterruptibleMask a =
        NoLoggingT $ uninterruptibleMask $ \u -> runNoLoggingT (a $ q u)
      where q u (NoLoggingT b) = NoLoggingT $ u b

main :: IO ()
main = hspec $ Git.smokeTestSpec Lg.lgFactory Lg.lgFactory

-- Smoke.hs ends here
