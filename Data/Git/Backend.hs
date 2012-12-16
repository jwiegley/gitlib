{-# LANGUAGE OverloadedStrings #-}

module Data.Git.Backend
       ( odbBackendAdd )
       where

import           Data.ByteString as B hiding (map, putStrLn)
import qualified Data.ByteString.Char8 as BC
import           Data.ByteString.Unsafe
import           Data.Git.Error
import           Data.Git.Internal
import qualified Prelude

default (Text)

odbBackendAdd :: Repository -> Ptr C'git_odb_backend -> Int
                 -> IO (Result Repository)
odbBackendAdd repo' backend priority =
  withForeignPtr repo $ \repoPtr ->
    alloca $ \odbPtr -> do
      r <- c'git_repository_odb odbPtr repoPtr
      if r < 0
        then return (Left "Cannot get repository ODB")
        else do
        odb <- peek odbPtr
        r2 <- c'git_odb_add_backend odb backend (fromIntegral priority)
        if r2 < 0
          then return (Left "Cannot add backend to repository ODB")
          else return (Right repo')

  where
    repo = fromMaybe (throw RepositoryInvalid) (repoObj repo')

-- Backend.hs
