{-# LANGUAGE OverloadedStrings #-}

-- | Interface for opening and creating repositories.  Repository objects are
--   immutable, and serve only to refer to the given repository.  Any data
--   associated with the repository — such as the list of branches — is
--   queried as needed.
module Data.Git.Repository
       ( ObjPtr

       , Updatable(..)

       , Repository(..)
       , openRepository
       , createRepository
       , openOrCreateRepository

       , lookupObject'
       , withObject
       , withObjectPtr
       )
       where

import Data.Git.Internal

-- Repository.hs
