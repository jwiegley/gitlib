{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Git.Repository
       ( Oid
       , Ident
       , ObjPtr

       , Updatable(..)

       , Repository
       , HasRepository(..)

       , openRepository
       , createRepository
       , openOrCreateRepository )
       where

import Data.Git.Internal

-- Repository.hs
