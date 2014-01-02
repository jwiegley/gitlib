{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

{-# OPTIONS_GHC -fno-warn-name-shadowing
                -fno-warn-unused-binds
                -fno-warn-orphans #-}

module Git.Sample ( SampleRepository(..), sampleFactory ) where

import           Control.Applicative
import           Control.Failure
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)
import qualified Git

data Void

instance Show Void where
    show _ = undefined

instance Ord Void where
    _ `compare` _ = undefined

instance Eq Void where
    _ == _ = undefined

instance Git.IsOid Void where
    renderOid _ = undefined

data SampleRepository = SampleRepository

class HasSampleRepository env where
    getSampleRepository :: env -> SampleRepository

instance HasSampleRepository SampleRepository where
    getSampleRepository = id

instance HasSampleRepository (e, SampleRepository) where
    getSampleRepository = snd

instance (Applicative m, Failure Git.GitException m,
          MonadReader env m, HasSampleRepository env)
         => Git.MonadGit SampleRepository m where
    type Oid SampleRepository     = Void
    data Tree SampleRepository    = Void
    data Options SampleRepository = Options

    facts = return Git.RepositoryFacts
        { Git.hasSymbolicReferences = True }

    getRepository     = getSampleRepository <$> ask
    closeRepository   = undefined
    deleteRepository  = undefined

    parseOid          = undefined

    lookupReference   = undefined
    createReference   = undefined
    updateReference   = undefined
    deleteReference   = undefined
    sourceReferences  = undefined

    lookupObject      = undefined
    existsObject      = undefined
    sourceObjects     = undefined

    lookupCommit      = undefined
    lookupTree        = undefined
    lookupBlob        = undefined
    lookupTag         = undefined

    newTreeBuilder    = undefined
    treeOid           = undefined
    treeEntry         = undefined
    sourceTreeEntries = undefined

    hashContents      = undefined
    createBlob        = undefined
    createCommit      = undefined
    createTag         = undefined

    diffContentsWithTree = undefined

sampleFactory :: (MonadReader env m, HasSampleRepository env)
              => Git.RepositoryFactory (ReaderT SampleRepository m) m
                     SampleRepository
sampleFactory = Git.RepositoryFactory
    { Git.openRepository = undefined
    , Git.runRepository  = flip runReaderT
    }

-- Sample.hs
