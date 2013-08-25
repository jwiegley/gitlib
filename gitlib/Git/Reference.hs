module Git.Reference where

import Control.Applicative
import Git.Types

resolveReference :: Repository m => RefName -> m (Maybe (CommitOid m))
resolveReference name = do
    mref <- lookupReference name
    maybe (return Nothing) referenceToOid mref

referenceToOid :: Repository m => RefTarget m -> m (Maybe (CommitOid m))
referenceToOid (RefObj oid)       = return $ Just oid
referenceToOid (RefSymbolic name) = resolveReference name

resolveReferenceTree :: Repository m => RefName -> m (Maybe (Tree m))
resolveReferenceTree refName = do
    c <- resolveReference refName
    case c of
        Nothing -> return Nothing
        Just c' -> Just <$> (lookupCommit c' >>= lookupTree . commitTree)
