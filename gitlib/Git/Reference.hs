module Git.Reference where

import Git.Types

resolveReference :: Repository m => RefName -> m (Maybe (Oid m))
resolveReference name = do
    mref <- lookupReference name
    maybe (return Nothing) referenceToOid mref

referenceToOid :: Repository m => RefTarget m -> m (Maybe (Oid m))
referenceToOid (RefObj oid)       = return $ Just oid
referenceToOid (RefSymbolic name) = resolveReference name
