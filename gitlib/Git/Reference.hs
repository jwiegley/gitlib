module Git.Reference where

import Conduit
import Git.Types

listReferences :: MonadGit r m => m [RefName]
listReferences = runConduit $ sourceReferences .| sinkList

resolveReference :: MonadGit r m => RefName -> m (Maybe (Oid r))
resolveReference name = do
    mref <- lookupReference name
    maybe (return Nothing) referenceToOid mref

referenceToOid :: MonadGit r m => RefTarget r -> m (Maybe (Oid r))
referenceToOid (RefObj oid)       = return $ Just oid
referenceToOid (RefSymbolic name) = resolveReference name
