module Git.Reference where

import           Git.DSL
import           Git.Types
import qualified Streaming.Prelude as S

listReferences :: Monad m => GitT r m [RefName]
listReferences = S.toList_ allReferences

resolveReference :: Monad m => RefName -> GitT r m (Maybe (Oid r))
resolveReference name = do
    mref <- lookupReference name
    maybe (return Nothing) referenceToOid mref

referenceToOid :: Monad m => RefTarget r -> GitT r m (Maybe (Oid r))
referenceToOid (RefObj oid)       = return $ Just oid
referenceToOid (RefSymbolic name) = resolveReference name
