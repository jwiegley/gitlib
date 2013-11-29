{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Git.Classy where

import Control.Lens
import Data.Char (toLower)
import Data.Function
import Data.Maybe
import Data.Set as Set hiding (toList,map,filter)
import Language.Haskell.TH
import Prelude hiding (FilePath)

_PrefixRules :: LensRules
_PrefixRules = LensRules mLowerName fld (const Nothing) $
    Set.fromList [SingletonIso, SingletonAndField, CreateClass,
                  CreateInstance, BuildTraversals, GenerateSignatures]
  where
    fld cs = Just ('_':cs)

mLowerName :: String -> Maybe String
mLowerName (c:cs) = Just (toLower c:cs)
mLowerName _ = Nothing

-- | Rules for making lenses and traversals that precompose another 'Lens'.
classy_Rules :: LensRules
classy_Rules = _PrefixRules
  & lensIso .~ const Nothing
  & handleSingletons .~ False
  & lensClass .~ classy
  & classRequired .~ True
  & partialLenses .~ False
  & buildTraversals .~ True
  where
    classy :: String -> Maybe (String, String)
    classy n@(a:as) = Just ("Has" ++ n, toLower a:as)
    classy _ = Nothing

makeClassy_ :: Name -> Q [Dec]
makeClassy_ = makeLensesWith classy_Rules

makeClassy_For :: String -> String -> [(String, String)] -> Name -> Q [Dec]
makeClassy_For clsName funName fields =
    makeLensesWith $ classy_Rules
        & lensClass .~ const (Just (clsName,funName))
        & lensField .~ (`Prelude.lookup` fields)
