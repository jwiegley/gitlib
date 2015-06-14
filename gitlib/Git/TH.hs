{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}

module Git.TH (gitDispatcher) where

import Control.Monad
import Data.Foldable as F
import Debug.Trace
import Language.Haskell.TH

runcons :: [t] -> ([t], t)
runcons [] = error "runcons called on empty list"
runcons [x] = ([], x)
runcons (x:xs) = let (ys, y) = runcons xs in (x:ys, y)

gitDispatcher :: Name -> String -> ExpQ
gitDispatcher runF prefix = do
    Just ty <- lookupTypeName "Git.GitExprF"
    TyConI (DataD _ _ _vars ctors _) <- reify ty
    [| \x -> $(caseE [|x|] (map branch ctors)) |]
  where
    branch (NormalC name@(nameBase -> "M") _) = do
        m <- newName "m"
        Just join_f <- lookupValueName "Control.Monad.join"
        let body = AppE (VarE join_f) (VarE m)
        return $ Match (ConP name [VarP m]) (NormalB body) []

    branch (ForallC _ _ (NormalC name@(nameBase -> "Catch") _)) = do
        a <- newName "a"
        h <- newName "h"
        mcatch <- lookupValueName "Control.Monad.Catch.catch"
        case mcatch of
            Nothing -> error "Please import Control.Monad.Catch.catch"
            Just catch -> do
                let body = NormalB $ AppE (AppE (VarE catch) (VarE a)) (VarE h)
                return $ Match (ConP name [VarP a, VarP h]) body []

    branch (NormalC name@(nameBase -> "Lifted") _) = do
        m <- newName "m"
        Just join_f <- lookupValueName "Control.Monad.join"
        let body = AppE (VarE join_f) (AppE (VarE m) (VarE runF))
        return $ Match (ConP name [VarP m]) (NormalB body) []

    branch (NormalC name (runcons -> (tys, ret))) =
        createBranches name (map snd tys) (snd ret)
    branch (RecC name (runcons -> (tys, ret))) =
        createBranches name (map thrd tys) (thrd ret)
      where thrd (_, _, c) = c

    branch x = error $ "Unsupported data constructor in GitExprF: " ++ show x

    createBranches :: Name -> [Type] -> Type -> MatchQ
    createBranches name tys ret = do
        args <- replicateM (1+ length tys) $ newName "a"
        let funName = prefix ++ nameBase name
        mfun <- lookupValueName funName
        expr <- case mfun of
            Nothing  -> do
                funn <- newName funName
                m <- newName "m"
                let endty = case ret of
                        AppT (AppT ArrowT a) _ -> AppT (VarT m) a
                        _ -> AppT (VarT m) (TupleT 0)
                    tyargs = F.foldr1 (\x rest -> AppT (AppT ArrowT x) rest)
                                      (tys ++ [endty])
                    funty  = SigD funn tyargs
                trace ("Not yet implemented: " ++ pprint funty) $
                    [| error ("Not yet implemented: " ++ funName) |]
            Just fun ->
                return $ foldl' AppE (VarE fun) (map VarE (init args))
        -- trace ("ret = " ++ show ret) $ return ()
        eres <- case ret of
            AppT _ _ -> lookupValueName "Control.Monad.>>="
            _ -> lookupValueName "Control.Monad.>>"
        let expr' = case eres of
                Just retf -> AppE (AppE (VarE retf) expr) (VarE (last args))
                Nothing   -> AppE (VarE (last args)) expr
        return $ Match (ConP name (map VarP args)) (NormalB expr') []
