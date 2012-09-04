module Main where

import Control.Applicative
import Control.Monad
import Data.Foldable hiding (concat, forM_, mapM_)
import Data.List
import Data.Maybe
import Language.C.Data.Ident
import Language.C.Data.Node
import Language.C.Data.Position
import Language.C.Parser
import Language.C.Syntax.AST
import Language.C.System.GCC
import Language.C.System.Preprocess
import Prelude
import System.Directory
import System.Environment
import Text.StringTemplate

main :: IO()
main = do
  gccExe <- findExecutable "gcc"
  case gccExe of
    Nothing   -> error "Cannot find 'gcc' executable on the PATH"
    Just path -> getArgs >>= parseCFile path

parseCFile :: FilePath -> [String] -> IO ()
parseCFile gccPath args = do
  fileName <- canonicalizePath $ last args
  let cppArgs = rawCppArgs (init args) fileName
  result <- runPreprocessor (newGCC gccPath) cppArgs
  case result of
    Left err -> error $ "Failed to run cpp on: " ++ show err
    Right stream ->
      case parseC stream (initPos fileName) of
        Left err -> error $ "Failed to compile: " ++ show err
        Right (CTranslUnit decls _) ->
          mapM_ printNode $ filter (declInFile fileName) decls

declInfo :: CExtDecl -> NodeInfo
declInfo (CDeclExt (CDecl _ _ info))       = info
declInfo (CFDefExt (CFunDef _ _ _ _ info)) = info
declInfo (CAsmExt _ info)                  = info

infoFile :: NodeInfo -> String
infoFile = posFile . posOfNode

declInFile :: FilePath -> CExtDecl -> Bool
declInFile fileName = (fileName ==) . infoFile . declInfo

printNode :: CExtDecl -> IO ()
printNode (CDeclExt (CDecl declSpec decls info)) =
  forM_ decls $ \(declrtr, initr, expr) ->
    case declrtr of
      Nothing -> return ()
      Just d  ->
        putStrLn $ fromMaybe "<no name>" (declaratorName d)

printNode (CFDefExt (CFunDef declSpecs declrtr decls stmt info)) = do
  printFunc "#cinline" declSpecs declrtr

printNode _ = return ()

nameFromIdent :: Maybe Ident -> String
nameFromIdent name = case name of
  Just (Ident n _ _) -> n
  _ -> "<no name>"

printFunc :: String -> [CDeclarationSpecifier a] -> CDeclarator b -> IO ()
printFunc marker declSpecs (CDeclr ident ddrs _ _ _) = do
  let name' = nameFromIdent ident
      tmpl  = "$marker$ $name$ , $argTypes;separator=' -> '$ -> IO ($retType$)"
      code  = newSTMP tmpl
      code' = setAttribute "argTypes" (getArgTypes (head ddrs)) code
  putStrLn $ toString $ setManyAttrib
    [ ("marker",  marker)
    , ("name",    name')
    , ("retType", fullTypeName declSpecs) ] code'

getArgTypes :: CDerivedDeclarator a -> [String]
getArgTypes (CFunDeclr (Right (decls, _)) _ _) =
  flip map decls $
    \(CDecl declSpecs more _) ->
      -- Get the base type of the argument
      let baseType = fullTypeName declSpecs in
      -- Now determine if the declarator (if there is one) has pointer type
      case head more of
        (Nothing, _, _) -> baseType
        (Just (CDeclr _ dds _ _ _), _, _) ->
          let fun [] = baseType
              fun (x:[]) = case x of
                CPtrDeclr _ _ -> "Ptr <" ++ baseType ++ ">"
                _ -> ""
              fun (x:xs) = case x of
                CPtrDeclr _ _ -> "Ptr (" ++ fun xs ++ ")"
                _ -> ""
          in fun dds

getArgTypes (CPtrDeclr _ _)          = undefined
getArgTypes (CArrDeclr _ _ _)        = undefined
getArgTypes (CFunDeclr (Left _) _ _) = undefined

data Signedness = None
                | Signed
                | Unsigned
                deriving (Eq, Show, Enum)

fullTypeName :: [CDeclarationSpecifier a] -> String
fullTypeName = flip typeNameFromDeclSpecs None

typeNameFromDeclSpecs :: [CDeclarationSpecifier a] -> Signedness -> String
typeNameFromDeclSpecs [] _     = ""
typeNameFromDeclSpecs (x:xs) s =
  case x of
    CTypeSpec (CSignedType _) -> typeNameFromDeclSpecs xs Signed
    CTypeSpec (CUnsigType _)  -> typeNameFromDeclSpecs xs Unsigned
    CTypeSpec tspec           -> fromMaybe "" $ typeName tspec s
    _                         -> typeNameFromDeclSpecs xs s

typeName :: CTypeSpecifier a -> Signedness -> Maybe String
typeName (CVoidType _) _   = Just ""
typeName (CCharType _) s   = case s of
                               Signed   -> Just "CSChar"
                               Unsigned -> Just "CUChar"
                               _        -> Just "CChar"
typeName (CShortType _) s  = case s of
                               Signed   -> Just "CShort"
                               Unsigned -> Just "CUShort"
                               _        -> Just "CShort"
typeName (CIntType _) s    = case s of
                               Signed   -> Just "CInt"
                               Unsigned -> Just "CUInt"
                               _        -> Just "CInt"
typeName (CLongType _) s   = case s of
                               Signed   -> Just "CLong"
                               Unsigned -> Just "CULong"
                               _        -> Just "CLong"
typeName (CFloatType _) _  = Just "CFloat"
typeName (CDoubleType _) _ = Just "CDouble"
typeName (CBoolType _) _   = Just "CInt"
--typeName (CComplexType _)  = Nothing
--typeName (CSUType _ _)     = Nothing
--typeName (CEnumType _ _)   = Nothing
typeName (CTypeDef (Ident name _ _) _) _ = Just name
--typeName (CTypeOfExpr _ _) = Nothing
--typeName (CTypeOfType _ _) = Nothing
typeName _ _ = Nothing

declaratorName :: CDeclarator t -> Maybe String
declaratorName (CDeclr (Just (Ident name _ _)) _ _ _ _) = Just name
declaratorName _ = Nothing

-- c2hsc.hs
