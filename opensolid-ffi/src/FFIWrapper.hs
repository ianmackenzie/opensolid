module FFIWrapper (wrapFunction) where

import Control.Monad (return, (>>=))
import CoordinateSystem qualified
import Data.Char (isUpper, toLower, toUpper)
import Data.Maybe (fromMaybe)
import Data.String (String, fromString)
import Data.Tuple (fst)
import Foreign (StablePtr, deRefStablePtr, newStablePtr)
import GHC.IO
import Language.Haskell.TH qualified as TH
import OpenSolid hiding (fail, fromString, (+), (++), (>>=))
import Prelude (Traversable (..), concat, foldl, foldr, map, mapM, maybe, unzip, (.))

-- Generates wrapper function type from the original function
-- Returns a list of argument types and return type
ffiFunctionType :: TH.Type -> ([TH.Type], TH.Type)
ffiFunctionType (TH.ForallT _ _ innerType) = ffiFunctionType innerType
ffiFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) =
  let (args, returnType) = ffiFunctionType rest
   in (ffiType arg : args, returnType)
ffiFunctionType returnType = ([], ffiType returnType)

-- Alias for coordinate system `@` type, since that seems to confuse Template Haskell (see below)
type Coords space units = space @ units

-- Template Haskell seems to get confused by type-level use of '@' operator,
-- so replace any instance of `space @ units` with the equivalent `'Coords space units`
fixupCoordinateSystem :: TH.Type -> TH.Type
-- Replace `@` with `Coords`
fixupCoordinateSystem (TH.ConT name) | name == ''(CoordinateSystem.@) = TH.ConT ''Coords
-- In types with parameters, recursively fix up each parameter
fixupCoordinateSystem (TH.AppT t a) = TH.AppT (fixupCoordinateSystem t) (fixupCoordinateSystem a)
-- Otherwise, do nothing
fixupCoordinateSystem typ = typ

-- Modify types for FFI
ffiType :: TH.Type -> TH.Type
-- Int and Bool can be passed directly
ffiType typ@(TH.ConT name) | name == ''Int || name == ''Bool || name == ''Float || name == ''Angle = typ
-- Any Qty type can be passed directly
ffiType typ@(TH.AppT (TH.ConT name) _) | name == ''Qty = typ
-- All other types should be wrapped in a StablePtr
ffiType typ = TH.AppT (TH.ConT ''StablePtr) (fixupCoordinateSystem typ)

-- Is the type wrapped in a StablePtr?
isPointer :: TH.Type -> Bool
isPointer (TH.AppT (TH.ConT name) _) = name == ''StablePtr
isPointer _ = False

-- Convert a StablePtr to an expression and an optional bind statement
fromStablePtr :: (TH.Name, Maybe TH.Name) -> (TH.Exp, Maybe TH.Stmt)
fromStablePtr (argName, Just newName) =
  ( TH.VarE newName
  , Just (TH.BindS (TH.VarP newName) (TH.AppE (TH.VarE 'deRefStablePtr) (TH.VarE argName)))
  )
fromStablePtr (argName, Nothing) = (TH.VarE argName, Nothing)

-- Convert an expression to a StablePtr
toStablePtrOrReturn :: TH.Type -> TH.Exp -> TH.Exp
toStablePtrOrReturn argType expr
  | isPointer argType = TH.AppE (TH.VarE 'newStablePtr) expr
  | otherwise = TH.AppE (TH.VarE 'return) expr

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x : xs)
  | isUpper x = '_' : toLower x : camelToSnake xs
  | otherwise = x : camelToSnake xs

uppercaseFirstChar :: String -> String
uppercaseFirstChar [] = []
uppercaseFirstChar (x : xs) = toUpper x : xs

-- Generate the argument name, and a temporary name for unwrapping pointer inside do
makeArgNames :: TH.Type -> TH.Q (TH.Name, Maybe TH.Name)
makeArgNames argType
  | isPointer argType = do
      original <- TH.newName "x"
      unwrapped <- TH.newName "y"
      return (original, Just unwrapped)
  | otherwise = do
      original <- TH.newName "x"
      return (original, Nothing)

-- Generate a name for the FII wrapper function
-- ''xCoordinate from the Point2d module becomes `opensolidPoint2dXCoordinate`
ffiFunctionName :: TH.Name -> String
ffiFunctionName fnName =
  concat
    [ "opensolid"
    , fromMaybe "" (TH.nameModule fnName)
    , uppercaseFirstChar $ TH.nameBase fnName
    ]

-- Wrap the function with an FFI
wrapFunction :: TH.Name -> TH.Q [TH.Dec]
wrapFunction fnName = do
  fnType <- TH.reifyType fnName
  let (argTypes, returnType) = ffiFunctionType fnType
  argNames <- mapM makeArgNames argTypes

  let foreignName = ffiFunctionName fnName
      wrapperName = TH.mkName foreignName
      arguments = map (TH.VarP . fst) argNames
      (argExprs, optionalBindStatements) = unzip $ map fromStablePtr argNames
      wrapperBody =
        TH.DoE Nothing $
          foldl
            (\statements optStmt -> maybe statements (\x -> x : statements) optStmt)
            [TH.NoBindS $ toStablePtrOrReturn returnType $ foldl TH.AppE (TH.VarE fnName) argExprs]
            optionalBindStatements
      wrapperType = foldr (\a b -> TH.AppT (TH.AppT TH.ArrowT a) b) (TH.AppT (TH.ConT ''IO) returnType) argTypes
      wrapperClause = TH.Clause arguments (TH.NormalB wrapperBody) []

  return
    [ TH.ForeignD (TH.ExportF TH.CCall (camelToSnake foreignName) wrapperName wrapperType)
    , TH.SigD wrapperName wrapperType
    , TH.FunD wrapperName [wrapperClause]
    ]
