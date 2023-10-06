module FFIWrapper (wrapFunction) where

import Control.Monad (replicateM, return, (>>=))
import CoordinateSystem qualified
import Data.Char (isUpper, toLower)
import Data.String (String, fromString)
import Foreign (StablePtr, deRefStablePtr, newStablePtr)
import GHC.IO (unsafePerformIO)
import Language.Haskell.TH qualified as TH
import OpenSolid hiding (fail, fromString, (+), (++), (>>=))
import Prelude (foldl, foldr, map, zipWith, (+), (++))

-- Split a function type into argument types and return type
splitFunctionType :: TH.Type -> ([TH.Type], TH.Type)
splitFunctionType (TH.ForallT _ _ innerType) = splitFunctionType innerType
splitFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) =
  let (args, returnType) = splitFunctionType rest
   in (ffiType arg : args, returnType)
splitFunctionType returnType = ([], ffiType returnType)

-- Count the number of arguments in a function type
countArgs :: TH.Type -> Int
countArgs (TH.ForallT _ _ innerType) = countArgs innerType
countArgs (TH.AppT (TH.AppT TH.ArrowT _) rest) = 1 + countArgs rest
countArgs _ = 0

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
ffiType typ@(TH.ConT name) | name == ''Int || name == ''Bool = typ
-- Any Qty type can be passed directly
ffiType typ@(TH.AppT (TH.ConT name) _) | name == ''Qty = typ
-- All other types should be wrapped in a StablePtr
ffiType typ = TH.AppT (TH.ConT ''StablePtr) (fixupCoordinateSystem typ)

-- Is the type wrapped in a StablePtr?
isPointer :: TH.Type -> Bool
isPointer (TH.AppT (TH.ConT name) _) = name == ''StablePtr
isPointer _ = False

-- Convert a StablePtr to an expression
fromStablePtrE :: TH.Type -> TH.Exp -> TH.Exp
fromStablePtrE argType expr =
  if isPointer argType
    then TH.AppE (TH.VarE 'unsafePerformIO) $ TH.AppE (TH.VarE 'deRefStablePtr) expr
    else expr

-- Convert an expression to a StablePtr
toStablePtrE :: TH.Type -> TH.Exp -> TH.Exp
toStablePtrE argType expr =
  if isPointer argType
    then TH.AppE (TH.VarE 'unsafePerformIO) $ TH.AppE (TH.VarE 'newStablePtr) expr
    else expr

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x : xs) =
  if isUpper x
    then '_' : toLower x : camelToSnake xs
    else x : camelToSnake xs

-- Wrap the function with an FFI
wrapFunction :: String -> TH.Name -> TH.Q [TH.Dec]
wrapFunction prefix fnName = do
  fnType <- TH.reifyType fnName
  argNames <- replicateM (countArgs fnType) (TH.newName "x")

  let foreignName = prefix ++ camelToSnake (TH.nameBase fnName)
      wrapperName = TH.mkName foreignName
      arguments = map TH.VarP argNames
      (argTypes, returnType) = splitFunctionType fnType
      argExprs = zipWith fromStablePtrE argTypes (map TH.VarE argNames)
      wrapperBody = TH.AppE (TH.VarE 'return) $ toStablePtrE returnType $ foldl TH.AppE (TH.VarE fnName) argExprs
      wrapperType = foldr (\a b -> TH.AppT (TH.AppT TH.ArrowT a) b) (TH.AppT (TH.ConT ''IO) returnType) argTypes
      wrapperClause = TH.Clause arguments (TH.NormalB wrapperBody) []

  return
    [ TH.ForeignD (TH.ExportF TH.CCall foreignName wrapperName wrapperType)
    , TH.SigD wrapperName wrapperType
    , TH.FunD wrapperName [wrapperClause]
    ]
