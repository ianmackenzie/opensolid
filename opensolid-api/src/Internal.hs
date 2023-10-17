module Internal
  ( Class
  , Function
  , ffi
  , api
  , cls
  , method
  , static
  )
where

import Api qualified
import Control.Monad (return, (>>=))
import CoordinateSystem qualified
import Data.Char (isUpper, toLower, toUpper)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.String (String, fromString)
import Data.Tuple (fst)
import Foreign (StablePtr, deRefStablePtr, freeStablePtr, newStablePtr)
import Foreign.Storable (Storable)
import Language.Haskell.TH qualified as TH
import List (map2)
import OpenSolid hiding (fail, fromString, (+), (++), (>>=))
import Prelude (Traversable (..), concat, foldl, foldr, map, mapM, maybe, unzip, (.), (<$>))

data Class = Class TH.Name (List TH.Name) (List Function)

data Function = Function TH.Name TH.Name (List String)

cls :: TH.Name -> List TH.Name -> List Function -> Class
cls = Class

method :: TH.Name -> List String -> Function
method = Function 'Api.Method

static :: TH.Name -> List String -> Function
static = Function 'Api.Static

ffi :: List Class -> TH.Q (List TH.Dec)
ffi classes = do
  functionDecls <- concat <$> mapM ffiClass classes
  freeFunctionDecl <- freeFunctionFfi 'freeStablePtr
  return $ freeFunctionDecl : functionDecls
 where
  ffiClass (Class _ _ functions) =
    concat <$> mapM ffiFunction functions

freeFunctionFfi :: TH.Name -> TH.Q TH.Dec
freeFunctionFfi name = do
  freeFunctionType <- TH.reifyType name
  return $ TH.ForeignD (TH.ExportF TH.CCall "opensolid_free" name freeFunctionType)

api :: List Class -> TH.Q TH.Exp
api classes = do
  apiCls <- mapM apiClass classes
  return $ TH.AppE (TH.ConE 'Api.Api) (TH.ListE apiCls)

-- human readable name in the API
apiName :: TH.Name -> TH.Exp
apiName name =
  TH.LitE (TH.StringL (camelToSnake (TH.nameBase name)))

apiClass :: Class -> TH.Q TH.Exp
apiClass (Class name representationProps functions) = do
  apiFns <- mapM apiFunction functions
  return $
    TH.ConE 'Api.Class
      `TH.AppE` TH.LitE (TH.StringL (TH.nameBase name))
      `TH.AppE` TH.ListE (map apiName representationProps)
      `TH.AppE` TH.ListE apiFns

apiFunction :: Function -> TH.Q TH.Exp
apiFunction (Function kind fnName argNames) = do
  fnType <- TH.reifyType fnName
  (argTypes, returnType) <- ffiFunctionType fnType
  return $
    TH.ConE 'Api.Function
      `TH.AppE` TH.ConE kind
      `TH.AppE` TH.LitE (TH.StringL (camelToSnake (ffiFunctionName fnName))) -- ffi name
      `TH.AppE` apiName fnName
      `TH.AppE` TH.ListE
        ( map2
            (\a t -> TH.TupE [Just (TH.LitE (TH.StringL a)), Just (apiType t)])
            argNames
            argTypes
        )
      `TH.AppE` apiType returnType

apiType :: TH.Type -> TH.Exp
apiType (TH.AppT (TH.ConT name) typ)
  | name == ''StablePtr = TH.ConE 'Api.Pointer `TH.AppE` typeNameBase typ
  | name == ''Maybe = TH.ConE 'Api.Maybe `TH.AppE` apiType typ
  | otherwise = apiType (TH.ConT name)
apiType (TH.ConT name)
  | name == ''Float = TH.ConE 'Api.Float
  | name == ''Qty = TH.ConE 'Api.Float
  | name == ''Angle = TH.ConE 'Api.Float
  | name == ''Bool = TH.ConE 'Api.Boolean
apiType _ = TH.ConE 'NotImplemented -- this should break the TH generated code

data NotImplemented = NotImplemented

typeNameBase :: TH.Type -> TH.Exp
typeNameBase (TH.AppT t _) = typeNameBase t
typeNameBase (TH.ConT name) = TH.LitE (TH.StringL (TH.nameBase name))
typeNameBase _ = TH.ConE 'NotImplemented -- this should break the TH generated code

-- Generates wrapper function type from the original function
-- Returns a list of argument types and return type
ffiFunctionType :: TH.Type -> TH.Q (List TH.Type, TH.Type)
ffiFunctionType (TH.ForallT _ _ innerType) = ffiFunctionType innerType
ffiFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) = do
  (args, returnType) <- ffiFunctionType rest
  typ <- ffiType arg
  return (typ : args, returnType)
ffiFunctionType returnType = do
  typ <- ffiType returnType
  return ([], typ)

-- Hack: replace 'units', 'units1', 'units2', `frameUnits` etc. with 'Unitless' in all types
-- since the FFI only deals with unitless values
fixupUnits :: TH.Type -> TH.Type
-- Replace any type variable whose name contains 'units' with 'Unitless'
fixupUnits (TH.VarT name) | "units" `isInfixOf` map toLower (TH.nameBase name) = TH.ConT ''Unitless
-- In types with parameters, recursively fix up each parameter
fixupUnits (TH.AppT t a) = TH.AppT (fixupUnits t) (fixupUnits a)
-- Otherwise, do nothing
fixupUnits typ = typ

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
-- We wrap a type in a StablePtr if it doesn't implement Storable
ffiType :: TH.Type -> TH.Q TH.Type
ffiType (TH.AppT (TH.ConT containerTyp) fixedTyp) | containerTyp == ''Maybe = do
  typ <- ffiType fixedTyp
  return $ TH.AppT (TH.ConT ''Maybe) typ
ffiType typ = do
  let fixedTyp = typ |> fixupUnits |> fixupCoordinateSystem
  instances <- TH.reifyInstances ''Storable [fixedTyp]
  if instances == []
    then return (TH.AppT (TH.ConT ''StablePtr) fixedTyp)
    else return fixedTyp

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
toStablePtrOrReturn (TH.AppT (TH.ConT name) (TH.AppT (TH.ConT name2) _))
  | name == ''Maybe && name2 == ''StablePtr = \exp -> TH.VarE '(<$>) `TH.AppE` TH.VarE 'newStablePtr `TH.AppE` exp
toStablePtrOrReturn (TH.AppT (TH.ConT name) _) | name == ''StablePtr = TH.AppE (TH.VarE 'newStablePtr)
toStablePtrOrReturn _ = TH.AppE (TH.VarE 'return)

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
ffiFunction :: Function -> TH.Q (List TH.Dec)
ffiFunction (Function _ fnName _) = do
  fnType <- TH.reifyType fnName
  (argTypes, returnType) <- ffiFunctionType fnType
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
