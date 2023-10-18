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
import Foreign
  ( Ptr
  , castPtrToStablePtr
  , castStablePtrToPtr
  , deRefStablePtr
  , freeStablePtr
  , malloc
  , newStablePtr
  , nullPtr
  , poke
  )
import Foreign.Storable (Storable)
import Language.Haskell.TH qualified as TH
import List (map2)
import OpenSolid hiding (fail, fromString, (+), (++), (<>), (>>=))
import Prelude (Show (show), Traversable (..), concat, error, foldl, foldr, map, mapM, maybe, (<$>), (<>))

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
  freeFunctionDecl <- freeFunctionFfi 'freePtr
  return $ freeFunctionDecl : functionDecls
 where
  ffiClass (Class _ _ functions) =
    concat <$> mapM ffiFunction functions

freePtr :: Ptr () -> IO ()
freePtr ptr
  | ptr == nullPtr = return ()
  | otherwise = freeStablePtr (castPtrToStablePtr ptr)

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
  (argTypes, returnType) <- apiFunctionType fnType
  return $
    TH.ConE 'Api.Function
      `TH.AppE` TH.ConE kind
      `TH.AppE` TH.LitE (TH.StringL (camelToSnake (ffiFunctionName fnName))) -- ffi name
      `TH.AppE` apiName fnName
      `TH.AppE` TH.ListE
        ( map2
            (\a t -> TH.TupE [Just (TH.LitE (TH.StringL a)), Just t])
            argNames
            argTypes
        )
      `TH.AppE` returnType

apiFunctionType :: TH.Type -> TH.Q (List TH.Exp, TH.Exp)
apiFunctionType (TH.ForallT _ _ innerType) = apiFunctionType innerType
apiFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) = do
  (args, returnType) <- apiFunctionType rest
  typ <- apiType arg
  return (typ : args, returnType)
apiFunctionType returnType = do
  typ <- apiType returnType
  return ([], typ)

apiType :: TH.Type -> TH.Q TH.Exp
apiType (TH.AppT (TH.ConT containerTyp) nestedTyp) | containerTyp == ''Maybe = do
  typ <- apiType nestedTyp
  return $ TH.ConE 'Api.Maybe `TH.AppE` typ
apiType typ = do
  isPtr <- isPointer typ
  if isPtr
    then return $ TH.ConE 'Api.Pointer `TH.AppE` typeNameBase typ
    else case typ of
      (TH.AppT (TH.ConT name) _)
        | name == ''Qty -> return $ TH.ConE 'Api.Float
      (TH.ConT name)
        | name == ''Float -> return $ TH.ConE 'Api.Float
        | name == ''Angle -> return $ TH.ConE 'Api.Float
        | name == ''Bool -> return $ TH.ConE 'Api.Boolean
      _ -> error ("Unknown type" <> show typ)

data NotImplemented

typeNameBase :: TH.Type -> TH.Exp
typeNameBase (TH.AppT t _) = typeNameBase t
typeNameBase (TH.ConT name) = TH.LitE (TH.StringL (TH.nameBase name))
typeNameBase _ = TH.ConE ''NotImplemented -- this should break the TH generated code

-- Generates wrapper function type from the original function
-- Returns a list of argument info, return expression wrapper fn and return type
ffiFunctionInfo :: TH.Type -> TH.Q (List (TH.Pat, TH.Type, TH.Exp, Maybe TH.Stmt), TH.Exp -> TH.Exp, TH.Type)
ffiFunctionInfo (TH.ForallT _ _ innerType) = ffiFunctionInfo innerType
ffiFunctionInfo (TH.AppT (TH.AppT TH.ArrowT argTyp) rest) = do
  (args, returnExp, returnType) <- ffiFunctionInfo rest
  arg <- ffiArgInfo argTyp
  return (arg : args, returnExp, returnType)
ffiFunctionInfo returnType = do
  (exp, typ) <- ffiReturnInfo returnType
  return ([], exp, typ)

fixupFunctionType :: TH.Type -> TH.Type
fixupFunctionType (TH.ForallT _ _ innerType) = fixupFunctionType innerType
fixupFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) =
  TH.AppT (TH.AppT TH.ArrowT (fixupType arg)) (fixupFunctionType rest)
fixupFunctionType typ = fixupType typ

fixupType :: TH.Type -> TH.Type
fixupType = fixupCoordinateSystem >> fixupUnits

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
-- We wrap a type in a Ptr if it doesn't implement Storable
ffiArgInfo :: TH.Type -> TH.Q (TH.Pat, TH.Type, TH.Exp, Maybe TH.Stmt)
ffiArgInfo typ = do
  argName <- TH.newName "x"
  isPtr <- isPointer typ
  if isPtr
    then do
      unwrappedName <- TH.newName "y"
      return
        ( TH.VarP argName -- arg name
        , TH.AppT (TH.ConT ''Ptr) (TH.ConT ''()) -- arg type
        , TH.VarE unwrappedName -- unwrapped pointer value
        , Just $
            -- unwrappedName <- derefPtr argName
            TH.BindS
              (TH.VarP unwrappedName)
              (TH.AppE (TH.VarE 'derefPtr) (TH.VarE argName))
        )
    else do
      return
        ( TH.VarP argName -- arg name
        , typ -- original type
        , TH.VarE argName -- original arg
        , Nothing
        )

ffiReturnInfo :: TH.Type -> TH.Q (TH.Exp -> TH.Exp, TH.Type)
ffiReturnInfo (TH.AppT (TH.ConT containerTyp) nestedTyp)
  | containerTyp == ''Maybe = do
      isPtr <- isPointer nestedTyp
      return $
        if isPtr
          then
            ( TH.AppE (TH.VarE 'newMaybePtr)
            , TH.AppT (TH.ConT ''Ptr) (TH.ConT ''())
            )
          else
            ( TH.AppE (TH.VarE 'newMaybeStorablePtr)
            , TH.AppT (TH.ConT ''Ptr) (TH.ConT ''())
            )
ffiReturnInfo typ = do
  isPtr <- isPointer typ
  return $
    if isPtr
      then
        ( TH.AppE (TH.VarE 'newPtr)
        , TH.AppT (TH.ConT ''Ptr) (TH.ConT ''())
        )
      else
        ( TH.AppE (TH.VarE 'return)
        , typ
        )

isPointer :: TH.Type -> TH.Q Bool
isPointer typ = do
  let fixedTyp = typ |> fixupUnits |> fixupCoordinateSystem
  instances <- TH.reifyInstances ''Storable [fixedTyp]
  return $ instances == []

newMaybePtr :: Maybe a -> IO (Ptr ())
newMaybePtr (Just val) = newPtr val
newMaybePtr Nothing = return nullPtr

newMaybeStorablePtr :: (Storable a) => Maybe a -> IO (Ptr a)
newMaybeStorablePtr (Just val) = do
  ptr <- malloc
  _ <- poke ptr val
  return ptr
newMaybeStorablePtr Nothing = return nullPtr

newPtr :: a -> IO (Ptr ())
newPtr val = do
  stablePtr <- newStablePtr val
  return $ castStablePtrToPtr stablePtr

derefPtr :: Ptr () -> IO a
derefPtr ptr =
  deRefStablePtr (castPtrToStablePtr ptr)

camelToSnake :: String -> String
camelToSnake [] = []
camelToSnake (x : xs)
  | isUpper x = '_' : toLower x : camelToSnake xs
  | otherwise = x : camelToSnake xs

uppercaseFirstChar :: String -> String
uppercaseFirstChar [] = []
uppercaseFirstChar (x : xs) = toUpper x : xs

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
  originalFnType <- TH.reifyType fnName
  let fixedFnType = fixupFunctionType originalFnType
  (args, returnExp, returnType) <- ffiFunctionInfo fixedFnType

  let foreignName = ffiFunctionName fnName
      wrapperName = TH.mkName foreignName
      argNames = map (\(name, _, _, _) -> name) args
      argTypes = map (\(_, typ, _, _) -> typ) args
      argExprs = map (\(_, _, expr, _) -> expr) args
      optionalBindStatements = map (\(_, _, _, bindStmt) -> bindStmt) args
      origFunc = TH.SigE (TH.VarE fnName) fixedFnType -- annotate with the fixed up signature
      wrapperBody =
        TH.DoE Nothing $
          foldl
            (\statements optStmt -> maybe statements (\x -> x : statements) optStmt)
            [TH.NoBindS $ returnExp $ foldl TH.AppE origFunc argExprs]
            optionalBindStatements
      wrapperType = foldr (\a b -> TH.AppT (TH.AppT TH.ArrowT a) b) (TH.AppT (TH.ConT ''IO) returnType) argTypes
      wrapperClause = TH.Clause argNames (TH.NormalB wrapperBody) []

  return
    [ TH.ForeignD (TH.ExportF TH.CCall (camelToSnake foreignName) wrapperName wrapperType)
    , TH.SigD wrapperName wrapperType
    , TH.FunD wrapperName [wrapperClause]
    ]
