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
import Data.Char qualified as Char
import Foreign qualified
import Foreign.Storable (Storable)
import Language.Haskell.TH qualified as TH
import List qualified
import Maybe qualified
import OpenSolid
import Pointers qualified
import Text qualified
import Prelude qualified

data Class = Class TH.Name (List TH.Name) (List TH.Name) (List Function)

data Function = Function TH.Name TH.Name (List Text)

cls :: TH.Name -> List TH.Name -> List TH.Name -> List Function -> Class
cls = Class

method :: TH.Name -> List Text -> Function
method = Function 'Api.Method

static :: TH.Name -> List Text -> Function
static = Function 'Api.Static

ffi :: List Class -> TH.Q (List TH.Dec)
ffi classes = Prelude.do
  functionDecls <- Prelude.fmap List.concat (Prelude.traverse ffiClass classes)
  freeStableFunctionDecl <- freeFunctionFfi "opensolid_free_stable" 'Pointers.freeStablePtr
  Prelude.return (freeStableFunctionDecl : functionDecls)
 where
  ffiClass (Class _ _ _ functions) =
    Prelude.fmap List.concat (Prelude.traverse ffiFunction functions)

freeFunctionFfi :: Text -> TH.Name -> TH.Q TH.Dec
freeFunctionFfi ffiName name = Prelude.do
  freeFunctionType <- TH.reifyType name
  Prelude.return (TH.ForeignD (TH.ExportF TH.CCall (Text.unpack ffiName) name freeFunctionType))

api :: List Class -> TH.Q TH.Exp
api classes = Prelude.do
  apiCls <- Prelude.traverse apiClass classes
  Prelude.return (TH.AppE (TH.ConE 'Api.Api) (TH.ListE apiCls))

-- human readable name in the API
apiName :: TH.Name -> TH.Exp
apiName name =
  TH.LitE (TH.StringL (camelToSnake (TH.nameBase name)))

apiClass :: Class -> TH.Q TH.Exp
apiClass (Class name representationProps errors functions) = Prelude.do
  apiFns <- Prelude.traverse apiFunction functions
  apiExceptions <- Prelude.fmap List.concat (Prelude.traverse apiException errors)
  Prelude.return $
    TH.ConE 'Api.Class
      `TH.AppE` TH.LitE (TH.StringL (TH.nameBase name))
      `TH.AppE` TH.ListE (List.map apiName representationProps)
      `TH.AppE` TH.ListE apiExceptions
      `TH.AppE` TH.ListE apiFns

apiException :: TH.Name -> TH.Q (List TH.Exp)
apiException name = Prelude.do
  constructors <- apiExceptionConstructors name
  Prelude.return
    [ TH.ConE 'Api.ExceptionClass
        `TH.AppE` TH.LitE (TH.StringL (TH.nameBase name))
        `TH.AppE` TH.ListE constructors
    ]

apiExceptionConstructors :: TH.Name -> TH.Q (List TH.Exp)
apiExceptionConstructors typeName = Prelude.do
  info <- TH.reify typeName
  case info of
    TH.TyConI (TH.DataD _ _ _ _ cons _) ->
      Prelude.sequence (List.mapWithIndex apiExceptionConstructor cons)
    _ -> internalError "Not a data type"

apiExceptionConstructor :: Int -> TH.Con -> TH.Q TH.Exp
apiExceptionConstructor idx (TH.NormalC name []) = Prelude.do
  Prelude.return $
    TH.TupE
      [ Just (TH.LitE (TH.IntegerL (fromIntegral (idx + 1))))
      , Just (TH.LitE (TH.StringL (TH.nameBase name)))
      , Just (TH.ConE 'Nothing)
      ]
apiExceptionConstructor idx (TH.NormalC name [(_, typ)]) = Prelude.do
  apiTyp <- apiType typ
  Prelude.return $
    TH.TupE
      [ Just (TH.LitE (TH.IntegerL (fromIntegral (idx + 1))))
      , Just (TH.LitE (TH.StringL (TH.nameBase name)))
      , Just (TH.ConE 'Just `TH.AppE` apiTyp)
      ]
apiExceptionConstructor _ _ =
  internalError "Unrecognized constructor"

apiFunction :: Function -> TH.Q TH.Exp
apiFunction (Function kind fnName argNames) = Prelude.do
  fnType <- TH.reifyType fnName
  (argTypes, returnType) <- apiFunctionType fnType
  let replaceLast _ [] = []
      replaceLast el [_] = [el]
      replaceLast el (h : t) = h : replaceLast el t
  let (namesWithTolerance, typesWithTolerance) =
        if containsImplicitTolerance fnType
          then -- prepend the tolerance argument and its type
            ("tolerance" : argNames, TH.ConE 'Api.ImplicitTolerance : argTypes)
          else (argNames, argTypes)
  let (finalNames, finalTypes) =
        if kind == 'Api.Method
          then -- replace the type of the last argument with "Self"
          -- TODO: validate that the type of the last argument is the same as Class?
            (namesWithTolerance, replaceLast (TH.ConE 'Api.Self) typesWithTolerance)
          else (namesWithTolerance, typesWithTolerance)
  if List.length finalNames /= List.length finalTypes
    then
      internalError $
        Text.concat
          [ "The length of arguments doesn't match the length of their types for "
          , Maybe.withDefault "" (Maybe.map Text.pack (TH.nameModule fnName))
          , "."
          , Text.pack (TH.nameBase fnName)
          , "(names: "
          , Text.show finalNames
          , ", types: "
          , Text.show finalTypes
          , ")"
          ]
    else
      Prelude.return $
        TH.ConE 'Api.Function
          `TH.AppE` TH.ConE kind
          `TH.AppE` TH.LitE (TH.StringL (camelToSnake (Text.unpack (ffiFunctionName fnName)))) -- ffi name
          `TH.AppE` apiName fnName
          `TH.AppE` TH.ListE
            ( List.map2
                (\a t -> TH.TupE [Just (TH.LitE (TH.StringL (Text.unpack a))), Just t])
                finalNames
                finalTypes
            )
          `TH.AppE` returnType

apiFunctionType :: TH.Type -> TH.Q (List TH.Exp, TH.Exp)
apiFunctionType (TH.ForallT _ _ innerType) = apiFunctionType innerType
apiFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) = Prelude.do
  (args, returnType) <- apiFunctionType rest
  typ <- apiType arg
  Prelude.return (typ : args, returnType)
apiFunctionType returnType = Prelude.do
  typ <- apiType returnType
  Prelude.return ([], typ)

apiType :: TH.Type -> TH.Q TH.Exp
apiType (TH.AppT (TH.AppT (TH.TupleT 2) nestedTyp1) nestedTyp2) = Prelude.do
  typ1 <- apiType nestedTyp1
  typ2 <- apiType nestedTyp2
  Prelude.return (TH.ConE 'Api.Tuple2 `TH.AppE` typ1 `TH.AppE` typ2)
apiType (TH.AppT (TH.AppT (TH.ConT containerName) errTyp) nestedTyp) | containerName == ''Result = Prelude.do
  typ <- apiType nestedTyp
  err <- typeNameBase errTyp
  mod <- modNameBase errTyp
  Prelude.return (TH.ConE 'Api.Result `TH.AppE` mod `TH.AppE` err `TH.AppE` typ)
apiType (TH.AppT (TH.ConT containerName) nestedTyp) | containerName == ''Maybe = Prelude.do
  typ <- apiType nestedTyp
  Prelude.return (TH.ConE 'Api.Maybe `TH.AppE` typ)
apiType typ = Prelude.do
  isPtr <- isPointer typ
  if isPtr
    then Prelude.fmap (TH.AppE (TH.ConE 'Api.Pointer)) (typeNameBase typ)
    else case typ of
      (TH.AppT (TH.ConT name) _)
        | name == ''Qty -> Prelude.return (TH.ConE 'Api.Float)
      (TH.ConT name)
        | name == ''Float -> Prelude.return (TH.ConE 'Api.Float)
        | name == ''Angle -> Prelude.return (TH.ConE 'Api.Float)
        | name == ''Bool -> Prelude.return (TH.ConE 'Api.Boolean)
      _ -> Prelude.fail (Text.unpack ("Unknown type: " + Text.show typ))

typeNameBase :: TH.Type -> TH.Q TH.Exp
typeNameBase (TH.AppT t _) = typeNameBase t
typeNameBase (TH.ConT name) = Prelude.return (TH.LitE (TH.StringL (TH.nameBase name)))
typeNameBase typ = Prelude.fail (Text.unpack ("Unknown type: " + Text.show typ))

modNameBase :: TH.Type -> TH.Q TH.Exp
modNameBase (TH.AppT t _) = modNameBase t
modNameBase typ@(TH.ConT name) =
  case TH.nameModule name of
    Just mod -> Prelude.return (TH.LitE (TH.StringL mod))
    Nothing -> Prelude.fail (Text.unpack ("Unknown module for type: " + Text.show typ))
modNameBase typ = Prelude.fail (Text.unpack ("Unknown module for type: " + Text.show typ))

-- Generates wrapper function type and clause from the original function
ffiFunctionInfo :: TH.Type -> TH.Exp -> List TH.Pat -> List TH.Stmt -> TH.Q (TH.Type, TH.Clause)
ffiFunctionInfo (TH.AppT (TH.AppT TH.ArrowT argTyp) remainingArgs) retExp arguments bindStmts = Prelude.do
  arg <- ffiArgInfo argTyp
  let (argName, argFfiTyp, argExpr, bindStmt) = arg
      newBindStmts = bindStmt + bindStmts
      newArguments = argName : arguments
      newRetExpr = TH.AppE retExp argExpr
  (returnType, returnClause) <- ffiFunctionInfo remainingArgs newRetExpr newArguments newBindStmts
  Prelude.return
    ( TH.AppT (TH.AppT TH.ArrowT argFfiTyp) returnType
    , returnClause
    )
ffiFunctionInfo returnType retExp argNames bindStmts = Prelude.do
  (finalRetExp, finalReturnType) <- ffiReturnInfo retExp returnType
  Prelude.return
    ( TH.AppT (TH.ConT ''IO) finalReturnType
    , TH.Clause
        (List.reverse argNames)
        (TH.NormalB (TH.DoE Nothing (bindStmts + [TH.NoBindS finalRetExp])))
        []
    )

-- Alias for coordinate system `@` type, since that seems to confuse Template Haskell (see below)
type Coords space units = space @ units

-- Fix up function type signatures to play more nicely with Template Haskell
fixupType :: TH.Type -> TH.Type
-- Replace 'units', 'units1', 'units2', `originUnits` etc. with 'Unitless' in all types
-- since the FFI only deals with unitless values
fixupType (TH.VarT name) | Text.contains "units" (Text.toLower (Text.pack (TH.nameBase name))) = TH.ConT ''Unitless
-- Strip 'forall' from function types
-- (shouldn't be needed since these are generally units-related...)
fixupType (TH.ForallT _ _ functionType) = fixupType functionType
-- Template Haskell seems to get confused by type-level use of '@' operator,
-- so replace any instance of `space @ units` with the equivalent `'Coords space units`
fixupType (TH.ConT name) | TH.nameBase name == Text.unpack "@" = TH.ConT ''Coords
-- In types with parameters, recursively fix up each parameter
fixupType (TH.AppT t a) = TH.AppT (fixupType t) (fixupType a)
-- Otherwise, do nothing and return the type as-is
fixupType typ = typ

-- Check if a given function has the implicit tolerance
containsImplicitTolerance :: TH.Type -> Bool
containsImplicitTolerance (TH.ForallT _ ctxt _) = List.any hasConstraint ctxt
 where
  hasConstraint (TH.AppT (TH.ConT className) _) = className == ''Tolerance
  hasConstraint _ = False
containsImplicitTolerance _ = False

-- Modify types for FFI
-- We wrap a type in a Ptr if it doesn't implement Storable
ffiArgInfo :: TH.Type -> TH.Q (TH.Pat, TH.Type, TH.Exp, Maybe TH.Stmt)
ffiArgInfo typ = Prelude.do
  argName <- TH.newName (Text.unpack "x")
  isPtr <- isPointer typ
  if isPtr
    then Prelude.do
      unwrappedName <- TH.newName (Text.unpack "y")
      Prelude.return
        ( TH.VarP argName -- arg name
        , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''()) -- arg type
        , TH.VarE unwrappedName -- unwrapped pointer value
        , Just $
            -- unwrappedName <- fromVoidPtr argName
            TH.BindS
              (TH.VarP unwrappedName)
              (TH.AppE (TH.VarE 'Pointers.fromVoidPtr) (TH.VarE argName))
        )
    else
      Prelude.return
        ( TH.VarP argName -- arg name
        , typ -- original type
        , TH.VarE argName -- original arg
        , Nothing
        )

ffiReturnInfo :: TH.Exp -> TH.Type -> TH.Q (TH.Exp, TH.Type)
ffiReturnInfo expr typ = Prelude.do
  isPtr <- isPointer typ
  Prelude.return $
    if isPtr
      then
        ( TH.AppE (TH.VarE 'Pointers.toVoidPtr) expr
        , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''())
        )
      else
        ( TH.AppE (TH.VarE 'Prelude.return) expr
        , typ
        )

isPointer :: TH.Type -> TH.Q Bool
isPointer typ = Prelude.do
  let fixedTyp = fixupType typ
  instances <- TH.reifyInstances ''Storable [fixedTyp]
  Prelude.return (List.isEmpty instances)

camelToSnake :: List Char -> List Char
camelToSnake [] = []
camelToSnake (x : xs)
  | Char.isUpper x = '_' : Char.toLower x : camelToSnake xs
  | otherwise = x : camelToSnake xs

uppercaseFirstChar :: List Char -> List Char
uppercaseFirstChar [] = []
uppercaseFirstChar (x : xs) = Char.toUpper x : xs

-- Generate a name for the FII wrapper function
-- ''xCoordinate from the Point2d module becomes `opensolidPoint2dXCoordinate`
ffiFunctionName :: TH.Name -> Text
ffiFunctionName fnName =
  Text.concat
    [ "opensolid"
    , Maybe.withDefault "" (Maybe.map Text.pack (TH.nameModule fnName))
    , Text.pack (uppercaseFirstChar (TH.nameBase fnName))
    ]

-- Wrap the function with an FFI
ffiFunction :: Function -> TH.Q (List TH.Dec)
ffiFunction (Function _ fnName _) = Prelude.do
  originalFnType <- TH.reifyType fnName
  let fixedFnType = fixupType originalFnType
  let origFunc = TH.SigE (TH.VarE fnName) fixedFnType -- annotate with the fixed up signature
  let toleranceName = TH.mkName (Text.unpack "tolerance")
  let needsTolerance = containsImplicitTolerance originalFnType
  let (initialArgs, initialStmts) =
        if needsTolerance
          then
            ( [TH.VarP toleranceName] -- prepend `tolerance` argument
            , [TH.LetS [TH.ImplicitParamBindD (Text.unpack "tolerance") (TH.VarE toleranceName)]] -- `let ?tolerance = tolerance`
            )
          else ([], [])
  (returnType, wrapperClause) <- ffiFunctionInfo fixedFnType origFunc initialArgs initialStmts

  let foreignName = Text.unpack (ffiFunctionName fnName)
  let wrapperName = TH.mkName foreignName
  let wrapperType =
        if needsTolerance
          then TH.AppT (TH.AppT TH.ArrowT (TH.ConT ''Float)) returnType -- prepend tolerance argument type
          else returnType
  Prelude.return
    [ TH.ForeignD (TH.ExportF TH.CCall (camelToSnake foreignName) wrapperName wrapperType)
    , TH.SigD wrapperName wrapperType
    , TH.FunD wrapperName [wrapperClause]
    ]
