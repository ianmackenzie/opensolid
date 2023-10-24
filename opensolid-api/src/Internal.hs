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
import Codegen (Codegen)
import Codegen qualified
import Data.Char qualified as Char
import Debug qualified
import Foreign qualified
import Foreign.Storable (Storable)
import Language.Haskell.TH qualified as TH
import List qualified
import Maybe qualified
import OpenSolid hiding (fromInteger)
import Pointers qualified
import Text qualified
import Prelude (fromInteger)
import Prelude qualified

data Class = Class TH.Name (List TH.Name) (List TH.Name) (List Function)

data Function = Function TH.Name TH.Name (List Text)

cls :: TH.Name -> List TH.Name -> List TH.Name -> List Function -> Class
cls = Class

method :: TH.Name -> List Text -> Function
method = Function 'Api.Method

static :: TH.Name -> List Text -> Function
static = Function 'Api.Static

ffi :: List Class -> Codegen (List TH.Dec)
ffi classes = do
  functionDecls <- Codegen.map List.concat (Codegen.collect ffiClass classes)
  freeFunctionDecl <- freeFunctionFfi "opensolid_free" 'Pointers.freeStorablePtr
  freeStableFunctionDecl <- freeFunctionFfi "opensolid_free_stable" 'Pointers.freeStablePtr
  Codegen.generate (freeFunctionDecl : freeStableFunctionDecl : functionDecls)
 where
  ffiClass (Class _ _ _ functions) =
    Codegen.map List.concat (Codegen.collect ffiFunction functions)

freeFunctionFfi :: Text -> TH.Name -> Codegen TH.Dec
freeFunctionFfi ffiName name = do
  freeFunctionType <- Codegen.reifyType name
  Codegen.generate (TH.ForeignD (TH.ExportF TH.CCall (Text.toChars ffiName) name freeFunctionType))

api :: List Class -> Codegen TH.Exp
api classes = do
  apiCls <- Codegen.collect apiClass classes
  Codegen.generate (TH.AppE (TH.ConE 'Api.Api) (TH.ListE apiCls))

-- human readable name in the API
apiName :: TH.Name -> TH.Exp
apiName name =
  TH.LitE (TH.StringL (Text.toChars (camelToSnake (Codegen.nameBase name))))

apiClass :: Class -> Codegen TH.Exp
apiClass (Class name representationProps errors functions) = do
  apiFns <- Codegen.collect apiFunction functions
  apiExceptions <- Codegen.map List.concat (Codegen.collect apiException errors)
  Codegen.generate $
    TH.ConE 'Api.Class
      `TH.AppE` TH.LitE (TH.StringL (TH.nameBase name))
      `TH.AppE` TH.ListE (List.map apiName representationProps)
      `TH.AppE` TH.ListE apiExceptions
      `TH.AppE` TH.ListE apiFns

apiException :: TH.Name -> Codegen (List TH.Exp)
apiException name = do
  constructors <- apiExceptionConstructors name
  Codegen.generate $
    [ TH.ConE 'Api.ExceptionClass
        `TH.AppE` TH.LitE (TH.StringL (TH.nameBase name))
        `TH.AppE` TH.ListE constructors
    ]

apiExceptionConstructors :: TH.Name -> Codegen (List TH.Exp)
apiExceptionConstructors typeName = do
  info <- Codegen.reify typeName
  case info of
    TH.TyConI (TH.DataD _ _ _ _ cons _) -> Codegen.sequence (List.indexedMap apiExceptionConstructor cons)
    _ -> Prelude.error (Text.toChars "Not a data type")

apiExceptionConstructor :: Int -> TH.Con -> Codegen TH.Exp
apiExceptionConstructor idx (TH.NormalC name []) = do
  Codegen.generate $
    TH.TupE
      [ Just $ TH.LitE (TH.IntegerL (Prelude.fromIntegral (idx + (1 :: Int))))
      , Just $ TH.LitE (TH.StringL (TH.nameBase name))
      , Just $ TH.ConE 'Nothing
      ]
apiExceptionConstructor idx (TH.NormalC name [(_, typ)]) = do
  apiTyp <- apiType typ
  Codegen.generate $
    TH.TupE
      [ Just $ TH.LitE (TH.IntegerL (Prelude.fromIntegral (idx + (1 :: Int))))
      , Just $ TH.LitE (TH.StringL (TH.nameBase name))
      , Just $ TH.ConE 'Just `TH.AppE` apiTyp
      ]
apiExceptionConstructor _ _ =
  Prelude.error (Text.toChars "Unrecognized constructor")

apiFunction :: Function -> Codegen TH.Exp
apiFunction (Function kind fnName argNames) = do
  fnType <- Codegen.reifyType fnName
  (argTypes, returnType) <- apiFunctionType fnType
  let (finalNames, finalTypes) =
        if containsImplicitTolerance fnType
          then -- prepend the tolerance argument and its type
            ("tolerance" : argNames, TH.ConE 'Api.ImplicitTolerance : argTypes)
          else (argNames, argTypes)
  Codegen.generate $
    TH.ConE 'Api.Function
      `TH.AppE` TH.ConE kind
      `TH.AppE` TH.LitE (TH.StringL (Text.toChars (camelToSnake (ffiFunctionName fnName)))) -- ffi name
      `TH.AppE` apiName fnName
      `TH.AppE` TH.ListE
        ( List.map2
            (\a t -> TH.TupE [Just (TH.LitE (TH.StringL (Text.toChars a))), Just t])
            finalNames
            finalTypes
        )
      `TH.AppE` returnType

apiFunctionType :: TH.Type -> Codegen (List TH.Exp, TH.Exp)
apiFunctionType (TH.ForallT _ _ innerType) = apiFunctionType innerType
apiFunctionType (TH.AppT (TH.AppT TH.ArrowT arg) rest) = do
  (args, returnType) <- apiFunctionType rest
  typ <- apiType arg
  Codegen.generate (typ : args, returnType)
apiFunctionType returnType = do
  typ <- apiType returnType
  Codegen.generate ([], typ)

apiType :: TH.Type -> Codegen TH.Exp
apiType (TH.AppT (TH.AppT (TH.TupleT 2) nestedTyp1) nestedTyp2) = do
  typ1 <- apiType nestedTyp1
  typ2 <- apiType nestedTyp2
  Codegen.generate (TH.ConE 'Api.Tuple2 `TH.AppE` typ1 `TH.AppE` typ2)
apiType (TH.AppT (TH.AppT (TH.ConT containerName) errTyp) nestedTyp) | containerName == ''Result = do
  typ <- apiType nestedTyp
  err <- typeNameBase errTyp
  mod <- modNameBase errTyp
  Codegen.generate (TH.ConE 'Api.Result `TH.AppE` mod `TH.AppE` err `TH.AppE` typ)
apiType (TH.AppT (TH.ConT containerName) nestedTyp) | containerName == ''Maybe = do
  typ <- apiType nestedTyp
  Codegen.generate (TH.ConE 'Api.Maybe `TH.AppE` typ)
apiType typ = do
  isPtr <- isPointer typ
  if isPtr
    then Codegen.map (TH.AppE (TH.ConE 'Api.Pointer)) (typeNameBase typ)
    else case typ of
      (TH.AppT (TH.ConT name) _)
        | name == ''Qty -> Codegen.generate (TH.ConE 'Api.Float)
      (TH.ConT name)
        | name == ''Float -> Codegen.generate (TH.ConE 'Api.Float)
        | name == ''Angle -> Codegen.generate (TH.ConE 'Api.Float)
        | name == ''Bool -> Codegen.generate (TH.ConE 'Api.Boolean)
      _ -> Codegen.fail ("Unknown type: " ++ Debug.show typ)

typeNameBase :: TH.Type -> Codegen TH.Exp
typeNameBase (TH.AppT t _) = typeNameBase t
typeNameBase (TH.ConT name) = Codegen.generate (TH.LitE (TH.StringL (TH.nameBase name)))
typeNameBase typ = Codegen.fail ("Unknown type: " ++ Debug.show typ)

modNameBase :: TH.Type -> Codegen TH.Exp
modNameBase (TH.AppT t _) = modNameBase t
modNameBase typ@(TH.ConT name) =
  case TH.nameModule name of
    Just mod -> Codegen.generate (TH.LitE (TH.StringL mod))
    Nothing -> Codegen.fail ("Unknown module for type: " ++ Debug.show typ)
modNameBase typ = Codegen.fail ("Unknown module for type: " ++ Debug.show typ)

-- Generates wrapper function type and clause from the original function
ffiFunctionInfo :: TH.Type -> TH.Exp -> List TH.Pat -> List TH.Stmt -> Codegen (TH.Type, TH.Clause)
ffiFunctionInfo (TH.AppT (TH.AppT TH.ArrowT argTyp) remainingArgs) retExp arguments bindStmts = do
  arg <- ffiArgInfo argTyp
  let (argName, argFfiTyp, argExpr, bindStmt) = arg
      newBindStmts = bindStmt ++ bindStmts
      newArguments = argName : arguments
      newRetExpr = TH.AppE retExp argExpr
  (returnType, returnClause) <- ffiFunctionInfo remainingArgs newRetExpr newArguments newBindStmts
  Codegen.generate
    ( TH.AppT (TH.AppT TH.ArrowT argFfiTyp) returnType
    , returnClause
    )
ffiFunctionInfo returnType retExp argNames bindStmts = do
  (finalRetExp, finalReturnType) <- ffiReturnInfo retExp returnType
  Codegen.generate
    ( TH.AppT (TH.ConT ''IO) finalReturnType
    , TH.Clause
        (List.reverse argNames)
        (TH.NormalB (TH.DoE Nothing $ bindStmts ++ [TH.NoBindS finalRetExp]))
        []
    )

-- Alias for coordinate system `@` type, since that seems to confuse Template Haskell (see below)
type Coords space units = space @ units

-- Fix up function type signatures to play more nicely with Template Haskell
fixupType :: TH.Type -> TH.Type
-- Replace 'units', 'units1', 'units2', `frameUnits` etc. with 'Unitless' in all types
-- since the FFI only deals with unitless values
fixupType (TH.VarT name) | Text.contains "units" (Text.toLower (Codegen.nameBase name)) = TH.ConT ''Unitless
-- Strip 'forall' from function types
-- (shouldn't be needed since these are generally units-related...)
fixupType (TH.ForallT _ _ functionType) = fixupType functionType
-- Template Haskell seems to get confused by type-level use of '@' operator,
-- so replace any instance of `space @ units` with the equivalent `'Coords space units`
fixupType (TH.ConT name) | Codegen.nameBase name == "@" = TH.ConT ''Coords
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
ffiArgInfo :: TH.Type -> Codegen (TH.Pat, TH.Type, TH.Exp, Maybe TH.Stmt)
ffiArgInfo typ = do
  argName <- Codegen.newName "x"
  isPtr <- isPointer typ
  if isPtr
    then do
      unwrappedName <- Codegen.newName "y"
      Codegen.generate
        ( TH.VarP argName -- arg name
        , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''()) -- arg type
        , TH.VarE unwrappedName -- unwrapped pointer value
        , Just $
            -- unwrappedName <- derefStablePtr argName
            TH.BindS
              (TH.VarP unwrappedName)
              (TH.AppE (TH.VarE 'Pointers.derefStablePtr) (TH.VarE argName))
        )
    else do
      Codegen.generate
        ( TH.VarP argName -- arg name
        , typ -- original type
        , TH.VarE argName -- original arg
        , Nothing
        )

ffiReturnInfo :: TH.Exp -> TH.Type -> Codegen (TH.Exp, TH.Type)
ffiReturnInfo expr tuple@(TH.AppT (TH.AppT (TH.TupleT 2) _) _) =
  Codegen.generate $
    ( TH.AppE (TH.VarE 'Pointers.newStorablePtr) expr
    , TH.AppT (TH.ConT ''Foreign.Ptr) tuple
    )
ffiReturnInfo expr result@(TH.AppT (TH.AppT (TH.ConT containerName) _) _)
  | containerName == ''Result = do
      Codegen.generate $
        ( TH.AppE (TH.VarE 'Pointers.newStorablePtr) expr
        , TH.AppT (TH.ConT ''Foreign.Ptr) result
        )
ffiReturnInfo expr (TH.AppT (TH.ConT containerName) _)
  | containerName == ''Maybe = do
      Codegen.generate $
        ( TH.AppE (TH.VarE 'Pointers.newMaybePtr) expr
        , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''())
        )
ffiReturnInfo expr typ = do
  isPtr <- isPointer typ
  Codegen.generate $
    if isPtr
      then
        ( TH.AppE (TH.VarE 'Pointers.newStablePtr) expr
        , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''())
        )
      else
        ( TH.AppE (TH.VarE 'Prelude.return) expr
        , typ
        )

isPointer :: TH.Type -> Codegen Bool
isPointer typ = do
  let fixedTyp = fixupType typ
  instances <- Codegen.reifyInstances ''Storable [fixedTyp]
  Codegen.generate (List.isEmpty instances)

camelToSnake :: Text -> Text
camelToSnake text = Text.fromChars (impl (Text.toChars text))
 where
  impl [] = []
  impl (x : xs)
    | Char.isUpper x = '_' : Char.toLower x : impl xs
    | otherwise = x : impl xs

uppercaseFirstChar :: Text -> Text
uppercaseFirstChar text = Text.fromChars (impl (Text.toChars text))
 where
  impl [] = []
  impl (x : xs) = Char.toUpper x : xs

-- Generate a name for the FII wrapper function
-- ''xCoordinate from the Point2d module becomes `opensolidPoint2dXCoordinate`
ffiFunctionName :: TH.Name -> Text
ffiFunctionName fnName =
  Text.concat
    [ "opensolid"
    , Maybe.withDefault "" (Codegen.nameModule fnName)
    , uppercaseFirstChar (Codegen.nameBase fnName)
    ]

-- Wrap the function with an FFI
ffiFunction :: Function -> Codegen (List TH.Dec)
ffiFunction (Function _ fnName _) = do
  originalFnType <- Codegen.reifyType fnName
  let fixedFnType = fixupType originalFnType
  let origFunc = TH.SigE (TH.VarE fnName) fixedFnType -- annotate with the fixed up signature
  let toleranceName = TH.mkName (Text.toChars "tolerance")
  let needsTolerance = containsImplicitTolerance originalFnType
  let (initialArgs, initialStmts) =
        if needsTolerance
          then
            ( [TH.VarP toleranceName] -- prepend `tolerance` argument
            , [TH.LetS [TH.ImplicitParamBindD (Text.toChars "tolerance") (TH.VarE toleranceName)]] -- `let ?tolerance = tolerance`
            )
          else ([], [])
  (returnType, wrapperClause) <- ffiFunctionInfo fixedFnType origFunc initialArgs initialStmts

  let foreignName = ffiFunctionName fnName
  let wrapperName = TH.mkName (Text.toChars foreignName)
  let wrapperType =
        if needsTolerance
          then TH.AppT (TH.AppT TH.ArrowT (TH.ConT ''Float)) returnType -- prepend tolerance argument type
          else returnType
  Codegen.generate
    [ TH.ForeignD (TH.ExportF TH.CCall (Text.toChars (camelToSnake foreignName)) wrapperName wrapperType)
    , TH.SigD wrapperName wrapperType
    , TH.FunD wrapperName [wrapperClause]
    ]
