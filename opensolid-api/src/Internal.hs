{-# OPTIONS_GHC -Wno-orphans #-}

module Internal
  ( Class
  , Function
  , ffi
  , api
  , cls
  , method
  , TaggedError (..)
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
import Text qualified
import Prelude (fromInteger)
import Prelude qualified

data Class = Class TH.Name (List TH.Name) (List Function)

data Function = Function TH.Name TH.Name (List Text)

cls :: TH.Name -> List TH.Name -> List Function -> Class
cls = Class

method :: TH.Name -> List Text -> Function
method = Function 'Api.Method

static :: TH.Name -> List Text -> Function
static = Function 'Api.Static

ffi :: List Class -> Codegen (List TH.Dec)
ffi classes = do
  functionDecls <- Codegen.map List.concat (Codegen.collect ffiClass classes)
  freeFunctionDecl <- freeFunctionFfi 'freePtr
  Codegen.generate (freeFunctionDecl : functionDecls)
 where
  ffiClass (Class _ _ functions) =
    Codegen.map List.concat (Codegen.collect ffiFunction functions)

freePtr :: Foreign.Ptr () -> IO ()
freePtr ptr
  | ptr == Foreign.nullPtr = Prelude.return ()
  | otherwise = Foreign.freeStablePtr (Foreign.castPtrToStablePtr ptr)

freeFunctionFfi :: TH.Name -> Codegen TH.Dec
freeFunctionFfi name = do
  freeFunctionType <- Codegen.reifyType name
  Codegen.generate (TH.ForeignD (TH.ExportF TH.CCall (Text.toChars "opensolid_free") name freeFunctionType))

api :: List Class -> Codegen TH.Exp
api classes = do
  apiCls <- Codegen.collect apiClass classes
  Codegen.generate (TH.AppE (TH.ConE 'Api.Api) (TH.ListE apiCls))

-- human readable name in the API
apiName :: TH.Name -> TH.Exp
apiName name =
  TH.LitE (TH.StringL (Text.toChars (camelToSnake (Codegen.nameBase name))))

apiClass :: Class -> Codegen TH.Exp
apiClass (Class name representationProps functions) = do
  apiFns <- Codegen.collect apiFunction functions
  Codegen.generate $
    TH.ConE 'Api.Class
      `TH.AppE` TH.LitE (TH.StringL (TH.nameBase name))
      `TH.AppE` TH.ListE (List.map apiName representationProps)
      `TH.AppE` TH.ListE apiFns

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
apiType (TH.AppT (TH.AppT (TH.ConT containerTyp) errName) nestedTyp) | containerTyp == ''Result = do
  typ <- apiType nestedTyp
  err <- typeNameBase errName
  Codegen.generate (TH.ConE 'Api.Result `TH.AppE` err `TH.AppE` typ)
apiType (TH.AppT (TH.ConT containerTyp) nestedTyp) | containerTyp == ''Maybe = do
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
            -- unwrappedName <- derefPtr argName
            TH.BindS
              (TH.VarP unwrappedName)
              (TH.AppE (TH.VarE 'derefPtr) (TH.VarE argName))
        )
    else do
      Codegen.generate
        ( TH.VarP argName -- arg name
        , typ -- original type
        , TH.VarE argName -- original arg
        , Nothing
        )

ffiReturnInfo :: TH.Exp -> TH.Type -> Codegen (TH.Exp, TH.Type)
ffiReturnInfo expr (TH.AppT (TH.AppT (TH.ConT containerTyp) errName) nestedTyp)
  | containerTyp == ''Result = do
      Codegen.generate $
        ( TH.AppE (TH.VarE 'newResultPtr) expr
        , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.AppT (TH.AppT (TH.ConT containerTyp) errName) nestedTyp)
        )
ffiReturnInfo expr (TH.AppT (TH.ConT containerTyp) nestedTyp)
  | containerTyp == ''Maybe = do
      isPtr <- isPointer nestedTyp
      Codegen.generate $
        if isPtr
          then
            ( TH.AppE (TH.VarE 'newMaybePtr) expr
            , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''())
            )
          else
            ( TH.AppE (TH.VarE 'newMaybeStorablePtr) expr
            , TH.AppT (TH.ConT ''Foreign.Ptr) (TH.ConT ''())
            )
ffiReturnInfo expr typ = do
  isPtr <- isPointer typ
  Codegen.generate $
    if isPtr
      then
        ( TH.AppE (TH.VarE 'newPtr) expr
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

newMaybePtr :: Maybe a -> IO (Foreign.Ptr ())
newMaybePtr (Just val) = newPtr val
newMaybePtr Nothing = Prelude.return Foreign.nullPtr

newMaybeStorablePtr :: (Storable a) => Maybe a -> IO (Foreign.Ptr a)
newMaybeStorablePtr Nothing = Prelude.return Foreign.nullPtr
newMaybeStorablePtr (Just val) = Prelude.do
  ptr <- Foreign.malloc
  Foreign.poke ptr val
  Prelude.return ptr

newResultPtr :: (Storable (Result a b)) => Result a b -> IO (Foreign.Ptr (Result a b))
newResultPtr result = Prelude.do
  ptr <- Foreign.malloc
  Foreign.poke ptr result
  Prelude.return ptr

newPtr :: a -> IO (Foreign.Ptr ())
newPtr val = Prelude.do
  stablePtr <- Foreign.newStablePtr val
  Prelude.return $ Foreign.castStablePtrToPtr stablePtr

derefPtr :: Foreign.Ptr () -> IO a
derefPtr = Foreign.castPtrToStablePtr >> Foreign.deRefStablePtr

class (ErrorMessage error) => TaggedError error where
  fromTaggedPtr :: Foreign.Word8 -> Foreign.Ptr () -> IO error
  toTaggedPtr :: error -> IO (Foreign.Word8, Foreign.Ptr ())

instance (TaggedError error) => Storable (Result error success) where
  sizeOf _ = Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ()) + (1 :: Int) -- size of a pointer + 1 byte for the tag
  alignment _ = Foreign.alignment (Prelude.undefined :: Foreign.Ptr ())
  peek ptr = Prelude.do
    voidPtr <- Foreign.peek (Foreign.castPtr ptr)
    tag <- Foreign.peekByteOff ptr $ Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ())
    case tag of
      (0 :: Foreign.Word8) -> Ok Prelude.<$> Foreign.deRefStablePtr (Foreign.castPtrToStablePtr voidPtr)
      _ -> Error Prelude.<$> fromTaggedPtr tag voidPtr
  poke ptr (Error err) = Prelude.do
    (tag, errPtr) <- toTaggedPtr err
    Foreign.poke (Foreign.castPtr ptr) errPtr
    Foreign.pokeByteOff ptr (Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ())) tag
  poke ptr (Ok success) = Prelude.do
    -- TODO: is this a good idea to allocate a stable ptr here?
    successPtr <- Foreign.castStablePtrToPtr Prelude.<$> Foreign.newStablePtr success
    Foreign.poke (Foreign.castPtr ptr) successPtr
    Foreign.pokeByteOff ptr (Foreign.sizeOf (Prelude.undefined :: Foreign.Ptr ())) (0 :: Foreign.Word8)

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
