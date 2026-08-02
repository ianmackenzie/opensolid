module OpenSolid.API.PostOperatorOverload
  ( PostOperatorOverload (..)
  , signature
  , rhsName
  , invoke
  , ffiName
  )
where

import Foreign (Ptr)
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.ImplicitArgument (ImplicitArgument)
import OpenSolid.API.ImplicitArgument qualified as ImplicitArgument
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data PostOperatorOverload where
  PostOperatorOverload ::
    (FFI value, FFI other, FFI result) =>
    (value -> other -> result) ->
    PostOperatorOverload
  PostOperatorOverloadM ::
    (FFI value, FFI other, FFI result) =>
    (Tolerance Meters => value -> other -> result) ->
    PostOperatorOverload

ffiName :: FFI.ClassName -> BinaryOperator.Id -> PostOperatorOverload -> Text
ffiName className operatorId overload = case overload of
  PostOperatorOverload f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignature f)
  PostOperatorOverloadM f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignatureM f)

signature :: PostOperatorOverload -> (Maybe ImplicitArgument, FFI.Type, FFI.Type)
signature overload = case overload of
  PostOperatorOverload f -> do
    let (_selfType, rhsType, returnType) = BinaryOperator.functionSignature f
    (Nothing, rhsType, returnType)
  PostOperatorOverloadM f -> do
    let (_selfType, rhsType, returnType) = BinaryOperator.functionSignatureM f
    (Just ImplicitArgument.ToleranceMeters, rhsType, returnType)

rhsName :: Name
rhsName = FFI.name "Rhs"

invoke :: PostOperatorOverload -> Ptr () -> Ptr () -> IO ()
invoke overload inputPtr outputPtr = case overload of
  PostOperatorOverload f -> do
    (value, other) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (f value other)
  PostOperatorOverloadM f -> do
    (tolerance, value, other) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (Tolerance.using tolerance (f value other))
