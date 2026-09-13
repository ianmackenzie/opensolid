module OpenSolid.API.PreOperatorOverload
  ( PreOperatorOverload (..)
  , signature
  , lhsName
  , invoke
  , ffiName
  )
where

import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.ImplicitTolerance (ImplicitTolerance (ImplicitTolerance))
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data PreOperatorOverload where
  PreOperatorOverload ::
    (FFI other, FFI value, FFI result) =>
    (other -> value -> IO result) ->
    PreOperatorOverload
  PreOperatorOverloadM ::
    (FFI other, FFI value, FFI result) =>
    (Tolerance Meters => other -> value -> IO result) ->
    PreOperatorOverload

ffiName :: FFI.ClassName -> BinaryOperator.Id -> PreOperatorOverload -> Text
ffiName className operatorId overload = case overload of
  PreOperatorOverload f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignature f)
  PreOperatorOverloadM f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignatureM f)

signature :: PreOperatorOverload -> (Maybe ImplicitTolerance, FFI.Type, FFI.Type)
signature overload = case overload of
  PreOperatorOverload f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignature f
    (Nothing, lhsType, returnType)
  PreOperatorOverloadM f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignatureM f
    (Just ImplicitTolerance, lhsType, returnType)

lhsName :: Name
lhsName = FFI.name "Lhs"

invoke :: PreOperatorOverload -> FFI.Function
invoke overload inputPtr outputPtr errorPtr = case overload of
  PreOperatorOverload f -> do
    (other, value) <- FFI.load inputPtr 0
    FFI.invoke (f other value) outputPtr errorPtr
  PreOperatorOverloadM f -> do
    (tolerance, other, value) <- FFI.load inputPtr 0
    FFI.invoke (Tolerance.using tolerance (f other value)) outputPtr errorPtr
