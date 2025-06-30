module OpenSolid.API.PreOperatorOverload
  ( PreOperatorOverload (..)
  , signature
  , lhsName
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
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

data PreOperatorOverload where
  PreOperatorOverload ::
    (FFI other, FFI value, FFI result) =>
    (other -> value -> result) ->
    PreOperatorOverload
  PreOperatorOverloadU ::
    (FFI other, FFI value, FFI result) =>
    (Tolerance Unitless => other -> value -> result) ->
    PreOperatorOverload
  PreOperatorOverloadR ::
    (FFI other, FFI value, FFI result) =>
    (Tolerance Radians => other -> value -> result) ->
    PreOperatorOverload
  PreOperatorOverloadM ::
    (FFI other, FFI value, FFI result) =>
    (Tolerance Meters => other -> value -> result) ->
    PreOperatorOverload
  PreOperatorOverloadS ::
    (FFI other, FFI value, FFI result) =>
    (Tolerance SquareMeters => other -> value -> result) ->
    PreOperatorOverload

ffiName :: FFI.ClassName -> BinaryOperator.Id -> PreOperatorOverload -> Text
ffiName className operatorId overload = case overload of
  PreOperatorOverload f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignature f)
  PreOperatorOverloadU f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignatureU f)
  PreOperatorOverloadR f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignatureR f)
  PreOperatorOverloadM f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignatureM f)
  PreOperatorOverloadS f ->
    BinaryOperator.ffiName className operatorId (BinaryOperator.functionSignatureS f)

signature :: PreOperatorOverload -> (Maybe ImplicitArgument, FFI.Type, FFI.Type)
signature overload = case overload of
  PreOperatorOverload f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignature f
    (Nothing, lhsType, returnType)
  PreOperatorOverloadU f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignatureU f
    (Just ImplicitArgument.ToleranceUnitless, lhsType, returnType)
  PreOperatorOverloadR f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignatureR f
    (Just ImplicitArgument.ToleranceRadians, lhsType, returnType)
  PreOperatorOverloadM f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignatureM f
    (Just ImplicitArgument.ToleranceMeters, lhsType, returnType)
  PreOperatorOverloadS f -> do
    let (lhsType, _selfType, returnType) = BinaryOperator.functionSignatureS f
    (Just ImplicitArgument.ToleranceSquareMeters, lhsType, returnType)

lhsName :: Name
lhsName = FFI.name "Lhs"

invoke :: PreOperatorOverload -> Ptr () -> Ptr () -> IO ()
invoke overload inputPtr outputPtr = case overload of
  PreOperatorOverload f -> IO.do
    (other, value) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (f other value)
  PreOperatorOverloadU f -> IO.do
    (tolerance, other, value) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (Tolerance.using tolerance (f other value))
  PreOperatorOverloadR f -> IO.do
    (tolerance, other, value) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (Tolerance.using tolerance (f other value))
  PreOperatorOverloadM f -> IO.do
    (tolerance, other, value) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (Tolerance.using tolerance (f other value))
  PreOperatorOverloadS f -> IO.do
    (tolerance, other, value) <- FFI.load inputPtr 0
    FFI.store outputPtr 0 (Tolerance.using tolerance (f other value))
