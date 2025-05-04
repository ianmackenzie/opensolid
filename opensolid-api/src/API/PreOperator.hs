module API.PreOperator
  ( PreOperator (..)
  , signature
  , lhsName
  , invoke
  , ffiName
  )
where

import API.BinaryOperator qualified as BinaryOperator
import Foreign (Ptr)
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI
import OpenSolid.IO qualified as IO
import OpenSolid.Prelude

data PreOperator value where
  PreOperator ::
    (FFI other, FFI value, FFI result) =>
    (other -> value -> result) ->
    PreOperator value

ffiName :: FFI.Class -> BinaryOperator.Id -> PreOperator value -> Text
ffiName ffiClass operatorId (PreOperator f) =
  BinaryOperator.ffiName ffiClass operatorId (BinaryOperator.functionSignature f)

signature :: PreOperator value -> (FFI.Type, FFI.Type)
signature (PreOperator f) = do
  let (lhsType, _selfType, returnType) = BinaryOperator.functionSignature f
  (lhsType, returnType)

lhsName :: Name
lhsName = FFI.name "Lhs"

invoke :: PreOperator value -> Ptr () -> Ptr () -> IO ()
invoke (PreOperator f) inputPtr outputPtr = IO.do
  (other, value) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f other value)
