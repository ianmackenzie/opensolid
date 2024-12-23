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
import IO qualified
import OpenSolid
import OpenSolid.FFI (FFI, Name)
import OpenSolid.FFI qualified as FFI

data PreOperator value where
  PreOperator ::
    (FFI other, FFI value, FFI result) =>
    (other -> value -> result) ->
    PreOperator value

ffiName :: FFI.Id value -> BinaryOperator.Id -> PreOperator value -> Text
ffiName classId operatorId operator =
  BinaryOperator.ffiName classId operatorId (signature operator)

signature :: PreOperator value -> (FFI.Type, FFI.Type, FFI.Type)
signature (PreOperator f) = BinaryOperator.functionSignature f

lhsName :: Name
lhsName = FFI.name "Lhs"

invoke :: PreOperator value -> Ptr () -> Ptr () -> IO ()
invoke (PreOperator f) inputPtr outputPtr = IO.do
  (other, value) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f other value)
