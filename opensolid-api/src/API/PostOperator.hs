module API.PostOperator
  ( PostOperator (..)
  , signature
  , rhsName
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

data PostOperator value where
  PostOperator ::
    (FFI value, FFI other, FFI result) =>
    (value -> other -> result) ->
    PostOperator value

ffiName :: FFI.Class -> BinaryOperator.Id -> PostOperator value -> Text
ffiName ffiClass operatorId operator =
  BinaryOperator.ffiName ffiClass operatorId (signature operator)

signature :: PostOperator value -> (FFI.Type, FFI.Type, FFI.Type)
signature (PostOperator f) = BinaryOperator.functionSignature f

rhsName :: Name
rhsName = FFI.name "Rhs"

invoke :: PostOperator value -> Ptr () -> Ptr () -> IO ()
invoke (PostOperator f) inputPtr outputPtr = IO.do
  (value, other) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value other)
