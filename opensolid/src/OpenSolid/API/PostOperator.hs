module OpenSolid.API.PostOperator
  ( PostOperator (..)
  , signature
  , rhsName
  , invoke
  , ffiName
  )
where

import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.API.BinaryOperator qualified as BinaryOperator
import OpenSolid.API.Name (Name)
import OpenSolid.API.Name qualified as Name
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI

data PostOperator value where
  PostOperator ::
    (FFI value, FFI other, FFI result) =>
    (value -> other -> result) ->
    PostOperator value

ffiName :: FFI.Id value -> BinaryOperator.Id -> PostOperator value -> Text
ffiName classId operatorId operator =
  BinaryOperator.ffiName classId operatorId (signature operator)

signature :: PostOperator value -> (FFI.Type, FFI.Type, FFI.Type)
signature (PostOperator f) = BinaryOperator.functionSignature f

rhsName :: Name
rhsName = Name.parse "Rhs"

invoke :: PostOperator value -> Ptr () -> Ptr () -> IO ()
invoke (PostOperator f) inputPtr outputPtr = IO.do
  (value, other) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value other)