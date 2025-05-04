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

data PostOperator where
  PostOperator ::
    (FFI value, FFI other, FFI result) =>
    (value -> other -> result) ->
    PostOperator

ffiName :: FFI.Class -> BinaryOperator.Id -> PostOperator -> Text
ffiName ffiClass operatorId (PostOperator f) =
  BinaryOperator.ffiName ffiClass operatorId (BinaryOperator.functionSignature f)

signature :: PostOperator -> (FFI.Type, FFI.Type)
signature (PostOperator f) = do
  let (_selfType, rhsType, returnType) = BinaryOperator.functionSignature f
  (rhsType, returnType)

rhsName :: Name
rhsName = FFI.name "Rhs"

invoke :: PostOperator -> Ptr () -> Ptr () -> IO ()
invoke (PostOperator f) inputPtr outputPtr = IO.do
  (value, other) <- FFI.load inputPtr 0
  FFI.store outputPtr 0 (f value other)
