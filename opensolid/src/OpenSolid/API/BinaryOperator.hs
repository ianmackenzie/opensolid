module OpenSolid.API.BinaryOperator
  ( BinaryOperator (..)
  , Id (..)
  , signature
  , invoke
  , ffiName
  )
where

import Data.Proxy (Proxy (Proxy))
import Foreign (Ptr)
import IO qualified
import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import Text qualified

data BinaryOperator value where
  Post ::
    (FFI value, FFI other, FFI result) =>
    (value -> other -> result) ->
    BinaryOperator value
  Pre ::
    (FFI other, FFI value, FFI result) =>
    (other -> value -> result) ->
    BinaryOperator value

data Id = Add | Sub | Mul | Div

ffiName :: FFI.Id value -> Id -> BinaryOperator value -> Text
ffiName classId operatorId operator = do
  let (lhsType, rhsType, _) = signature operator
  Text.join "_" $
    [ "opensolid"
    , FFI.className classId
    , functionName operatorId
    , FFI.typeName lhsType
    , FFI.typeName rhsType
    ]

functionName :: Id -> Text
functionName id = case id of
  Add -> "add"
  Sub -> "sub"
  Mul -> "mul"
  Div -> "div"

signature :: BinaryOperator value -> (FFI.Type, FFI.Type, FFI.Type)
signature operator = case operator of
  Post f -> functionSignature f
  Pre f -> functionSignature f

functionSignature ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  (a -> b -> c) ->
  (FFI.Type, FFI.Type, FFI.Type)
functionSignature _ =
  ( FFI.typeOf @a Proxy
  , FFI.typeOf @b Proxy
  , FFI.typeOf @c Proxy
  )

invoke :: BinaryOperator value -> Ptr () -> Ptr () -> IO ()
invoke operator = case operator of
  Post f ->
    \inputPtr outputPtr -> IO.do
      (value, other) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f value other)
  Pre f ->
    \inputPtr outputPtr -> IO.do
      (other, value) <- FFI.load inputPtr 0
      FFI.store outputPtr 0 (f other value)
