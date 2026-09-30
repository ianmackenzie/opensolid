module OpenSolid.API.BinaryOperator
  ( Id (..)
  , ffiName
  , functionSignature
  , functionSignatureT
  )
where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Text qualified as Text

data Id = Add | Sub | Mul | Div | FloorDiv | Mod | Dot | Cross deriving (Eq)

ffiName :: FFI.ClassName -> Id -> (FFI.Type, FFI.Type, FFI.Type) -> Text
ffiName className operatorId (lhsType, rhsType, _) =
  Text.join "_" $
    [ "opensolid"
    , FFI.concatenatedName className
    , functionName operatorId
    , FFI.typeName lhsType
    , FFI.typeName rhsType
    ]

functionName :: Id -> Text
functionName operatorId = case operatorId of
  Add -> "add"
  Sub -> "sub"
  Mul -> "mul"
  Div -> "div"
  FloorDiv -> "floorDiv"
  Mod -> "mod"
  Dot -> "dot"
  Cross -> "cross"

functionSignature ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  (a -> b -> IO c) ->
  (FFI.Type, FFI.Type, FFI.Type)
functionSignature _ = (FFI.typeOf a, FFI.typeOf b, FFI.typeOf c)

functionSignatureT ::
  forall a b c.
  (FFI a, FFI b, FFI c) =>
  (Tolerance Meters => a -> b -> IO c) ->
  (FFI.Type, FFI.Type, FFI.Type)
functionSignatureT _ = (FFI.typeOf a, FFI.typeOf b, FFI.typeOf c)
