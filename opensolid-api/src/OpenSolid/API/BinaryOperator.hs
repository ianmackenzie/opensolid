module OpenSolid.API.BinaryOperator
  ( Id (..)
  , ffiName
  , functionSignature
  )
where

import Data.Proxy (Proxy (Proxy))
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
functionName id = case id of
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
  (a -> b -> c) ->
  (FFI.Type, FFI.Type, FFI.Type)
functionSignature _ =
  ( FFI.typeOf @a Proxy
  , FFI.typeOf @b Proxy
  , FFI.typeOf @c Proxy
  )
