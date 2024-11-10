module OpenSolid.API (classes, exceptions) where

import Data.Proxy (Proxy (Proxy))
import OpenSolid hiding (Type)
import OpenSolid.FFI qualified as FFI
import OpenSolid.FFI.Exception (Exception)
import Point2d (Point2d)

data Type where
  Type :: FFI.Representation a -> Type

classes :: List Type
classes =
  [ Type (FFI.representation (Proxy @(Point2d FFI.FloatCoordinates)))
  , Type (FFI.representation (Proxy @(Point2d FFI.LengthCoordinates)))
  ]

exceptions :: List Exception
exceptions = []
