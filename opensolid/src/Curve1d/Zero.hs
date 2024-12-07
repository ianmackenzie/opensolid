module Curve1d.Zero (Zero (Zero, location, order, sign)) where

import OpenSolid
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI

data Zero = Zero
  { location :: Float
  , order :: Int
  , sign :: Sign
  }
  deriving (Eq, Show)

instance ApproximateEquality Zero Zero Unitless where
  Zero location1 order1 sign1 ~= Zero location2 order2 sign2 =
    location1 ~= location2 && order1 == order2 && sign1 == sign2

instance FFI Zero where
  representation = FFI.classRepresentation "CurveZero"
