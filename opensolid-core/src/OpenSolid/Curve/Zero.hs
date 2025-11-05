module OpenSolid.Curve.Zero (Zero (Zero, location, order, sign)) where

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude

-- | Contains details about a single point where a curve is zero.
data Zero = Zero
  { location :: Number
  -- ^ Get the parameter value at which the curve is zero.
  , order :: Int
  -- ^ Check whether the zero is a crossing zero, a tangent zero etc.
  --
  --   * An order 0 zero means the curve crosses zero at the given location,
  --     with a non-zero first derivative.
  --   * An order 1 zero means the first derivative is also zero at the given
  --     location, but the second derivative is not (that is, the curve just
  --     'touches' zero at that point).
  --   * An order 2 zero means the first and second derivatives are zero at the
  --     given location, etc.
  , sign :: Sign
  -- ^ Check whether the curve 'curves up' or 'curves down' at the zero.
  --
  -- A positive sign means that the curve is positive to the right of the zero
  -- (for a crossing zero, that means the curve will be negative to the left,
  -- but for an order 1 tangent zero, that means the curve will also be positive
  -- to the left!). Similarly, a negative sign means that the curve is negative
  -- to the right of the zero.
  }
  deriving (Eq, Show)

instance ApproximateEquality Zero Unitless where
  Zero location1 order1 sign1 ~= Zero location2 order2 sign2 =
    location1 ~= location2 && order1 == order2 && sign1 == sign2

instance FFI Zero where
  representation = FFI.nestedClassRepresentation "Curve" "Zero"
