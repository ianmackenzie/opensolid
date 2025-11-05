module OpenSolid.Prelude
  ( module OpenSolid.Arithmetic
  , module OpenSolid.Bootstrap
  , module OpenSolid.Composition
  , module OpenSolid.CoordinateSystem
  , module OpenSolid.Number
  , module OpenSolid.Intersects
  , module OpenSolid.Named
  , module OpenSolid.NonEmpty
  , module OpenSolid.Quantity
  , module OpenSolid.Result
  , module OpenSolid.Sign
  , module OpenSolid.Tolerance
  , module OpenSolid.Units
  )
where

import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import OpenSolid.Composition (Composition (compose, (.)))
import OpenSolid.CoordinateSystem
  ( CoordinateSystem
  , Defines
  , LocalSpace
  , UvCoordinates
  , UvSpace
  , type (@)
  )
import OpenSolid.Intersects (Intersects ((^)))
import OpenSolid.Named ((:::) (Named))
import OpenSolid.NonEmpty (NonEmpty ((:|)), (|:), pattern NonEmpty)
import OpenSolid.Number (Number, fromRational)
import OpenSolid.Quantity (Quantity)
import OpenSolid.Result (Result (Failure, Success))
import OpenSolid.Sign (Sign (Negative, Positive))
import OpenSolid.Tolerance (ApproximateEquality ((~=)), Tolerance, (!=))
import OpenSolid.Units
  ( CubicMeters
  , HasUnits
  , Meters
  , Radians
  , Seconds
  , SquareMeters
  , Unitless
  , type (#*#)
  , type (#/#)
  )
