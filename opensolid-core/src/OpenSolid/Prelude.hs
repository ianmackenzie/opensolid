-- Needed for Colour HasField instances
-- (should hopefully be safe having those instances here,
-- since pretty much *any* modules that use OpenSolid
-- will indirectly import this module, even if e.g.
-- they don't import OpenSolid.Color)
{-# OPTIONS_GHC -Wno-orphans #-}

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
  , module Prelude
  )
where

import OpenSolid.Arithmetic
import OpenSolid.Bootstrap
import OpenSolid.Composition (Composition ((.)))
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
  , type (*#)
  , type (/#)
  )
import Prelude qualified
