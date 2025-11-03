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
  , module OpenSolid.Float
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

import Data.Colour (Colour)
import Data.Colour.SRGB qualified
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
import OpenSolid.Float (Float, fromRational)
import OpenSolid.Intersects (Intersects ((^)))
import OpenSolid.Named ((:::) (Named))
import OpenSolid.NonEmpty (NonEmpty ((:|)), (|:), pattern NonEmpty)
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
  , (:*:)
  , (:/:)
  )
import Prelude qualified

instance HasField "rgbFloatComponents" (Colour Float) (Float, Float, Float) where
  getField colour = let Data.Colour.SRGB.RGB r g b = Data.Colour.SRGB.toSRGB colour in (r, g, b)

instance HasField "rgbIntComponents" (Colour Float) (Int, Int, Int) where
  getField colour = do
    let (r, g, b) = colour.rgbFloatComponents
    let toInt component = Prelude.round (component * 255.0)
    (toInt r, toInt g, toInt b)
