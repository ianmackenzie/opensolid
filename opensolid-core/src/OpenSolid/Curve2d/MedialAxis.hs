module OpenSolid.Curve2d.MedialAxis
  ( Error (..)
  , Segment (..)
  )
where

import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Error qualified as Error
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvCoordinates)

data Error
  = HigherOrderSolution
  | DegenerateCurve
  deriving (Eq, Show, Error.Message)

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    { t1 :: ~(Curve Unitless)
    , t2 :: ~(Curve Unitless)
    , t12 :: ~(Curve2d UvCoordinates)
    , curve :: ~(Curve2d (space @ units))
    , radius :: ~(Curve units)
    } ->
    Segment (space @ units)
