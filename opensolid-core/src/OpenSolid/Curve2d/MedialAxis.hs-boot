module OpenSolid.Curve2d.MedialAxis (Segment (Segment)) where

import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    ( "t1" ::: Curve Unitless
    , "t2" ::: Curve Unitless
    , "t12" ::: Curve2d UvCoordinates
    , "curve" ::: Curve2d (space @ units)
    , "radius" ::: Curve units
    ) ->
    Segment (space @ units)
