module OpenSolid.Curve2d.MedialAxis (Segment (Segment)) where

import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvCoordinates)

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    ( "t1" ::: Curve Unitless
    , "t2" ::: Curve Unitless
    , "t12" ::: Curve2d UvCoordinates
    , "curve" ::: Curve2d (space @ units)
    , "radius" ::: Curve units
    ) ->
    Segment (space @ units)

instance HasField "t1" (Segment (space @ units)) (Curve Unitless) where
  getField (Segment fields) = fields.t1

instance HasField "t2" (Segment (space @ units)) (Curve Unitless) where
  getField (Segment fields) = fields.t2

instance HasField "t12" (Segment (space @ units)) (Curve2d UvCoordinates) where
  getField (Segment fields) = fields.t12

instance HasField "curve" (Segment (space @ units)) (Curve2d (space @ units)) where
  getField (Segment fields) = fields.curve

instance HasField "radius" (Segment (space @ units)) (Curve units) where
  getField (Segment fields) = fields.radius
