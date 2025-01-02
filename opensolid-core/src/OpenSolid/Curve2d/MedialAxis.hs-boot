module OpenSolid.Curve2d.MedialAxis
  ( Error (..)
  , Segment (..)
  )
where

import OpenSolid.Curve1d (Curve1d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import OpenSolid.Error qualified as Error
import OpenSolid.Prelude
import OpenSolid.SurfaceParameter (UvCoordinates)

data Error
  = HigherOrderSolution
  | DegenerateCurve

instance Eq Error

instance Show Error

instance Error.Message Error

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    { t1 :: Curve1d Unitless
    , t2 :: Curve1d Unitless
    , t12 :: Curve2d UvCoordinates
    } ->
    Segment (space @ units)
