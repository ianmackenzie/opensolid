module OpenSolid.Curve2d.MedialAxis
  ( Error (..)
  , Segment (..)
  , solve
  )
where

import Error qualified
import OpenSolid.Prelude
import OpenSolid.Curve1d (Curve1d)
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import SurfaceParameter (UvCoordinates)

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

solve ::
  forall space units.
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Error (List (Segment (space @ units)))
