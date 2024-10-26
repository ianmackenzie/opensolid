module Curve2d.MedialAxis
  ( Error (..)
  , Segment (..)
  , solve
  )
where

import Curve1d (Curve1d)
import {-# SOURCE #-} Curve2d (Curve2d)
import Error qualified
import OpenSolid
import Uv qualified

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
    , t12 :: Curve2d Uv.Coordinates
    } ->
    Segment (space @ units)

solve ::
  forall space units.
  Tolerance units =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Error (List (Segment (space @ units)))
