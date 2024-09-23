module Curve2d.MedialAxis
  ( Error (..)
  , Segment (..)
  , solve
  )
where

import Curve1d (Curve1d)
import {-# SOURCE #-} Curve2d (Curve2d)
import {-# SOURCE #-} Curve2d qualified
import Debug qualified
import Error qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Surface1d.Function qualified
import Surface1d.Function.Zeros qualified
import Surface2d.Function ()
import Tolerance qualified

data Error
  = HigherOrderSolution
  | DegenerateCurve
  deriving (Eq, Show, Error.Message)

data Segment (coordinateSystem :: CoordinateSystem) where
  Segment ::
    { t1 :: Curve1d Unitless
    , t2 :: Curve1d Unitless
    } ->
    Segment (space @ units)

solve ::
  forall space units.
  (Known space, Known units, Tolerance units) =>
  Curve2d (space @ units) ->
  Curve2d (space @ units) ->
  Result Error (List (Segment (space @ units)))
solve curve1 curve2 = Result.do
  let p1 = curve1 . Surface1d.Function.u
  let p2 = curve2 . Surface1d.Function.v
  let v1 = Curve2d.derivative curve1 . Surface1d.Function.u
  let v2 = Curve2d.derivative curve2 . Surface1d.Function.v
  let d = p2 - p1
  let target = v2 .><. (2.0 * (v1 .<>. d) .*. d - d .<>. d .*. v1)
  let targetTolerance = ?tolerance .*. ((?tolerance .*. ?tolerance) .*. ?tolerance)
  case Tolerance.using targetTolerance (Surface1d.Function.zeros target) of
    Failure Surface1d.Function.Zeros.HigherOrderZero -> Failure HigherOrderSolution
    Failure Surface1d.Function.Zeros.ZeroEverywhere -> TODO -- curves are identical arcs?
    Success zeros -> do
      Debug.assert (List.isEmpty (Surface1d.Function.Zeros.crossingLoops zeros))
      Debug.assert (List.isEmpty (Surface1d.Function.Zeros.tangentPoints zeros))
      let allCrossingCurves = List.collect NonEmpty.toList (Surface1d.Function.Zeros.crossingCurves zeros)
      let toSegment (solutionCurve, _) =
            Segment
              { t1 = Surface1d.Function.u . solutionCurve
              , t2 = Surface1d.Function.v . solutionCurve
              }
      Success (List.map toSegment allCrossingCurves)
