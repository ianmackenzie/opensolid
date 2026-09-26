module Tests.Matching (Matching, (~~)) where

import OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint qualified as Curve.IntersectionPoint
import OpenSolid.Curve.Intersections qualified as Curve.Intersections
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Root qualified as Curve1D.Root
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Curve3D.IntersectionPointWithSurface qualified as Curve3D.IntersectionPointWithSurface
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvPoint (UvPoint)

class Matching a where
  (~~) :: a -> a -> Bool

infix 4 ~~

instance (Matching a, Matching b) => Matching (a, b) where
  (a1, b1) ~~ (a2, b2) = a1 ~~ a2 && b1 ~~ b2

instance Matching a => Matching (List a) where
  [] ~~ [] = True
  NonEmpty _ ~~ [] = False
  [] ~~ NonEmpty _ = False
  x : xs ~~ y : ys = x ~~ y && xs ~~ ys

instance Matching a => Matching (NonEmpty a) where
  x :| xs ~~ y :| ys = x ~~ y && xs ~~ ys

instance Matching a => Matching (Maybe a) where
  Just first ~~ Just second = first ~~ second
  Nothing ~~ Nothing = True
  Just _ ~~ Nothing = False
  Nothing ~~ Just _ = False

instance Matching Number where
  (~~) = unitless (~=)

instance Matching Length where
  (~~) = spatial (~=)

instance Matching (Point3D space) where
  (~~) = spatial (~=)

instance Matching Curve1D.Root where
  root1 ~~ root2 =
    Curve1D.Root.location root1 ~~ Curve1D.Root.location root2
      && Curve1D.Root.order root1 == Curve1D.Root.order root2
      && Curve1D.Root.sign root1 == Curve1D.Root.sign root2

instance Matching Curve.IntersectionPoint where
  first ~~ second = do
    let firstContinuity = Curve.IntersectionPoint.continuity first
    let secondContinuity = Curve.IntersectionPoint.continuity second
    let firstParameterValues = Curve.IntersectionPoint.parameterValues first
    let secondParameterValues = Curve.IntersectionPoint.parameterValues second
    firstContinuity == secondContinuity && firstParameterValues ~~ secondParameterValues

instance Matching Curve3D.IntersectionPointWithSurface where
  first ~~ second =
    first.kind == second.kind
      && first.t ~~ second.t
      && first.uv ~~ second.uv

instance Matching UvPoint where
  (~~) = unitless (~=)

instance Matching UvCurve where
  (~~) = unitless matchingCurves

matchingCurves ::
  (CurveExists dimension units space, Tolerance units) =>
  Curve dimension units space ->
  Curve dimension units space ->
  Bool
matchingCurves curve1 curve2 =
  case Curve.intersections curve1 curve2 of
    Nothing -> False
    Just Curve.Intersections.IntersectionPoints{} -> False
    Just (Curve.Intersections.OverlappingSegments sign segments intersectionPoints) ->
      sign == Positive
        && segments == NonEmpty.one (Interval.unit, Interval.unit)
        && List.isEmpty intersectionPoints
