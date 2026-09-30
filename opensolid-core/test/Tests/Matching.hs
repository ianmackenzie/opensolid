module Tests.Matching (Matching, (~~)) where

import OpenSolid.Bag (Bag)
import OpenSolid.Bag qualified as Bag
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve (Curve, CurveExists)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint qualified as Curve.IntersectionPoint
import OpenSolid.Curve.Intersections qualified as Curve.Intersections
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Root qualified as Curve1D.Root
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Curve3D.IntersectionPointWithSurface qualified as Curve3D.IntersectionPointWithSurface
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Primitives (Bounds3D (Bounds3D))
import OpenSolid.Set (Set)
import OpenSolid.Set qualified as Set
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set3D (Set3D)
import OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.Surface3D.Pole qualified as Surface3D.Pole
import OpenSolid.SurfacePoint3D (SurfacePoint3D)
import OpenSolid.SurfacePoint3D qualified as SurfacePoint3D
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvCurve (UvCurve)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvSet (UvSet)

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

instance Matching (Point2D Meters) where
  (~~) = spatial (~=)

instance Matching (Point3D space) where
  (~~) = spatial (~=)

instance Matching (Quantity units) => Matching (Interval units) where
  Interval low1 high1 ~~ Interval low2 high2 = low1 ~~ low2 && high1 ~~ high2

instance Matching (Quantity units) => Matching (Bounds2D units) where
  Bounds2D x1 y1 ~~ Bounds2D x2 y2 = x1 ~~ x2 && y1 ~~ y2

instance Matching (Bounds3D space) where
  Bounds3D x1 y1 z1 ~~ Bounds3D x2 y2 z2 = x1 ~~ x2 && y1 ~~ y2 && z1 ~~ z2

instance
  (Matching a, Intersects a (Bounds2D Meters) Meters) =>
  Matching (Set2D Meters a)
  where
  (~~) = spatial matchingSets

instance
  (Matching a, Intersects a UvBounds Unitless) =>
  Matching (UvSet a)
  where
  (~~) = unitless matchingSets

instance
  (Matching a, Intersects a (Bounds3D space) Meters) =>
  Matching (Set3D space a)
  where
  (~~) = spatial matchingSets

matchingSets :: (Tolerance units, Matching a, Intersects a b units) => Set b a -> Set b a -> Bool
matchingSets set1 set2 = do
  let hasMatchIn set item = Set.any (item ^) (item ~~) set
  Set.allItems (hasMatchIn set2) set1 && Set.allItems (hasMatchIn set1) set2

instance Matching (Set b a) => Matching (Bag b a) where
  Bag.Empty ~~ Bag.Empty = True
  Bag.Empty ~~ Bag.Full _ = False
  Bag.Full _ ~~ Bag.Empty = False
  Bag.Full set1 ~~ Bag.Full set2 = set1 ~~ set2

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

instance Matching (Surface3D.Pole space) where
  first ~~ second =
    Surface3D.Pole.uvCurve first ~~ Surface3D.Pole.uvCurve second
      && Surface3D.Pole.point first ~~ Surface3D.Pole.point second

instance Matching (SurfacePoint3D space) where
  SurfacePoint3D.Pole {} ~~ SurfacePoint3D.Point {} = False
  SurfacePoint3D.Point {} ~~ SurfacePoint3D.Pole {} = False
  SurfacePoint3D.Pole pole1 ~~ SurfacePoint3D.Pole pole2 = pole1 ~~ pole2
  SurfacePoint3D.Point uvPoint1 point1 ~~ SurfacePoint3D.Point uvPoint2 point2 =
    uvPoint1 ~~ uvPoint2 && point1 ~~ point2

instance Matching (Curve3D.IntersectionPointWithSurface space) where
  first ~~ second = do
    let firstKind = Curve3D.IntersectionPointWithSurface.kind first
    let secondKind = Curve3D.IntersectionPointWithSurface.kind second
    let firstParameterValue = Curve3D.IntersectionPointWithSurface.parameterValue first
    let secondParameterValue = Curve3D.IntersectionPointWithSurface.parameterValue second
    let firstSurfacePoint = Curve3D.IntersectionPointWithSurface.surfacePoint first
    let secondSurfacePoint = Curve3D.IntersectionPointWithSurface.surfacePoint second
    firstKind == secondKind
      && firstParameterValue ~~ secondParameterValue
      && firstSurfacePoint ~~ secondSurfacePoint

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
    Just (Curve.Intersections.IntersectionPoints {}) -> False
    Just (Curve.Intersections.OverlappingSegments sign segments intersectionPoints) ->
      sign == Positive
        && segments == NonEmpty.one (Interval.unit, Interval.unit)
        && List.isEmpty intersectionPoints
