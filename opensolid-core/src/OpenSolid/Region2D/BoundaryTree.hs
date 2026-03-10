module OpenSolid.Region2D.BoundaryTree
  ( BoundaryTree
  , build
  , pointSweptAngle
  , boundsSweptAngle
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Fuzzy (Fuzzy (Resolved, Unresolved))
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Vector2D qualified as Vector2D

data BoundaryTree units space = BoundaryTree
  { bounds :: Bounds2D units space
  , startPoint :: Point2D units space
  , endPoint :: Point2D units space
  , left :: ~(BoundaryTree units space)
  , right :: ~(BoundaryTree units space)
  }

build :: NonEmpty (Curve2D units space) -> BoundaryTree units space
build loop = assembleLoop (NonEmpty.map buildCurve loop)

assembleLoop :: NonEmpty (BoundaryTree units space) -> BoundaryTree units space
assembleLoop subtrees =
  case reduceLoop subtrees of
    NonEmpty.One tree -> tree
    reduced -> assembleLoop reduced

reduceLoop :: NonEmpty (BoundaryTree units space) -> NonEmpty (BoundaryTree units space)
reduceLoop (first :| []) = NonEmpty.one first
reduceLoop (first :| second : []) = NonEmpty.one (join first second)
reduceLoop (first :| second : third : rest) =
  NonEmpty.push (join first second) (reduceLoop (third :| rest))

join :: BoundaryTree units space -> BoundaryTree units space -> BoundaryTree units space
join left right =
  BoundaryTree
    { bounds = Bounds2D.aggregate2 left.bounds right.bounds
    , startPoint = left.startPoint
    , endPoint = right.endPoint
    , left
    , right
    }

buildCurve :: Curve2D units space -> BoundaryTree units space
buildCurve curve =
  buildCurveImpl curve Interval.unit (Curve2D.startPoint curve) (Curve2D.endPoint curve)

buildCurveImpl ::
  Curve2D units space ->
  Interval Unitless ->
  Point2D units space ->
  Point2D units space ->
  BoundaryTree units space
buildCurveImpl curve tBounds startPoint endPoint = do
  let Interval tLow tHigh = tBounds
  let tMid = Number.midpoint tLow tHigh
  let midpoint = Curve2D.evaluate curve tMid
  BoundaryTree
    { bounds = Curve2D.evaluateBounds curve tBounds
    , startPoint
    , endPoint
    , left = buildCurveImpl curve (Interval tLow tMid) startPoint midpoint
    , right = buildCurveImpl curve (Interval tMid tHigh) midpoint endPoint
    }

distinctSweptAngle :: Point2D units space -> BoundaryTree units space -> Angle
distinctSweptAngle point tree =
  Vector2D.angleFrom (tree.startPoint - point) (tree.endPoint - point)

pointSweptAngle :: Point2D units space -> BoundaryTree units space -> Angle
pointSweptAngle point tree
  | Bounds2D.isDistinctFrom point tree.bounds = distinctSweptAngle point tree
  | otherwise = pointSweptAngle point tree.left + pointSweptAngle point tree.right

boundsSweptAngle :: Bounds2D units space -> BoundaryTree units space -> Fuzzy Angle
boundsSweptAngle bounds tree =
  boundsSweptAngleImpl bounds (Bounds2D.centerPoint bounds) (Bounds2D.diameter bounds) tree

boundsSweptAngleImpl ::
  Bounds2D units space ->
  Point2D units space ->
  Quantity units ->
  BoundaryTree units space ->
  Fuzzy Angle
boundsSweptAngleImpl bounds centerPoint diameter tree
  | Bounds2D.areDistinct bounds tree.bounds = Resolved (distinctSweptAngle centerPoint tree)
  | Bounds2D.diameter tree.bounds < diameter = Unresolved
  | otherwise = do
      resolvedLeftSweptAngle <- boundsSweptAngleImpl bounds centerPoint diameter tree.left
      resolvedRightSweptAngle <- boundsSweptAngleImpl bounds centerPoint diameter tree.right
      Resolved (resolvedLeftSweptAngle + resolvedRightSweptAngle)
