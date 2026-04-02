module OpenSolid.Region2D.BoundaryTree
  ( BoundaryTree
  , build
  , bounds
  , pointSweptAngle
  , boundsSweptAngle
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2D qualified as Vector2D

data BoundaryTree units = BoundaryTree
  { bounds :: Bounds2D units
  , startPoint :: Point2D units
  , endPoint :: Point2D units
  , left :: ~(BoundaryTree units)
  , right :: ~(BoundaryTree units)
  }

instance Units.Coercion (BoundaryTree units1) (BoundaryTree units2) where
  coerce tree =
    BoundaryTree
      { bounds = Units.coerce tree.bounds
      , startPoint = Units.coerce tree.startPoint
      , endPoint = Units.coerce tree.endPoint
      , left = Units.coerce tree.left
      , right = Units.coerce tree.right
      }

build :: NonEmpty (Curve2D units) -> BoundaryTree units
build loop = assembleLoop (NonEmpty.map buildCurve loop)

assembleLoop :: NonEmpty (BoundaryTree units) -> BoundaryTree units
assembleLoop subtrees =
  case reduceLoop subtrees of
    NonEmpty.One tree -> tree
    reduced -> assembleLoop reduced

reduceLoop :: NonEmpty (BoundaryTree units) -> NonEmpty (BoundaryTree units)
reduceLoop (first :| []) = NonEmpty.one first
reduceLoop (first :| second : []) = NonEmpty.one (join first second)
reduceLoop (first :| second : NonEmpty rest) = NonEmpty.push (join first second) (reduceLoop rest)

join :: BoundaryTree units -> BoundaryTree units -> BoundaryTree units
join left right =
  BoundaryTree
    { bounds = Bounds2D.aggregate2 left.bounds right.bounds
    , startPoint = left.startPoint
    , endPoint = right.endPoint
    , left
    , right
    }

buildCurve :: Curve2D units -> BoundaryTree units
buildCurve curve =
  buildCurveImpl curve Interval.unit (Curve2D.startPoint curve) (Curve2D.endPoint curve)

buildCurveImpl ::
  Curve2D units ->
  Interval Unitless ->
  Point2D units ->
  Point2D units ->
  BoundaryTree units
buildCurveImpl curve tBounds startPoint endPoint = do
  let Interval tLow tHigh = tBounds
  let tMid = Number.midpoint tLow tHigh
  let midpoint = Curve2D.point curve tMid
  BoundaryTree
    { bounds = Curve2D.bounds curve tBounds
    , startPoint
    , endPoint
    , left = buildCurveImpl curve (Interval tLow tMid) startPoint midpoint
    , right = buildCurveImpl curve (Interval tMid tHigh) midpoint endPoint
    }

bounds :: BoundaryTree units -> Bounds2D units
bounds = (.bounds)

distinctSweptAngle :: Point2D units -> BoundaryTree units -> Angle
distinctSweptAngle point tree =
  Vector2D.angleFrom (tree.startPoint - point) (tree.endPoint - point)

pointSweptAngle :: Point2D units -> BoundaryTree units -> Angle
pointSweptAngle point tree
  | Bounds2D.isDistinctFrom point tree.bounds = distinctSweptAngle point tree
  | otherwise = pointSweptAngle point tree.left + pointSweptAngle point tree.right

boundsSweptAngle :: Bounds2D units -> BoundaryTree units -> Fuzzy Angle
boundsSweptAngle givenBounds tree = do
  let centerPoint = Bounds2D.centerPoint givenBounds
  let diameter = Bounds2D.diameter givenBounds
  boundsSweptAngleImpl givenBounds centerPoint diameter tree

boundsSweptAngleImpl ::
  Bounds2D units ->
  Point2D units ->
  Quantity units ->
  BoundaryTree units ->
  Fuzzy Angle
boundsSweptAngleImpl givenBounds centerPoint diameter tree
  | Bounds2D.areDistinct givenBounds tree.bounds = Resolved (distinctSweptAngle centerPoint tree)
  | Bounds2D.diameter tree.bounds < diameter = Unresolved
  | otherwise = do
      resolvedLeftSweptAngle <- boundsSweptAngleImpl givenBounds centerPoint diameter tree.left
      resolvedRightSweptAngle <- boundsSweptAngleImpl givenBounds centerPoint diameter tree.right
      Resolved (resolvedLeftSweptAngle + resolvedRightSweptAngle)
