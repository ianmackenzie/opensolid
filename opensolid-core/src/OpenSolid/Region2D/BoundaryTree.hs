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
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
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

build :: Set2D units (Curve2D units) -> BoundaryTree units
build curves = case curves of
  Set2D.Leaf _ curve -> buildCurve curve
  Set2D.Node nodeBounds leftCurves rightCurves -> do
    let leftChild = build leftCurves
    let rightChild = build rightCurves
    BoundaryTree nodeBounds leftChild.startPoint rightChild.endPoint leftChild rightChild

buildCurve :: Curve2D units -> BoundaryTree units
buildCurve curve =
  buildCurveImpl curve Interval.unit (Curve2D.startPoint curve) (Curve2D.endPoint curve)

buildCurveImpl ::
  Curve2D units ->
  Interval Unitless ->
  Point2D units ->
  Point2D units ->
  BoundaryTree units
buildCurveImpl curve tRange startPoint endPoint = do
  let Interval tLow tHigh = tRange
  let tMid = Number.midpoint tLow tHigh
  let midpoint = Curve2D.point curve tMid
  let leftChild = buildCurveImpl curve (Interval tLow tMid) startPoint midpoint
  let rightChild = buildCurveImpl curve (Interval tMid tHigh) midpoint endPoint
  BoundaryTree (Curve2D.range curve tRange) startPoint endPoint leftChild rightChild

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
