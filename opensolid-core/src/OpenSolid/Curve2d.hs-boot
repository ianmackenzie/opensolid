module OpenSolid.Curve2d
  ( Curve2d
  , Compiled
  , HasDegeneracy (HasDegeneracy)
  , constant
  , new
  , evaluate
  , evaluateBounds
  , bounds
  , tangentDirection
  , reverse
  , hermite
  , synthetic
  , transformBy
  , piecewise
  , unsafePiecewise
  )
where

import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.CompiledFunction (CompiledFunction)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Vector2d (Vector2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

type role Curve2d nominal

data Curve2d (coordinateSystem :: CoordinateSystem)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Point2d coordinateSystem)
    (Bounds Unitless)
    (Bounds2d coordinateSystem)

data HasDegeneracy = HasDegeneracy

instance HasField "compiled" (Curve2d (space @ units)) (Compiled (space @ units))

instance HasField "derivative" (Curve2d (space @ units)) (VectorCurve2d (space @ units))

instance HasField "startPoint" (Curve2d (space @ units)) (Point2d (space @ units))

instance HasField "endPoint" (Curve2d (space @ units)) (Point2d (space @ units))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))

constant :: Point2d (space @ units) -> Curve2d (space @ units)
new :: Compiled (space @ units) -> VectorCurve2d (space @ units) -> Curve2d (space @ units)
evaluate :: Curve2d (space @ units) -> Float -> Point2d (space @ units)
evaluateBounds :: Curve2d (space @ units) -> Bounds Unitless -> Bounds2d (space @ units)
bounds :: Curve2d (space @ units) -> Bounds2d (space @ units)
tangentDirection ::
  Tolerance units =>
  Curve2d (space @ units) ->
  Result HasDegeneracy (DirectionCurve2d space)
reverse :: Curve2d (space @ units) -> Curve2d (space @ units)
hermite ::
  Point2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  Point2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  Curve2d (space @ units)
synthetic :: Curve2d (space @ units) -> VectorCurve2d (space @ units) -> Curve2d (space @ units)
transformBy ::
  Transform2d tag (space @ units) ->
  Curve2d (space @ units) ->
  Curve2d (space @ units)
piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d (space @ units)) ->
  Result HasDegeneracy (Curve2d (space @ units))
unsafePiecewise :: NonEmpty (Curve2d (space @ units)) -> Curve2d (space @ units)
