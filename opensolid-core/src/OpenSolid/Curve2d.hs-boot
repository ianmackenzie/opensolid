module OpenSolid.Curve2d
  ( Curve2d
  , Compiled
  , IsPoint (IsPoint)
  , constant
  , new
  , evaluate
  , evaluateBounds
  , bounds
  , tangentDirection
  , reverse
  , xy
  , line
  , hermite
  , desingularize
  , transformBy
  , piecewise
  )
where

import GHC.Records (HasField)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Vector2d (Vector2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

type role Curve2d nominal nominal

type Curve2d :: Type -> Type -> Type
data Curve2d space units

type Compiled space units =
  CompiledFunction
    Number
    (Point2d space units)
    (Bounds Unitless)
    (Bounds2d space units)

data IsPoint = IsPoint

instance HasField "compiled" (Curve2d space units) (Compiled space units)

instance HasField "derivative" (Curve2d space units) (VectorCurve2d space units)

instance HasField "startPoint" (Curve2d space units) (Point2d space units)

instance HasField "endPoint" (Curve2d space units) (Point2d space units)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2d space1 units1)
    (VectorCurve2d space2 units2)
    (Curve2d space1 units1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d space1 units1)
    (VectorCurve2d space2 units2)
    (Curve2d space1 units1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d uvSpace unitless)
    (SurfaceFunction units)
    (Curve units)

constant :: Point2d space units -> Curve2d space units
new :: Compiled space units -> VectorCurve2d space units -> Curve2d space units
evaluate :: Curve2d space units -> Number -> Point2d space units
evaluateBounds :: Curve2d space units -> Bounds Unitless -> Bounds2d space units
bounds :: Curve2d space units -> Bounds2d space units
tangentDirection ::
  Tolerance units =>
  Curve2d space units ->
  Result IsPoint (DirectionCurve2d space)
reverse :: Curve2d space units -> Curve2d space units
xy :: Curve units -> Curve units -> Curve2d space units
line :: Point2d space units -> Point2d space units -> Curve2d space units
hermite ::
  Point2d space units ->
  List (Vector2d space units) ->
  Point2d space units ->
  List (Vector2d space units) ->
  Curve2d space units
desingularize ::
  Maybe (Point2d space units, Vector2d space units) ->
  Curve2d space units ->
  Maybe (Point2d space units, Vector2d space units) ->
  Curve2d space units
transformBy ::
  Transform2d tag space units ->
  Curve2d space units ->
  Curve2d space units
piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d space units) ->
  Curve2d space units
