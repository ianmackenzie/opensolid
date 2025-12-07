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
import OpenSolid.Polymorphic.Point2d (Point2d)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Polymorphic.Vector2d (Vector2d)
import {-# SOURCE #-} OpenSolid.VectorCurve2d (VectorCurve2d)

type role Curve2d nominal nominal

type Curve2d :: Type -> Type -> Type
data Curve2d units space

type Compiled units space =
  CompiledFunction
    Number
    (Point2d units space)
    (Bounds Unitless)
    (Bounds2d units space)

data IsPoint = IsPoint

instance HasField "compiled" (Curve2d units space) (Compiled units space)

instance HasField "derivative" (Curve2d units space) (VectorCurve2d units space)

instance HasField "startPoint" (Curve2d units space) (Point2d units space)

instance HasField "endPoint" (Curve2d units space) (Point2d units space)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve2d units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2d units1 space1)
    (VectorCurve2d units2 space2)
    (Curve2d units1 space1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d unitless uvSpace)
    (SurfaceFunction units)
    (Curve units)

constant :: Point2d units space -> Curve2d units space
new :: Compiled units space -> VectorCurve2d units space -> Curve2d units space
evaluate :: Curve2d units space -> Number -> Point2d units space
evaluateBounds :: Curve2d units space -> Bounds Unitless -> Bounds2d units space
bounds :: Curve2d units space -> Bounds2d units space
tangentDirection ::
  Tolerance units =>
  Curve2d units space ->
  Result IsPoint (DirectionCurve2d space)
reverse :: Curve2d units space -> Curve2d units space
xy :: Curve units -> Curve units -> Curve2d units space
line :: Point2d units space -> Point2d units space -> Curve2d units space
hermite ::
  Point2d units space ->
  List (Vector2d units space) ->
  Point2d units space ->
  List (Vector2d units space) ->
  Curve2d units space
desingularize ::
  Maybe (Point2d units space, Vector2d units space) ->
  Curve2d units space ->
  Maybe (Point2d units space, Vector2d units space) ->
  Curve2d units space
transformBy ::
  Transform2d tag units space ->
  Curve2d units space ->
  Curve2d units space
piecewise ::
  Tolerance units =>
  NonEmpty (Curve2d units space) ->
  Curve2d units space
