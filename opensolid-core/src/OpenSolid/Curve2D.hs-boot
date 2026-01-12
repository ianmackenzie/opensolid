module OpenSolid.Curve2D
  ( Curve2D
  , Compiled
  , IsPoint (IsPoint)
  , constant
  , new
  , evaluate
  , evaluateBounds
  , bounds
  , compiled
  , derivative
  , tangentDirection
  , reverse
  , xy
  , lineFrom
  , hermite
  , desingularize
  , transformBy
  , piecewise
  )
where

import GHC.Records (HasField)
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.Curve (Curve)
import {-# SOURCE #-} OpenSolid.DirectionCurve2D (DirectionCurve2D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.Transform2D (Transform2D)
import {-# SOURCE #-} OpenSolid.VectorCurve2D (VectorCurve2D)

type role Curve2D nominal nominal

type Curve2D :: Type -> Type -> Type
data Curve2D units space

type Compiled units space =
  CompiledFunction
    Number
    (Point2D units space)
    (Bounds Unitless)
    (Bounds2D units space)

data IsPoint = IsPoint

instance HasField "startPoint" (Curve2D units space) (Point2D units space)

instance HasField "endPoint" (Curve2D units space) (Point2D units space)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Curve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Curve2D units1 space1)
    (VectorCurve2D units2 space2)
    (Curve2D units1 space1)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2D unitless uvSpace)
    (SurfaceFunction units)
    (Curve units)

constant :: Point2D units space -> Curve2D units space
new :: Compiled units space -> VectorCurve2D units space -> Curve2D units space
evaluate :: Curve2D units space -> Number -> Point2D units space
evaluateBounds :: Curve2D units space -> Bounds Unitless -> Bounds2D units space
bounds :: Curve2D units space -> Bounds2D units space
compiled :: Curve2D units space -> Compiled units space
derivative :: Curve2D units space -> VectorCurve2D units space
tangentDirection ::
  Tolerance units =>
  Curve2D units space ->
  Result IsPoint (DirectionCurve2D space)
reverse :: Curve2D units space -> Curve2D units space
xy :: Curve units -> Curve units -> Curve2D units space
lineFrom :: Point2D units space -> Point2D units space -> Curve2D units space
hermite ::
  Point2D units space ->
  List (Vector2D units space) ->
  Point2D units space ->
  List (Vector2D units space) ->
  Curve2D units space
desingularize ::
  Maybe (Point2D units space, Vector2D units space) ->
  Curve2D units space ->
  Maybe (Point2D units space, Vector2D units space) ->
  Curve2D units space
transformBy ::
  Transform2D tag units space ->
  Curve2D units space ->
  Curve2D units space
piecewise ::
  Tolerance units =>
  NonEmpty (Curve2D units space) ->
  Curve2D units space
