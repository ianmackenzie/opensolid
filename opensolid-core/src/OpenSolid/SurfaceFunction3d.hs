module OpenSolid.SurfaceFunction3d
  ( SurfaceFunction3d
  , Compiled
  , new
  , constant
  , evaluate
  , evaluateBounds
  , derivative
  , IsDegenerate (IsDegenerate)
  , normalDirection
  , placeIn
  , relativeTo
  , transformBy
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Bounds3d qualified as Bounds3d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.DirectionSurfaceFunction3d (DirectionSurfaceFunction3d)
import OpenSolid.Expression.Surface3d qualified as Expression.Surface3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Region2d (Region2d)
import {-# SOURCE #-} OpenSolid.Surface3d (Surface3d)
import {-# SOURCE #-} OpenSolid.Surface3d qualified as Surface3d
import OpenSolid.SurfaceFunction2d (SurfaceFunction2d)
import OpenSolid.SurfaceFunction2d qualified as SurfaceFunction2d
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data SurfaceFunction3d space
  = SurfaceFunction3d
      (Compiled space)
      ~(VectorSurfaceFunction3d space Meters)
      ~(VectorSurfaceFunction3d space Meters)

instance
  HasField
    "du"
    (SurfaceFunction3d space)
    (VectorSurfaceFunction3d space Meters)
  where
  getField (SurfaceFunction3d _ du _) = du

instance
  HasField
    "dv"
    (SurfaceFunction3d space)
    (VectorSurfaceFunction3d space Meters)
  where
  getField (SurfaceFunction3d _ _ dv) = dv

type Compiled space =
  CompiledFunction UvPoint (Point3d space) UvBounds (Bounds3d space)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3d space1)
    (VectorSurfaceFunction3d space2 meters)
    (SurfaceFunction3d space1)
  where
  lhs .+. rhs =
    new
      (lhs.compiled .+. rhs.compiled)
      (\parameter -> derivative parameter lhs .+. VectorSurfaceFunction3d.derivative parameter rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3d space1)
    (Vector3d space2 meters)
    (SurfaceFunction3d space1)
  where
  f .+. v = f .+. VectorSurfaceFunction3d.constant v

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3d space1)
    (VectorSurfaceFunction3d space2 meters)
    (SurfaceFunction3d space1)
  where
  lhs .-. rhs =
    new
      (lhs.compiled .-. rhs.compiled)
      (\parameter -> derivative parameter lhs .-. VectorSurfaceFunction3d.derivative parameter rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3d space1)
    (Vector3d space2 meters)
    (SurfaceFunction3d space1)
  where
  f .-. v = f .-. VectorSurfaceFunction3d.constant v

instance
  space1 ~ space2 =>
  Subtraction
    (SurfaceFunction3d space1)
    (SurfaceFunction3d space2)
    (VectorSurfaceFunction3d space1 Meters)
  where
  lhs .-. rhs =
    VectorSurfaceFunction3d.new
      (lhs.compiled .-. rhs.compiled)
      (\parameter -> derivative parameter lhs .-. derivative parameter rhs)

instance
  space1 ~ space2 =>
  Subtraction
    (SurfaceFunction3d space1)
    (Point3d space2)
    (VectorSurfaceFunction3d space1 Meters)
  where
  function .-. point = function .-. constant point

instance
  space1 ~ space2 =>
  Subtraction
    (Point3d space1)
    (SurfaceFunction3d space2)
    (VectorSurfaceFunction3d space1 Meters)
  where
  point .-. function = constant point .-. function

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Region2d uvSpace unitless)
    (SurfaceFunction3d space)
    (Surface3d space)
  where
  function `compose` domain = Surface3d.parametric function domain

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (SurfaceFunction3d space)
    (SurfaceFunction3d space)
  where
  outer `compose` inner = do
    let duOuter = outer.du `compose` inner
    let dvOuter = outer.dv `compose` inner
    let composedDerivative parameter = do
          let innerDerivative = SurfaceFunction2d.derivative parameter inner
          let (dU, dV) = innerDerivative.components
          duOuter .*. dU .+. dvOuter .*. dV
    new (outer.compiled `compose` inner.compiled) composedDerivative

instance HasField "compiled" (SurfaceFunction3d space) (Compiled space) where
  getField (SurfaceFunction3d c _ _) = c

new ::
  Compiled space ->
  (SurfaceParameter -> VectorSurfaceFunction3d space Meters) ->
  SurfaceFunction3d space
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction3d.new dv.compiled (\case U -> du.dv; V -> dv.dv)
  SurfaceFunction3d c du dv'

constant :: Point3d space -> SurfaceFunction3d space
constant value = new (CompiledFunction.constant value) (const VectorSurfaceFunction3d.zero)

evaluate :: SurfaceFunction3d space -> UvPoint -> Point3d space
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds :: SurfaceFunction3d space -> UvBounds -> Bounds3d space
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction3d space ->
  VectorSurfaceFunction3d space Meters
derivative U = (.du)
derivative V = (.dv)

data IsDegenerate = IsDegenerate deriving (Eq, Show)

derivativeDirection ::
  Tolerance units =>
  VectorSurfaceFunction3d space units ->
  Result IsDegenerate (DirectionSurfaceFunction3d space)
derivativeDirection partialDerivative = case VectorSurfaceFunction3d.direction partialDerivative of
  Ok direction -> Ok direction
  Error VectorSurfaceFunction3d.IsZero -> Error IsDegenerate

normalDirection ::
  Tolerance Meters =>
  SurfaceFunction3d space ->
  Result IsDegenerate (DirectionSurfaceFunction3d space)
normalDirection function = do
  duDirection <- derivativeDirection function.du
  dvDirection <- derivativeDirection function.dv
  let crossProduct = duDirection `cross` dvDirection
  case Tolerance.using 1e-9 (VectorSurfaceFunction3d.direction crossProduct) of
    Ok directionFunction -> Ok directionFunction
    Error VectorSurfaceFunction3d.IsZero -> Error IsDegenerate

transformBy :: Transform3d tag space -> SurfaceFunction3d space -> SurfaceFunction3d space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.Surface3d.transformBy transform)
          (Point3d.transformBy transform)
          (Bounds3d.transformBy transform)
          function.compiled
  let transformedDerivative parameter =
        VectorSurfaceFunction3d.transformBy transform (derivative parameter function)
  new compiledTransformed transformedDerivative

placeIn :: Frame3d global (Defines local) -> SurfaceFunction3d local -> SurfaceFunction3d global
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.Surface3d.placeIn frame)
          (Point3d.placeIn frame)
          (Bounds3d.placeIn frame)
          function.compiled
  let placedDerivative parameter =
        VectorSurfaceFunction3d.placeIn frame (derivative parameter function)
  new compiledPlaced placedDerivative

relativeTo :: Frame3d global (Defines local) -> SurfaceFunction3d global -> SurfaceFunction3d local
relativeTo frame = placeIn (Frame3d.inverse frame)
