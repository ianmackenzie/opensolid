module OpenSolid.SurfaceFunction3D
  ( SurfaceFunction3D
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

import GHC.Records (HasField)
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.DirectionSurfaceFunction3D (DirectionSurfaceFunction3D)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Region2D (Region2D)
import {-# SOURCE #-} OpenSolid.Surface3D (Surface3D)
import {-# SOURCE #-} OpenSolid.Surface3D qualified as Surface3D
import OpenSolid.SurfaceFunction2D (SurfaceFunction2D)
import OpenSolid.SurfaceFunction2D qualified as SurfaceFunction2D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3D (Transform3D)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvSpace (UvSpace)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data SurfaceFunction3D space
  = SurfaceFunction3D
      (Compiled space)
      ~(VectorSurfaceFunction3D Meters space)
      ~(VectorSurfaceFunction3D Meters space)

instance
  HasField
    "du"
    (SurfaceFunction3D space)
    (VectorSurfaceFunction3D Meters space)
  where
  getField (SurfaceFunction3D _ du _) = du

instance
  HasField
    "dv"
    (SurfaceFunction3D space)
    (VectorSurfaceFunction3D Meters space)
  where
  getField (SurfaceFunction3D _ _ dv) = dv

type Compiled space =
  CompiledFunction UvPoint (Point3D space) UvBounds (Bounds3D space)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)
  where
  lhs + rhs =
    new
      (lhs.compiled + rhs.compiled)
      (\parameter -> derivative parameter lhs + VectorSurfaceFunction3D.derivative parameter rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Addition
    (SurfaceFunction3D space1)
    (Vector3D meters space2)
    (SurfaceFunction3D space1)
  where
  f + v = f + VectorSurfaceFunction3D.constant v

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3D space1)
    (VectorSurfaceFunction3D meters space2)
    (SurfaceFunction3D space1)
  where
  lhs - rhs =
    new
      (lhs.compiled - rhs.compiled)
      (\parameter -> derivative parameter lhs - VectorSurfaceFunction3D.derivative parameter rhs)

instance
  (space1 ~ space2, meters ~ Meters) =>
  Subtraction
    (SurfaceFunction3D space1)
    (Vector3D meters space2)
    (SurfaceFunction3D space1)
  where
  f - v = f - VectorSurfaceFunction3D.constant v

instance
  space1 ~ space2 =>
  Subtraction
    (SurfaceFunction3D space1)
    (SurfaceFunction3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  lhs - rhs =
    VectorSurfaceFunction3D.new
      (lhs.compiled - rhs.compiled)
      (\parameter -> derivative parameter lhs - derivative parameter rhs)

instance
  space1 ~ space2 =>
  Subtraction
    (SurfaceFunction3D space1)
    (Point3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  function - point = function - constant point

instance
  space1 ~ space2 =>
  Subtraction
    (Point3D space1)
    (SurfaceFunction3D space2)
    (VectorSurfaceFunction3D Meters space1)
  where
  point - function = constant point - function

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Region2D unitless uvSpace)
    (SurfaceFunction3D space)
    (Surface3D space)
  where
  function `compose` domain = Surface3D.parametric function domain

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2D unitless uvSpace)
    (SurfaceFunction3D space)
    (SurfaceFunction3D space)
  where
  outer `compose` inner = do
    let duOuter = outer.du `compose` inner
    let dvOuter = outer.dv `compose` inner
    let composedDerivative parameter = do
          let innerDerivative = SurfaceFunction2D.derivative parameter inner
          let (dU, dV) = innerDerivative.components
          duOuter * dU + dvOuter * dV
    new (outer.compiled `compose` inner.compiled) composedDerivative

instance HasField "compiled" (SurfaceFunction3D space) (Compiled space) where
  getField (SurfaceFunction3D c _ _) = c

new ::
  Compiled space ->
  (SurfaceParameter -> VectorSurfaceFunction3D Meters space) ->
  SurfaceFunction3D space
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction3D.new dv.compiled (\case U -> du.dv; V -> dv.dv)
  SurfaceFunction3D c du dv'

constant :: Point3D space -> SurfaceFunction3D space
constant value = new (CompiledFunction.constant value) (const VectorSurfaceFunction3D.zero)

evaluate :: SurfaceFunction3D space -> UvPoint -> Point3D space
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds :: SurfaceFunction3D space -> UvBounds -> Bounds3D space
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction3D space ->
  VectorSurfaceFunction3D Meters space
derivative U = (.du)
derivative V = (.dv)

data IsDegenerate = IsDegenerate deriving (Eq, Show)

derivativeDirection ::
  Tolerance units =>
  VectorSurfaceFunction3D units space ->
  Result IsDegenerate (DirectionSurfaceFunction3D space)
derivativeDirection partialDerivative = case VectorSurfaceFunction3D.direction partialDerivative of
  Ok direction -> Ok direction
  Error VectorSurfaceFunction3D.IsZero -> Error IsDegenerate

normalDirection ::
  Tolerance Meters =>
  SurfaceFunction3D space ->
  Result IsDegenerate (DirectionSurfaceFunction3D space)
normalDirection function = do
  duDirection <- derivativeDirection function.du
  dvDirection <- derivativeDirection function.dv
  let crossProduct = duDirection `cross` dvDirection
  case Tolerance.using 1e-9 (VectorSurfaceFunction3D.direction crossProduct) of
    Ok directionFunction -> Ok directionFunction
    Error VectorSurfaceFunction3D.IsZero -> Error IsDegenerate

transformBy :: Transform3D tag space -> SurfaceFunction3D space -> SurfaceFunction3D space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point3D.transformBy transform)
          (Bounds3D.transformBy transform)
          function.compiled
  let transformedDerivative parameter =
        VectorSurfaceFunction3D.transformBy transform (derivative parameter function)
  new compiledTransformed transformedDerivative

placeIn :: Frame3D global local -> SurfaceFunction3D local -> SurfaceFunction3D global
placeIn frame function = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.placeIn frame)
          (Point3D.placeIn frame)
          (Bounds3D.placeIn frame)
          function.compiled
  let placedDerivative parameter =
        VectorSurfaceFunction3D.placeIn frame (derivative parameter function)
  new compiledPlaced placedDerivative

relativeTo :: Frame3D global local -> SurfaceFunction3D global -> SurfaceFunction3D local
relativeTo frame = placeIn (Frame3D.inverse frame)
