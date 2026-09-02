module OpenSolid.SurfaceFunction2D
  ( SurfaceFunction2D
  , Compiled
  , new
  , constant
  , uv
  , xy
  , pointAt
  , pointOn
  , range
  , compiled
  , partialDerivatives
  , distanceAlong
  , xCoordinate
  , yCoordinate
  , coordinates
  , transformBy
  )
where

import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Pair qualified as Pair
import OpenSolid.PartialDerivatives qualified as PartialDerivatives
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data SurfaceFunction2D units = SurfaceFunction2D
  { compiled :: Compiled units
  , partialDerivatives :: (VectorSurfaceFunction2D units, VectorSurfaceFunction2D units)
  }

type Compiled units =
  CompiledFunction UvPoint (Point2D units) UvBounds (Bounds2D units)

instance Units (SurfaceFunction2D units) units

instance Units.Coercion (SurfaceFunction2D units1) (SurfaceFunction2D units2) where
  coerce function =
    SurfaceFunction2D
      { compiled = Units.coerce function.compiled
      , partialDerivatives = Pair.map Units.coerce function.partialDerivatives
      }

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction2D units1)
  where
  f + g =
    new
      (compiled f + VectorSurfaceFunction2D.compiled g)
      (Pair.map2 (+) (partialDerivatives f) (VectorSurfaceFunction2D.partialDerivatives g))

instance
  units1 ~ units2 =>
  Addition
    (SurfaceFunction2D units1)
    (Vector2D units2)
    (SurfaceFunction2D units1)
  where
  f + v = f + VectorSurfaceFunction2D.constant v

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction2D units1)
    (VectorSurfaceFunction2D units2)
    (SurfaceFunction2D units1)
  where
  f - g =
    new
      (compiled f - VectorSurfaceFunction2D.compiled g)
      (Pair.map2 (-) (partialDerivatives f) (VectorSurfaceFunction2D.partialDerivatives g))

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction2D units1)
    (Vector2D units2)
    (SurfaceFunction2D units1)
  where
  f - v = f - VectorSurfaceFunction2D.constant v

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction2D units1)
    (SurfaceFunction2D units2)
    (VectorSurfaceFunction2D units1)
  where
  f - g =
    VectorSurfaceFunction2D.new
      (compiled f - compiled g)
      (Pair.map2 (-) (partialDerivatives f) (partialDerivatives g))

instance
  units1 ~ units2 =>
  Subtraction
    (SurfaceFunction2D units1)
    (Point2D units2)
    (VectorSurfaceFunction2D units1)
  where
  function - point = function - constant point

instance
  units1 ~ units2 =>
  Subtraction
    (Point2D units2)
    (SurfaceFunction2D units1)
    (VectorSurfaceFunction2D units1)
  where
  point - function = constant point - function

new ::
  Compiled units ->
  (VectorSurfaceFunction2D units, VectorSurfaceFunction2D units) ->
  SurfaceFunction2D units
new givenCompiled givenPartialDerivatives = do
  let mergedPartialDerivatives =
        PartialDerivatives.merge
          VectorSurfaceFunction2D.new
          VectorSurfaceFunction2D.compiled
          VectorSurfaceFunction2D.partialDerivatives
          givenPartialDerivatives
  SurfaceFunction2D givenCompiled mergedPartialDerivatives

constant :: Point2D units -> SurfaceFunction2D units
constant value =
  new (CompiledFunction.constant value) (VectorSurfaceFunction2D.zero, VectorSurfaceFunction2D.zero)

uv :: SurfaceFunction2D Unitless
uv = xy SurfaceFunction1D.u SurfaceFunction1D.v

xy :: SurfaceFunction1D units -> SurfaceFunction1D units -> SurfaceFunction2D units
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Point2D
          Bounds2D
          (SurfaceFunction1D.compiled x)
          (SurfaceFunction1D.compiled y)
  let xyPartialDerivatives =
        Pair.map2
          VectorSurfaceFunction2D.xy
          (SurfaceFunction1D.partialDerivatives x)
          (SurfaceFunction1D.partialDerivatives y)
  new compiledXY xyPartialDerivatives

{-# INLINE pointAt #-}
pointAt :: UvPoint -> SurfaceFunction2D units -> Point2D units
pointAt uvPoint function = CompiledFunction.value uvPoint (compiled function)

{-# INLINE pointOn #-}
pointOn :: SurfaceFunction2D units -> UvPoint -> Point2D units
pointOn function uvPoint = pointAt uvPoint function

{-# INLINE range #-}
range :: SurfaceFunction2D units -> UvBounds -> Bounds2D units
range function uvRange = CompiledFunction.range uvRange (compiled function)

{-# INLINE compiled #-}
compiled :: SurfaceFunction2D units -> Compiled units
compiled = (.compiled)

{-# INLINE partialDerivatives #-}
partialDerivatives ::
  SurfaceFunction2D units ->
  (VectorSurfaceFunction2D units, VectorSurfaceFunction2D units)
partialDerivatives = (.partialDerivatives)

transformBy :: Transform2D tag units -> SurfaceFunction2D units -> SurfaceFunction2D units
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point2D.transformBy transform)
          (Bounds2D.transformBy transform)
          function.compiled
  let transformDerivative =
        VectorSurfaceFunction2D.transformBy (Transform2D.vectorTransform transform)
  new compiledTransformed (Pair.map transformDerivative function.partialDerivatives)

instance
  Composition
    (SurfaceFunction2D units)
    (Curve2D Unitless)
    (Curve2D units)
  where
  f . g = do
    let (dfdu, dfdv) = Pair.map (. g) (partialDerivatives f)
    let (dudt, dvdt) = VectorCurve2D.components (Curve2D.derivative g)
    Curve2D.new (compiled f . Curve2D.compiled g) (dfdu * dudt + dfdv * dvdt)

instance
  Composition
    (SurfaceFunction1D units)
    (SurfaceFunction2D Unitless)
    (SurfaceFunction1D units)
  where
  f . g = do
    let (dfdx, dfdy) = Pair.map (. g) (SurfaceFunction1D.partialDerivatives f)
    let (dgdu, dgdv) = partialDerivatives g
    let (dxdu, dydu) = VectorSurfaceFunction2D.components dgdu
    let (dxdv, dydv) = VectorSurfaceFunction2D.components dgdv
    let compiledComposed = SurfaceFunction1D.compiled f . compiled g
    let composedPartialDerivatives =
          ( dfdx * dxdu + dfdy * dydu
          , dfdx * dxdv + dfdy * dydv
          )
    SurfaceFunction1D.new compiledComposed composedPartialDerivatives

instance
  Composition
    (VectorSurfaceFunction2D units)
    (SurfaceFunction2D Unitless)
    (VectorSurfaceFunction2D units)
  where
  f . g = do
    let (dfdx, dfdy) = Pair.map (. g) (VectorSurfaceFunction2D.partialDerivatives f)
    let (dgdu, dgdv) = partialDerivatives g
    let (dxdu, dydu) = VectorSurfaceFunction2D.components dgdu
    let (dxdv, dydv) = VectorSurfaceFunction2D.components dgdv
    let compiledComposed = VectorSurfaceFunction2D.compiled f . compiled g
    let composedPartialDerivatives =
          ( dfdx * dxdu + dfdy * dydu
          , dfdx * dxdv + dfdy * dydv
          )
    VectorSurfaceFunction2D.new compiledComposed composedPartialDerivatives

instance
  Composition
    (VectorSurfaceFunction3D units space)
    (SurfaceFunction2D Unitless)
    (VectorSurfaceFunction3D units space)
  where
  f . g = do
    let (dfdx, dfdy) = Pair.map (. g) (VectorSurfaceFunction3D.partialDerivatives f)
    let (dgdu, dgdv) = partialDerivatives g
    let (dxdu, dydu) = VectorSurfaceFunction2D.components dgdu
    let (dxdv, dydv) = VectorSurfaceFunction2D.components dgdv
    let compiledComposed = VectorSurfaceFunction3D.compiled f . compiled g
    let composedPartialDerivatives =
          ( dfdx * dxdu + dfdy * dydu
          , dfdx * dxdv + dfdy * dydv
          )
    VectorSurfaceFunction3D.new compiledComposed composedPartialDerivatives

distanceAlong :: Axis2D units -> SurfaceFunction2D units -> SurfaceFunction1D units
distanceAlong axis function = (function - Axis2D.originPoint axis) `dot` Axis2D.direction axis

xCoordinate :: SurfaceFunction2D units -> SurfaceFunction1D units
xCoordinate function = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2D.xCoordinate
          Bounds2D.xCoordinate
          function.compiled
  let xCoordinatePartialDerivatives =
        Pair.map VectorSurfaceFunction2D.xComponent function.partialDerivatives
  SurfaceFunction1D.new compiledXCoordinate xCoordinatePartialDerivatives

yCoordinate :: SurfaceFunction2D units -> SurfaceFunction1D units
yCoordinate function = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2D.yCoordinate
          Bounds2D.yCoordinate
          function.compiled
  let yCoordinatePartialDerivatives =
        Pair.map VectorSurfaceFunction2D.yComponent function.partialDerivatives
  SurfaceFunction1D.new compiledYCoordinate yCoordinatePartialDerivatives

coordinates :: SurfaceFunction2D units -> (SurfaceFunction1D units, SurfaceFunction1D units)
coordinates function = (xCoordinate function, yCoordinate function)
