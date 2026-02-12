module OpenSolid.SurfaceFunction2D
  ( SurfaceFunction2D
  , Compiled
  , new
  , recursive
  , constant
  , uv
  , xy
  , evaluate
  , evaluateBounds
  , derivative
  , distanceAlong
  , xCoordinate
  , yCoordinate
  , coordinates
  , transformBy
  )
where

import GHC.Records (HasField)
import OpenSolid.Axis2D (Axis2D)
import OpenSolid.Axis2D qualified as Axis2D
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import {-# SOURCE #-} OpenSolid.Curve2D (Curve2D)
import {-# SOURCE #-} OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.DirectionSurfaceFunction2D (DirectionSurfaceFunction2D)
import OpenSolid.DirectionSurfaceFunction2D qualified as DirectionSurfaceFunction2D
import OpenSolid.DirectionSurfaceFunction3D (DirectionSurfaceFunction3D)
import OpenSolid.DirectionSurfaceFunction3D qualified as DirectionSurfaceFunction3D
import OpenSolid.Expression qualified as Expression
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceFunction1D qualified as SurfaceFunction1D
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.UvSpace (UvSpace)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

data SurfaceFunction2D units space
  = SurfaceFunction2D
      (Compiled units space)
      ~(VectorSurfaceFunction2D units space)
      ~(VectorSurfaceFunction2D units space)

instance
  HasField
    "xCoordinate"
    (SurfaceFunction2D units space)
    (SurfaceFunction1D units)
  where
  getField = xCoordinate

instance
  HasField
    "yCoordinate"
    (SurfaceFunction2D units space)
    (SurfaceFunction1D units)
  where
  getField = yCoordinate

instance
  HasField
    "coordinates"
    (SurfaceFunction2D units space)
    (SurfaceFunction1D units, SurfaceFunction1D units)
  where
  getField = coordinates

instance
  HasField
    "du"
    (SurfaceFunction2D units space)
    (VectorSurfaceFunction2D units space)
  where
  getField (SurfaceFunction2D _ du _) = du

instance
  HasField
    "dv"
    (SurfaceFunction2D units space)
    (VectorSurfaceFunction2D units space)
  where
  getField (SurfaceFunction2D _ _ dv) = dv

type Compiled units space =
  CompiledFunction
    UvPoint
    (Point2D units space)
    UvBounds
    (Bounds2D units space)

instance HasUnits (SurfaceFunction2D units space) units

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction2D unitsA space1) (SurfaceFunction2D unitsB space2)
  where
  coerce (SurfaceFunction2D c du dv) =
    SurfaceFunction2D (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction2D units1 space1)
  where
  lhs + rhs =
    new
      (lhs.compiled + rhs.compiled)
      (\p -> derivative p lhs + VectorSurfaceFunction2D.derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (SurfaceFunction2D units1 space1)
  where
  f + v = f + VectorSurfaceFunction2D.constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2D units1 space1)
    (VectorSurfaceFunction2D units2 space2)
    (SurfaceFunction2D units1 space1)
  where
  lhs - rhs =
    new
      (lhs.compiled - rhs.compiled)
      (\p -> derivative p lhs - VectorSurfaceFunction2D.derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2D units1 space1)
    (Vector2D units2 space2)
    (SurfaceFunction2D units1 space1)
  where
  f - v = f - VectorSurfaceFunction2D.constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2D units1 space1)
    (SurfaceFunction2D units2 space2)
    (VectorSurfaceFunction2D units1 space1)
  where
  lhs - rhs =
    VectorSurfaceFunction2D.new
      (lhs.compiled - rhs.compiled)
      (\p -> derivative p lhs - derivative p rhs)

instance HasField "compiled" (SurfaceFunction2D units space) (Compiled units space) where
  getField (SurfaceFunction2D c _ _) = c

new ::
  Compiled units space ->
  (SurfaceParameter -> VectorSurfaceFunction2D units space) ->
  SurfaceFunction2D units space
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction2D.new dv.compiled (\case U -> du.dv; V -> dv.dv)
  SurfaceFunction2D c du dv'

recursive ::
  Compiled units space ->
  ( SurfaceFunction2D units space ->
    SurfaceParameter ->
    VectorSurfaceFunction2D units space
  ) ->
  SurfaceFunction2D units space
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

constant :: Point2D units space -> SurfaceFunction2D units space
constant value = new (CompiledFunction.constant value) (const VectorSurfaceFunction2D.zero)

uv :: SurfaceFunction2D Unitless UvSpace
uv = xy SurfaceFunction1D.u SurfaceFunction1D.v

xy ::
  SurfaceFunction1D units ->
  SurfaceFunction1D units ->
  SurfaceFunction2D units space
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Point2D
          Bounds2D
          x.compiled
          y.compiled
  let xyDerivative p =
        VectorSurfaceFunction2D.xy
          (SurfaceFunction1D.derivative p x)
          (SurfaceFunction1D.derivative p y)
  new compiledXY xyDerivative

evaluate :: SurfaceFunction2D units space -> UvPoint -> Point2D units space
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds :: SurfaceFunction2D units space -> UvBounds -> Bounds2D units space
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction2D units space ->
  VectorSurfaceFunction2D units space
derivative U = (.du)
derivative V = (.dv)

transformBy ::
  Transform2D tag units space ->
  SurfaceFunction2D units space ->
  SurfaceFunction2D units space
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.transformBy transform)
          (Point2D.transformBy transform)
          (Bounds2D.transformBy transform)
          function.compiled
  let transformedDerivative p =
        VectorSurfaceFunction2D.transformBy transform (derivative p function)
  new compiledTransformed transformedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2D units space)
    (Curve2D unitless uvSpace)
    (Curve2D units space)
  where
  function `compose` curve = do
    let (dudt, dvdt) = VectorCurve2D.components (Curve2D.derivative curve)
    Curve2D.new
      (function.compiled `compose` Curve2D.compiled curve)
      ((function.du `compose` curve) * dudt + (function.dv `compose` curve) * dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction1D units)
    (SurfaceFunction2D unitless uvSpace)
    (SurfaceFunction1D units)
  where
  f `compose` g = do
    let dfdu = f.du `compose` g
    let dfdv = f.dv `compose` g
    let composedDerivative p = do
          let (dudp, dvdp) = VectorSurfaceFunction2D.components (derivative p g)
          dfdu * dudp + dfdv * dvdp
    SurfaceFunction1D.new
      (f.compiled `compose` g.compiled)
      composedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (VectorSurfaceFunction2D units space)
    (SurfaceFunction2D unitless uvSpace)
    (VectorSurfaceFunction2D units space)
  where
  f `compose` g = do
    let dfdu = f.du `compose` g
    let dfdv = f.dv `compose` g
    let composedDerivative p = do
          let (dudp, dvdp) = VectorSurfaceFunction2D.components (derivative p g)
          dfdu * dudp + dfdv * dvdp
    VectorSurfaceFunction2D.new
      (f.compiled `compose` g.compiled)
      composedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (VectorSurfaceFunction3D units space)
    (SurfaceFunction2D unitless uvSpace)
    (VectorSurfaceFunction3D units space)
  where
  f `compose` g = do
    let dfdu = f.du `compose` g
    let dfdv = f.dv `compose` g
    let composedDerivative p = do
          let (dudp, dvdp) = VectorSurfaceFunction2D.components (derivative p g)
          dfdu * dudp + dfdv * dvdp
    VectorSurfaceFunction3D.new
      (f.compiled `compose` g.compiled)
      composedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (DirectionSurfaceFunction2D space)
    (SurfaceFunction2D unitless uvSpace)
    (DirectionSurfaceFunction2D space)
  where
  f `compose` g =
    DirectionSurfaceFunction2D.unsafe (DirectionSurfaceFunction2D.unwrap f `compose` g)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (DirectionSurfaceFunction3D space)
    (SurfaceFunction2D unitless uvSpace)
    (DirectionSurfaceFunction3D space)
  where
  f `compose` g =
    DirectionSurfaceFunction3D.unsafe (DirectionSurfaceFunction3D.unwrap f `compose` g)

distanceAlong ::
  Axis2D units space ->
  SurfaceFunction2D units space ->
  SurfaceFunction1D units
distanceAlong axis function =
  (function - constant (Axis2D.originPoint axis)) `dot` Axis2D.direction axis

xCoordinate :: SurfaceFunction2D units space -> SurfaceFunction1D units
xCoordinate function = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2D.xCoordinate
          Bounds2D.xCoordinate
          function.compiled
  SurfaceFunction1D.new
    compiledXCoordinate
    (\parameter -> VectorSurfaceFunction2D.xComponent (derivative parameter function))

yCoordinate :: SurfaceFunction2D units space -> SurfaceFunction1D units
yCoordinate function = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2D.yCoordinate
          Bounds2D.yCoordinate
          function.compiled
  SurfaceFunction1D.new
    compiledYCoordinate
    (\parameter -> VectorSurfaceFunction2D.yComponent (derivative parameter function))

coordinates :: SurfaceFunction2D units space -> (SurfaceFunction1D units, SurfaceFunction1D units)
coordinates function = (xCoordinate function, yCoordinate function)
