module OpenSolid.SurfaceFunction2d
  ( SurfaceFunction2d
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

import GHC.Records (HasField (getField))
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.DirectionSurfaceFunction2d (DirectionSurfaceFunction2d)
import OpenSolid.DirectionSurfaceFunction2d qualified as DirectionSurfaceFunction2d
import OpenSolid.DirectionSurfaceFunction3d (DirectionSurfaceFunction3d)
import OpenSolid.DirectionSurfaceFunction3d qualified as DirectionSurfaceFunction3d
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Surface2d qualified as Expression.Surface2d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data SurfaceFunction2d space units
  = SurfaceFunction2d
      (Compiled space units)
      ~(VectorSurfaceFunction2d space units)
      ~(VectorSurfaceFunction2d space units)

instance
  HasField
    "xCoordinate"
    (SurfaceFunction2d space units)
    (SurfaceFunction units)
  where
  getField = xCoordinate

instance
  HasField
    "yCoordinate"
    (SurfaceFunction2d space units)
    (SurfaceFunction units)
  where
  getField = yCoordinate

instance
  HasField
    "coordinates"
    (SurfaceFunction2d space units)
    (SurfaceFunction units, SurfaceFunction units)
  where
  getField = coordinates

instance
  HasField
    "du"
    (SurfaceFunction2d space units)
    (VectorSurfaceFunction2d space units)
  where
  getField (SurfaceFunction2d _ du _) = du

instance
  HasField
    "dv"
    (SurfaceFunction2d space units)
    (VectorSurfaceFunction2d space units)
  where
  getField (SurfaceFunction2d _ _ dv) = dv

type Compiled space units =
  CompiledFunction
    UvPoint
    (Point2d space units)
    UvBounds
    (Bounds2d space units)

instance HasUnits (SurfaceFunction2d space units) units

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction2d space1 unitsA) (SurfaceFunction2d space2 unitsB)
  where
  coerce (SurfaceFunction2d c du dv) =
    SurfaceFunction2d (Units.coerce c) (Units.coerce du) (Units.coerce dv)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2d space1 units1)
    (VectorSurfaceFunction2d space2 units2)
    (SurfaceFunction2d space1 units1)
  where
  lhs .+. rhs =
    new
      (lhs.compiled .+. rhs.compiled)
      (\p -> derivative p lhs .+. VectorSurfaceFunction2d.derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2d space1 units1)
    (Vector2d space2 units2)
    (SurfaceFunction2d space1 units1)
  where
  f .+. v = f .+. VectorSurfaceFunction2d.constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2d space1 units1)
    (VectorSurfaceFunction2d space2 units2)
    (SurfaceFunction2d space1 units1)
  where
  lhs .-. rhs =
    new
      (lhs.compiled .-. rhs.compiled)
      (\p -> derivative p lhs .-. VectorSurfaceFunction2d.derivative p rhs)

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2d space1 units1)
    (Vector2d space2 units2)
    (SurfaceFunction2d space1 units1)
  where
  f .-. v = f .-. VectorSurfaceFunction2d.constant v

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d space1 units1)
    (SurfaceFunction2d space2 units2)
    (VectorSurfaceFunction2d space1 units1)
  where
  lhs .-. rhs =
    VectorSurfaceFunction2d.new
      (lhs.compiled .-. rhs.compiled)
      (\p -> derivative p lhs .-. derivative p rhs)

instance HasField "compiled" (SurfaceFunction2d space units) (Compiled space units) where
  getField (SurfaceFunction2d c _ _) = c

new ::
  Compiled space units ->
  (SurfaceParameter -> VectorSurfaceFunction2d space units) ->
  SurfaceFunction2d space units
new c derivativeFunction = do
  let du = derivativeFunction U
  let dv = derivativeFunction V
  let dv' = VectorSurfaceFunction2d.new dv.compiled (\case U -> du.dv; V -> dv.dv)
  SurfaceFunction2d c du dv'

recursive ::
  Compiled space units ->
  ( SurfaceFunction2d space units ->
    SurfaceParameter ->
    VectorSurfaceFunction2d space units
  ) ->
  SurfaceFunction2d space units
recursive givenCompiled derivativeFunction =
  let self = new givenCompiled (derivativeFunction self) in self

constant :: Point2d space units -> SurfaceFunction2d space units
constant value = new (CompiledFunction.constant value) (const VectorSurfaceFunction2d.zero)

uv :: SurfaceFunction2d UvSpace Unitless
uv = xy SurfaceFunction.u SurfaceFunction.v

xy ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction2d space units
xy x y = do
  let compiledXY =
        CompiledFunction.map2
          Expression.xy
          Point2d
          Bounds2d
          x.compiled
          y.compiled
  let xyDerivative p =
        VectorSurfaceFunction2d.xy
          (SurfaceFunction.derivative p x)
          (SurfaceFunction.derivative p y)
  new compiledXY xyDerivative

evaluate :: SurfaceFunction2d space units -> UvPoint -> Point2d space units
evaluate function uvPoint = CompiledFunction.evaluate function.compiled uvPoint

evaluateBounds :: SurfaceFunction2d space units -> UvBounds -> Bounds2d space units
evaluateBounds function uvBounds = CompiledFunction.evaluateBounds function.compiled uvBounds

derivative ::
  SurfaceParameter ->
  SurfaceFunction2d space units ->
  VectorSurfaceFunction2d space units
derivative U = (.du)
derivative V = (.dv)

transformBy ::
  Transform2d tag space units ->
  SurfaceFunction2d space units ->
  SurfaceFunction2d space units
transformBy transform function = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.Surface2d.transformBy transform)
          (Point2d.transformBy transform)
          (Bounds2d.transformBy transform)
          function.compiled
  let transformedDerivative p =
        VectorSurfaceFunction2d.transformBy transform (derivative p function)
  new compiledTransformed transformedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (Curve2d uvSpace unitless)
    (SurfaceFunction2d space units)
    (Curve2d space units)
  where
  function `compose` curve = do
    let (dudt, dvdt) = curve.derivative.components
    Curve2d.new
      (function.compiled `compose` curve.compiled)
      ((function.du `compose` curve) .*. dudt .+. (function.dv `compose` curve) .*. dvdt)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (SurfaceFunction units)
    (SurfaceFunction units)
  where
  f `compose` g = do
    let dfdu = f.du `compose` g
    let dfdv = f.dv `compose` g
    let composedDerivative p = do
          let (dudp, dvdp) = VectorSurfaceFunction2d.components (derivative p g)
          dfdu .*. dudp .+. dfdv .*. dvdp
    SurfaceFunction.new
      (f.compiled `compose` g.compiled)
      composedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (VectorSurfaceFunction2d space units)
    (VectorSurfaceFunction2d space units)
  where
  f `compose` g = do
    let dfdu = f.du `compose` g
    let dfdv = f.dv `compose` g
    let composedDerivative p = do
          let (dudp, dvdp) = VectorSurfaceFunction2d.components (derivative p g)
          dfdu .*. dudp .+. dfdv .*. dvdp
    VectorSurfaceFunction2d.new
      (f.compiled `compose` g.compiled)
      composedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (VectorSurfaceFunction3d space units)
    (VectorSurfaceFunction3d space units)
  where
  f `compose` g = do
    let dfdu = f.du `compose` g
    let dfdv = f.dv `compose` g
    let composedDerivative p = do
          let (dudp, dvdp) = VectorSurfaceFunction2d.components (derivative p g)
          dfdu .*. dudp .+. dfdv .*. dvdp
    VectorSurfaceFunction3d.new
      (f.compiled `compose` g.compiled)
      composedDerivative

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (DirectionSurfaceFunction2d space)
    (DirectionSurfaceFunction2d space)
  where
  f `compose` g =
    DirectionSurfaceFunction2d.unsafe (DirectionSurfaceFunction2d.unwrap f `compose` g)

instance
  (uvSpace ~ UvSpace, unitless ~ Unitless) =>
  Composition
    (SurfaceFunction2d uvSpace unitless)
    (DirectionSurfaceFunction3d space)
    (DirectionSurfaceFunction3d space)
  where
  f `compose` g =
    DirectionSurfaceFunction3d.unsafe (DirectionSurfaceFunction3d.unwrap f `compose` g)

distanceAlong ::
  Axis2d space units ->
  SurfaceFunction2d space units ->
  SurfaceFunction units
distanceAlong axis function =
  (function .-. constant (Axis2d.originPoint axis)) `dot` Axis2d.direction axis

xCoordinate :: SurfaceFunction2d space units -> SurfaceFunction units
xCoordinate function = do
  let compiledXCoordinate =
        CompiledFunction.map
          Expression.xCoordinate
          Point2d.xCoordinate
          Bounds2d.xCoordinate
          function.compiled
  SurfaceFunction.new
    compiledXCoordinate
    (\parameter -> VectorSurfaceFunction2d.xComponent (derivative parameter function))

yCoordinate :: SurfaceFunction2d space units -> SurfaceFunction units
yCoordinate function = do
  let compiledYCoordinate =
        CompiledFunction.map
          Expression.yCoordinate
          Point2d.yCoordinate
          Bounds2d.yCoordinate
          function.compiled
  SurfaceFunction.new
    compiledYCoordinate
    (\parameter -> VectorSurfaceFunction2d.yComponent (derivative parameter function))

coordinates :: SurfaceFunction2d space units -> (SurfaceFunction units, SurfaceFunction units)
coordinates function = (xCoordinate function, yCoordinate function)
