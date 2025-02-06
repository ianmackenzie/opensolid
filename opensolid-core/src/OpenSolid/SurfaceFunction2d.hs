module OpenSolid.SurfaceFunction2d
  ( SurfaceFunction2d (Parametric)
  , Interface (..)
  , new
  , constant
  , uv
  , xy
  , evaluate
  , evaluateBounds
  , derivative
  , signedDistanceAlong
  , xCoordinate
  , yCoordinate
  )
where

import OpenSolid.Arithmetic qualified as Arithmetic
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.Composition
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Surface2d qualified as Expression.Surface2d
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V), UvBounds, UvCoordinates, UvPoint)
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> UvPoint -> Point2d coordinateSystem
  evaluateBoundsImpl :: function -> UvBounds -> Bounds2d coordinateSystem
  derivativeImpl :: SurfaceParameter -> function -> VectorSurfaceFunction2d coordinateSystem
  transformByImpl ::
    Transform2d tag coordinateSystem ->
    function ->
    SurfaceFunction2d coordinateSystem

data SurfaceFunction2d (coordinateSystem :: CoordinateSystem) where
  SurfaceFunction2d ::
    Interface function (space @ units) =>
    function ->
    SurfaceFunction2d (space @ units)
  Coerce ::
    SurfaceFunction2d (space @ units1) ->
    SurfaceFunction2d (space @ units2)
  Parametric ::
    Expression UvPoint (Point2d (space @ units)) ->
    SurfaceFunction2d (space @ units)
  XY ::
    SurfaceFunction units ->
    SurfaceFunction units ->
    SurfaceFunction2d (space @ units)
  Addition ::
    SurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units) ->
    SurfaceFunction2d (space @ units)
  Subtraction ::
    SurfaceFunction2d (space @ units) ->
    VectorSurfaceFunction2d (space @ units) ->
    SurfaceFunction2d (space @ units)
  Transformed ::
    Transform2d.Affine (space @ units) ->
    SurfaceFunction2d (space @ units) ->
    SurfaceFunction2d (space @ units)

deriving instance Show (SurfaceFunction2d (space @ units))

instance HasUnits (SurfaceFunction2d (space @ units)) units (SurfaceFunction2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (SurfaceFunction2d (space1 @ unitsA)) (SurfaceFunction2d (space2 @ unitsB))
  where
  coerce function = case function of
    Parametric expression -> Parametric (Units.coerce expression)
    Coerce f -> Coerce f
    _ -> Coerce function

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  Parametric lhs + VectorSurfaceFunction2d.Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Addition lhs rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Addition
    (SurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  f + v = f + VectorSurfaceFunction2d.constant v

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (VectorSurfaceFunction2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  Parametric lhs - VectorSurfaceFunction2d.Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Subtraction lhs rhs

instance
  ( space1 ~ space2
  , units1 ~ units2
  ) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (SurfaceFunction2d (space1 @ units1))
  where
  f - v = f - VectorSurfaceFunction2d.constant v

instance
  VectorSurfaceFunction2d.Interface
    (Arithmetic.Difference (SurfaceFunction2d (space @ units)) (SurfaceFunction2d (space @ units)))
    (space @ units)
  where
  evaluateImpl (Arithmetic.Difference f1 f2) uvPoint =
    evaluate f1 uvPoint - evaluate f2 uvPoint

  evaluateBoundsImpl (Arithmetic.Difference f1 f2) uvBounds =
    evaluateBounds f1 uvBounds - evaluateBounds f2 uvBounds

  derivativeImpl parameter (Arithmetic.Difference f1 f2) =
    derivative parameter f1 - derivative parameter f2

  transformByImpl transform (Arithmetic.Difference f1 f2) =
    VectorSurfaceFunction2d.new $
      Arithmetic.Difference
        -- Note the slight hack here:
        -- the definition of VectorCurve2d.Interface states that the units of the transform
        -- do *not* have to match the units of the vector curve,
        -- because vectors and vector curves ignore translation
        -- (and the units of the transform are just the units of its translation part).
        -- This would in general mean that we couldn't apply the given transform to a Point2d or Curve2d,
        -- but in this case it's safe since any translation will cancel out
        -- when the point and curve are subtracted from each other.
        (transformBy (Units.coerce transform) f1)
        (transformBy (Units.coerce transform) f2)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (SurfaceFunction2d (space1 @ units1))
    (SurfaceFunction2d (space2 @ units2))
    (VectorSurfaceFunction2d (space1 @ units1))
  where
  Parametric lhs - Parametric rhs = VectorSurfaceFunction2d.Parametric (lhs - rhs)
  lhs - rhs = VectorSurfaceFunction2d.new (Arithmetic.Difference lhs rhs)

new :: Interface function (space @ units) => function -> SurfaceFunction2d (space @ units)
new = SurfaceFunction2d

constant :: Point2d (space @ units) -> SurfaceFunction2d (space @ units)
constant = Parametric . Expression.constant

uv :: SurfaceFunction2d UvCoordinates
uv = xy SurfaceFunction.u SurfaceFunction.v

xy ::
  SurfaceFunction units ->
  SurfaceFunction units ->
  SurfaceFunction2d (space @ units)
xy (SurfaceFunction.Parametric x) (SurfaceFunction.Parametric y) =
  Parametric (Expression.xy x y)
xy x y = XY x y

evaluate :: SurfaceFunction2d (space @ units) -> UvPoint -> Point2d (space @ units)
evaluate function uvPoint = case function of
  SurfaceFunction2d f -> evaluateImpl f uvPoint
  Coerce f -> Units.coerce (evaluate f uvPoint)
  Parametric expression -> Expression.evaluate expression uvPoint
  XY x y ->
    Point2d.xy
      (SurfaceFunction.evaluate x uvPoint)
      (SurfaceFunction.evaluate y uvPoint)
  Addition f1 f2 -> evaluate f1 uvPoint + VectorSurfaceFunction2d.evaluate f2 uvPoint
  Subtraction f1 f2 -> evaluate f1 uvPoint - VectorSurfaceFunction2d.evaluate f2 uvPoint
  Transformed transform f -> Point2d.transformBy transform (evaluate f uvPoint)

evaluateBounds :: SurfaceFunction2d (space @ units) -> UvBounds -> Bounds2d (space @ units)
evaluateBounds function uvBounds = case function of
  SurfaceFunction2d f -> evaluateBoundsImpl f uvBounds
  Coerce f -> Units.coerce (evaluateBounds f uvBounds)
  Parametric expression -> Expression.evaluateBounds expression uvBounds
  XY x y ->
    Bounds2d.xy
      (SurfaceFunction.evaluateBounds x uvBounds)
      (SurfaceFunction.evaluateBounds y uvBounds)
  Addition f1 f2 ->
    evaluateBounds f1 uvBounds + VectorSurfaceFunction2d.evaluateBounds f2 uvBounds
  Subtraction f1 f2 ->
    evaluateBounds f1 uvBounds - VectorSurfaceFunction2d.evaluateBounds f2 uvBounds
  Transformed transform f -> Bounds2d.transformBy transform (evaluateBounds f uvBounds)

derivative ::
  SurfaceParameter ->
  SurfaceFunction2d (space @ units) ->
  VectorSurfaceFunction2d (space @ units)
derivative parameter function = case function of
  SurfaceFunction2d f -> derivativeImpl parameter f
  Coerce f -> Units.coerce (derivative parameter f)
  Parametric expression ->
    VectorSurfaceFunction2d.Parametric (Expression.surfaceDerivative parameter expression)
  XY x y ->
    VectorSurfaceFunction2d.xy
      (SurfaceFunction.derivative parameter x)
      (SurfaceFunction.derivative parameter y)
  Addition f1 f2 -> derivative parameter f1 + VectorSurfaceFunction2d.derivative parameter f2
  Subtraction f1 f2 -> derivative parameter f1 - VectorSurfaceFunction2d.derivative parameter f2
  Transformed transform f -> VectorSurfaceFunction2d.transformBy transform (derivative parameter f)

transformBy ::
  Transform2d tag (space @ units) ->
  SurfaceFunction2d (space @ units) ->
  SurfaceFunction2d (space @ units)
transformBy transform function = case function of
  SurfaceFunction2d f -> transformByImpl transform f
  Coerce c -> Units.coerce (transformBy (Units.coerce transform) c)
  Parametric expression -> Parametric (Expression.Surface2d.transformBy transform expression)
  XY{} -> Transformed (Transform2d.toAffine transform) function
  Addition f1 f2 -> transformBy transform f1 + VectorSurfaceFunction2d.transformBy transform f2
  Subtraction c v -> transformBy transform c - VectorSurfaceFunction2d.transformBy transform v
  Transformed current f -> Transformed (Transform2d.toAffine transform . current) f

instance
  uvCoordinates ~ UvCoordinates =>
  Composition
    (Curve2d uvCoordinates)
    (SurfaceFunction2d (space @ units))
    (Curve2d (space @ units))
  where
  Parametric function . Curve2d.Parametric curve = Curve2d.Parametric (function . curve)
  function . curve = Curve2d.new (function :.: curve)

instance
  uvCoordinates ~ UvCoordinates =>
  Curve2d.Interface
    (SurfaceFunction2d (space @ units) :.: Curve2d uvCoordinates)
    (space @ units)
  where
  evaluateImpl (function :.: curve) tValue =
    evaluate function (Curve2d.evaluate curve tValue)

  evaluateBoundsImpl (function :.: curve) tRange =
    evaluateBounds function (Curve2d.evaluateBounds curve tRange)

  boundsImpl (function :.: curve) =
    evaluateBounds function (Curve2d.bounds curve)

  derivativeImpl (function :.: curve) = do
    let curveDerivative = Curve2d.derivative curve
    let dudt = VectorCurve2d.xComponent curveDerivative
    let dvdt = VectorCurve2d.yComponent curveDerivative
    (derivative U function . curve) * dudt + (derivative V function . curve) * dvdt

  reverseImpl (function :.: curve) =
    Curve2d.new (function :.: Curve2d.reverse curve)

  transformByImpl transform (function :.: curve) =
    transformBy transform function . curve

signedDistanceAlong ::
  Axis2d (space @ units) ->
  SurfaceFunction2d (space @ units) ->
  SurfaceFunction units
signedDistanceAlong axis function =
  (function - constant (Axis2d.originPoint axis)) <> Axis2d.direction axis

xCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
xCoordinate = signedDistanceAlong Axis2d.x

yCoordinate :: SurfaceFunction2d (space @ units) -> SurfaceFunction units
yCoordinate = signedDistanceAlong Axis2d.y
