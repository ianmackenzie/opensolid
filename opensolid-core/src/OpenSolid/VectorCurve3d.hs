module OpenSolid.VectorCurve3d
  ( VectorCurve3d
  , Compiled
  , new
  , on
  , compiled
  , derivative
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , zero
  , constant
  , line
  , arc
  , quadraticSpline
  , cubicSpline
  , bezierCurve
  , quotient
  , quotient'
  , magnitude
  , unsafeMagnitude
  , squaredMagnitude
  , squaredMagnitude'
  , reverse
  , ZeroEverywhere (ZeroEverywhere)
  , zeros
  , HasZero (HasZero)
  , direction
  , placeIn
  , relativeTo
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Bounds (Bounds)
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Direction3d (Direction3d)
import {-# SOURCE #-} OpenSolid.DirectionCurve3d (DirectionCurve3d)
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Expression.VectorCurve3d qualified as Expression.VectorCurve3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d (VectorBounds3d)
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve3d.Direction qualified as VectorCurve3d.Direction
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

data VectorCurve3d (coordinateSystem :: CoordinateSystem)
  = VectorCurve3d (Compiled coordinateSystem) ~(VectorCurve3d coordinateSystem)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Vector3d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds3d coordinateSystem)

instance HasField "compiled" (VectorCurve3d (space @ units)) (Compiled (space @ units)) where
  getField = compiled

instance HasField "derivative" (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units)) where
  getField = derivative

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve3d (space @ units1)) (Curve units2)
  where
  getField = squaredMagnitude

instance
  HasField
    "squaredMagnitude'"
    (VectorCurve3d (space @ units))
    (Curve (units :*: units))
  where
  getField = squaredMagnitude'

instance HasUnits (VectorCurve3d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d (space1 @ units1)) (VectorCurve3d (space2 @ units2))
  where
  coerce curve = VectorCurve3d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    units1
  where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    units1
  where
  curve ~= vector = List.allTrue [evaluate curve t ~= vector | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    units1
  where
  curve ^ vector = Tolerance.using Tolerance.squared' do
    (curve - vector).squaredMagnitude' ^ Qty.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    units1
  where
  vector ^ curve = curve ^ vector

instance Negation (VectorCurve3d (space @ units)) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units)) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units)) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  curve + vector = curve + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  vector + curve = constant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  curve - vector = curve - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units1))
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    new
      @ lhs.compiled .*. rhs.compiled
      @ lhs.derivative .*. rhs + lhs .*. rhs.derivative

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  c1 .*. c2 = Curve.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    new
      (lhs.compiled .*. rhs.compiled)
      (lhs.derivative .*. rhs + lhs .*. rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Qty units2) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorCurve3d (space @ units1))
    (Qty units2)
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  curve .*. value = curve .*. Curve.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Qty units2) (VectorCurve3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorCurve3d (space @ units1))
    (Qty units2)
    (VectorCurve3d (space @ (units1 :/: units2)))
  where
  curve ./. value = Units.simplify (curve .*. (1.0 ./. value))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  lhs `dot'` rhs =
    Curve.new
      (lhs.compiled `dot'` rhs.compiled)
      (lhs.derivative `dot'` rhs + lhs `dot'` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve3d (space1 @ units1)) (Vector3d (space2 @ units2)) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  curve `dot'` vector = curve `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d (space1 @ units1)) (VectorCurve3d (space2 @ units2)) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  vector `dot'` curve = constant vector `dot'` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (Direction3d space2) (Curve units)
  where
  lhs `dot` rhs = lhs `dot` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (VectorCurve3d (space2 @ units)) (Curve units)
  where
  lhs `dot` rhs = Vector3d.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 :*: units2)))
  where
  lhs `cross'` rhs =
    new
      (lhs.compiled `cross'` rhs.compiled)
      (lhs.derivative `cross'` rhs + lhs `cross'` rhs.derivative)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 :*: units2)))
  where
  curve `cross'` vector = curve `cross'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 :*: units2)))
  where
  vector `cross'` curve = constant vector `cross'` curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (Direction3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs `cross` rhs = lhs `cross` Vector3d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  lhs `cross` rhs = Vector3d.unit lhs `cross` rhs

instance
  Composition
    (Curve Unitless)
    (VectorCurve3d (space @ units))
    (VectorCurve3d (space @ units))
  where
  f . g = new (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  curve . function =
    VectorSurfaceFunction3d.new
      @ curve.compiled . function.compiled
      @ \p -> curve.derivative . function * SurfaceFunction.derivative p function

compiled :: VectorCurve3d (space @ units) -> Compiled (space @ units)
compiled (VectorCurve3d c _) = c

derivative :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
derivative (VectorCurve3d _ d) = d

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
transformBy transform curve = do
  let compiledTransformed =
        CompiledFunction.map
          (Expression.VectorCurve3d.transformBy transform)
          (Vector3d.transformBy transform)
          (VectorBounds3d.transformBy transform)
          curve.compiled
  new compiledTransformed (transformBy transform curve.derivative)

new :: Compiled (space @ units) -> VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
new = VectorCurve3d

recursive ::
  Compiled (space @ units) ->
  (VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)) ->
  VectorCurve3d (space @ units)
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

zero :: VectorCurve3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
constant value = new (CompiledFunction.constant value) zero

on ::
  Plane3d (space @ planeUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve3d (space @ units)
on plane vectorCurve2d = do
  let compiledPlanar =
        CompiledFunction.map
          (Expression.VectorCurve2d.on plane)
          (Vector2d.on plane)
          (VectorBounds2d.on plane)
          vectorCurve2d.compiled
  new compiledPlanar (on plane vectorCurve2d.derivative)

line :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorCurve3d (space @ units)
line v1 v2 = bezierCurve (NonEmpty.two v1 v2)

arc ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Angle ->
  Angle ->
  VectorCurve3d (space @ units)
arc v1 v2 a b
  | v1 == Vector3d.zero && v2 == Vector3d.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = do
      let angle = Curve.line a b
      v1 * Curve.cos angle + v2 * Curve.sin angle

quadraticSpline ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorCurve3d (space @ units)
quadraticSpline v1 v2 v3 = bezierCurve (NonEmpty.three v1 v2 v3)

cubicSpline ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorCurve3d (space @ units)
cubicSpline v1 v2 v3 v4 = bezierCurve (NonEmpty.four v1 v2 v3 v4)

bezierCurve :: NonEmpty (Vector3d (space @ units)) -> VectorCurve3d (space @ units)
bezierCurve controlPoints =
  new
    (CompiledFunction.concrete (Expression.bezierCurve controlPoints))
    (bezierCurve (Bezier.derivative controlPoints))

startValue :: VectorCurve3d (space @ units) -> Vector3d (space @ units)
startValue curve = evaluate curve 0.0

endValue :: VectorCurve3d (space @ units) -> Vector3d (space @ units)
endValue curve = evaluate curve 1.0

evaluate :: VectorCurve3d (space @ units) -> Float -> Vector3d (space @ units)
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

evaluateBounds :: VectorCurve3d (space @ units) -> Bounds Unitless -> VectorBounds3d (space @ units)
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

reverse :: VectorCurve3d (space @ units) -> VectorCurve3d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  VectorCurve3d (space @ units3)
quotient lhs rhs = Units.specialize (quotient' lhs rhs)

quotient' ::
  Tolerance units2 =>
  VectorCurve3d (space @ units1) ->
  Curve units2 ->
  VectorCurve3d (space @ (units1 :/: units2))
quotient' lhs rhs =
  recursive
    @ CompiledFunction.map2 (./.) (./.) (./.) lhs.compiled rhs.compiled
    @ \self -> quotient' lhs.derivative rhs - self * (Curve.quotient rhs.derivative rhs)

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d (space @ units1) -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve3d (space @ units) -> Curve (units :*: units)
squaredMagnitude' curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3d.squaredMagnitude'
          Vector3d.squaredMagnitude'
          VectorBounds3d.squaredMagnitude'
          curve.compiled
  let squaredMagnitudeDerivative = 2.0 * curve `dot'` curve.derivative
  Curve.new compiledSquaredMagnitude squaredMagnitudeDerivative

unsafeMagnitude :: VectorCurve3d (space @ units) -> Curve units
unsafeMagnitude curve = do
  let compiledMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3d.magnitude
          Vector3d.magnitude
          VectorBounds3d.magnitude
          curve.compiled
  let magnitudeDerivative self = curve.derivative `dot` (Tolerance.exactly (quotient curve self))
  Curve.recursive compiledMagnitude magnitudeDerivative

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve3d (space @ units) -> Result HasZero (Curve units)
magnitude curve =
  case zeros curve of
    Success [] -> Success (unsafeMagnitude curve)
    Success List.OneOrMore -> Failure HasZero
    Failure ZeroEverywhere -> Failure HasZero

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, Error.Message)

zeros :: Tolerance units => VectorCurve3d (space @ units) -> Result ZeroEverywhere (List Float)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve.zeros (squaredMagnitude' curve)) of
    Success zeros1d -> Success (List.map (.location) zeros1d)
    Failure Curve.ZeroEverywhere -> Failure ZeroEverywhere

direction ::
  Tolerance units =>
  VectorCurve3d (space @ units) ->
  Result HasZero (DirectionCurve3d space)
direction curve =
  case zeros curve of
    -- If the vector curve has no zeros, then we can safely compute its direction
    Success [] -> Success (VectorCurve3d.Direction.unsafe curve curve.derivative)
    -- Otherwise, check where the vector curve is zero:
    -- if it's only zero at one or both endpoints,
    -- and the curve's *derivative* is non-zero at those endpoints,
    -- then it's still possible to uniquely determine a tangent direction everywhere
    Success (NonEmpty curveZeros) ->
      if NonEmpty.allSatisfy (isRemovableDegeneracy curve.derivative) curveZeros
        then Success (VectorCurve3d.Direction.unsafe curve curve.derivative)
        else Failure HasZero
    -- Definitely can't get the direction of a vector curve
    -- if that vector curve is zero everywhere!
    Failure ZeroEverywhere -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve3d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative tValue =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (tValue == 0.0 || tValue == 1.0) && evaluate curveDerivative tValue != Vector3d.zero

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorCurve3d (local @ units) ->
  VectorCurve3d (global @ units)
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve3d.placeIn frame)
          (Vector3d.placeIn frame)
          (VectorBounds3d.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  VectorCurve3d (global @ units) ->
  VectorCurve3d (local @ units)
relativeTo frame = placeIn (Frame3d.inverse frame)
