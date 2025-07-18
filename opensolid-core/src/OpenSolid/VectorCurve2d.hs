-- Allow typeclass instances to be declared here
-- even though the type is actually defined in the Functions module
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorCurve2d
  ( VectorCurve2d
  , Compiled
  , new
  , compiled
  , derivative
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , xComponent
  , yComponent
  , components
  , zero
  , constant
  , unit
  , xy
  , line
  , arc
  , quadraticSpline
  , cubicSpline
  , bezierCurve
  , synthetic
  , quotient
  , quotient'
  , magnitude
  , squaredMagnitude
  , squaredMagnitude'
  , unsafeMagnitude
  , reverse
  , ZeroEverywhere (ZeroEverywhere)
  , zeros
  , HasZero (HasZero)
  , direction
  , placeIn
  , relativeTo
  , on
  , transformBy
  , rotateBy
  , convert
  , unconvert
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
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import {-# SOURCE #-} OpenSolid.DirectionCurve2d qualified as DirectionCurve2d
import OpenSolid.Error qualified as Error
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Functions (VectorCurve2d (VectorCurve2d))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Plane3d (Plane3d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d.Direction qualified as VectorCurve2d.Direction
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Vector2d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds2d coordinateSystem)

instance HasField "compiled" (VectorCurve2d (space @ units)) (Compiled (space @ units)) where
  getField = compiled

instance HasField "derivative" (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units)) where
  getField = derivative

instance HasField "xComponent" (VectorCurve2d (space @ units)) (Curve units) where
  getField = xComponent

instance HasField "yComponent" (VectorCurve2d (space @ units)) (Curve units) where
  getField = yComponent

instance HasField "components" (VectorCurve2d (space @ units)) (Curve units, Curve units) where
  getField = components

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve2d (space @ units1)) (Curve units2)
  where
  getField = squaredMagnitude

instance
  HasField
    "squaredMagnitude'"
    (VectorCurve2d (space @ units))
    (Curve (units :*: units))
  where
  getField = squaredMagnitude'

instance FFI (VectorCurve2d (space @ Unitless)) where
  representation = FFI.classRepresentation "VectorCurve2d"

instance FFI (VectorCurve2d (space @ Meters)) where
  representation = FFI.classRepresentation "DisplacementCurve2d"

instance HasUnits (VectorCurve2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  coerce curve = VectorCurve2d (Units.coerce curve.compiled) (Units.coerce curve.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    units1
  where
  curve1 ~= curve2 = List.allTrue [evaluate curve1 t ~= evaluate curve2 t | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  ApproximateEquality
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    units1
  where
  curve ~= vector = List.allTrue [evaluate curve t ~= vector | t <- Parameter.samples]

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    units1
  where
  curve ^ vector = Tolerance.using Tolerance.squared' do
    (curve - vector).squaredMagnitude' ^ Qty.zero

instance
  (space1 ~ space2, units1 ~ units2) =>
  Intersects
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    units1
  where
  vector ^ curve = curve ^ vector

instance Negation (VectorCurve2d (space @ units)) where
  negate curve = new (negate curve.compiled) (negate curve.derivative)

instance Multiplication Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units)) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units)) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  lhs + rhs = new (lhs.compiled + rhs.compiled) (lhs.derivative + rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  curve + vector = curve + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  vector + curve = constant vector + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  lhs - rhs = new (lhs.compiled - rhs.compiled) (lhs.derivative - rhs.derivative)

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  curve - vector = curve - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (VectorCurve2d (space1 @ units1))
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    new (lhs.compiled .*. rhs.compiled) (lhs.derivative .*. rhs + lhs .*. rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Qty units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  c1 .*. c2 = Curve.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Curve units2) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Curve units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs =
    new (lhs.compiled .*. rhs.compiled) (lhs.derivative .*. rhs + lhs .*. rhs.derivative)

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorCurve2d (space @ units1))
    (Qty units2)
    (VectorCurve2d (space @ (units1 :/: units2)))
  where
  curve ./. value = Units.simplify (curve .*. (1.0 ./. value))

instance
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Qty units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  curve .*. value = curve .*. Curve.constant value

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  lhs `dot'` rhs = Curve.new do
    #compiled (lhs.compiled `dot'` rhs.compiled)
    #derivative (lhs.derivative `dot'` rhs + lhs `dot'` rhs.derivative)
    #composeCurve (\inner -> (lhs . inner) `dot'` (rhs . inner))
    #composeSurfaceFunction (\inner -> (lhs . inner) `dot'` (rhs . inner))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  curve `dot'` vector = curve `dot'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (VectorCurve2d (space2 @ units2)) (Curve units3)
  where
  lhs `dot` rhs = Units.specialize (lhs `dot'` rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  vector `dot'` curve = constant vector `dot'` curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2d (space1 @ units)) (Direction2d space2) (Curve units)
  where
  lhs `dot` rhs = lhs `dot` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (VectorCurve2d (space2 @ units)) (Curve units)
  where
  lhs `dot` rhs = Vector2d.unit lhs `dot` rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  lhs `cross'` rhs =
    Curve.new do
      #compiled (lhs.compiled `cross'` rhs.compiled)
      #derivative (lhs.derivative `cross'` rhs + lhs `cross'` rhs.derivative)
      #composeCurve (\inner -> (lhs . inner) `cross'` (rhs . inner))
      #composeSurfaceFunction (\inner -> (lhs . inner) `cross'` (rhs . inner))

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  curve `cross'` vector = curve `cross'` constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve units3)
  where
  lhs `cross` rhs = Units.specialize (lhs `cross'` rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  vector `cross'` curve = constant vector `cross'` curve

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2d (space1 @ units)) (Direction2d space2) (Curve units)
  where
  lhs `cross` rhs = lhs `cross` Vector2d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (VectorCurve2d (space2 @ units)) (Curve units)
  where
  lhs `cross` rhs = Vector2d.unit lhs `cross` rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Point2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  point + curve = Curve2d.constant point + curve

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Point2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve2d (space1 @ units1))
  where
  point - curve = Curve2d.constant point - curve

instance
  Composition
    (Curve Unitless)
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  f . g = new (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  Composition
    (SurfaceFunction Unitless)
    (VectorCurve2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  curve . function =
    VectorSurfaceFunction2d.new
      @ curve.compiled . function.compiled
      @ \p -> (curve.derivative . function) * SurfaceFunction.derivative p function

compiled :: VectorCurve2d (space @ units) -> Compiled (space @ units)
compiled (VectorCurve2d c _) = c

derivative :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
derivative (VectorCurve2d _ d) = d

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
transformBy transform curve =
  new
    @ CompiledFunction.map
      (Expression.VectorCurve2d.transformBy transform)
      (Vector2d.transformBy transform)
      (VectorBounds2d.transformBy transform)
      curve.compiled
    @ transformBy transform curve.derivative

rotateBy ::
  forall space units.
  Angle ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
rotateBy angle = transformBy (Transform2d.rotateAround (Point2d.origin @space @units) angle)

new :: Compiled (space @ units) -> VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
new = VectorCurve2d

recursive ::
  Compiled (space @ units) ->
  (VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)) ->
  VectorCurve2d (space @ units)
recursive givenCompiled derivativeFunction =
  let result = new givenCompiled (derivativeFunction result) in result

-- | The constant zero vector.
zero :: VectorCurve2d (space @ units)
zero = constant Vector2d.zero

-- | Create a curve with a constant value.
constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
constant value = new (CompiledFunction.constant value) zero

unit :: DirectionCurve2d space -> VectorCurve2d (space @ Unitless)
unit = DirectionCurve2d.unwrap

-- | Create a curve from its X and Y component curves.
xy :: forall space units. Curve units -> Curve units -> VectorCurve2d (space @ units)
xy x y =
  new
    @ CompiledFunction.map2
      Expression.xy
      Vector2d
      VectorBounds2d
      x.compiled
      y.compiled
    @ xy x.derivative y.derivative

line :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
line v1 v2 = bezierCurve (NonEmpty.two v1 v2)

arc ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  VectorCurve2d (space @ units)
arc v1 v2 a b
  | v1 == Vector2d.zero && v2 == Vector2d.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = do
      let angle = Curve.line a b
      v1 * Curve.cos angle + v2 * Curve.sin angle

quadraticSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
quadraticSpline v1 v2 v3 = bezierCurve (NonEmpty.three v1 v2 v3)

cubicSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
cubicSpline v1 v2 v3 v4 = bezierCurve (NonEmpty.four v1 v2 v3 v4)

bezierCurve :: NonEmpty (Vector2d (space @ units)) -> VectorCurve2d (space @ units)
bezierCurve controlPoints =
  new
    @ CompiledFunction.concrete (Expression.bezierCurve controlPoints Expression.t)
    @ bezierCurve (Bezier.derivative controlPoints)

synthetic ::
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
synthetic curve curveDerivative = new curve.compiled curveDerivative

startValue :: VectorCurve2d (space @ units) -> Vector2d (space @ units)
startValue curve = evaluate curve 0.0

endValue :: VectorCurve2d (space @ units) -> Vector2d (space @ units)
endValue curve = evaluate curve 1.0

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: VectorCurve2d (space @ units) -> Float -> Vector2d (space @ units)
evaluate curve tValue = CompiledFunction.evaluate curve.compiled tValue

evaluateBounds :: VectorCurve2d (space @ units) -> Bounds Unitless -> VectorBounds2d (space @ units)
evaluateBounds curve tBounds = CompiledFunction.evaluateBounds curve.compiled tBounds

-- | Get the X coordinate of a 2D curve as a scalar curve.
xComponent :: VectorCurve2d (space @ units) -> Curve units
xComponent curve =
  Curve.new do
    #compiled do
      CompiledFunction.map
        Expression.xComponent
        Vector2d.xComponent
        VectorBounds2d.xComponent
        curve.compiled
    #derivative curve.derivative.xComponent
    #composeCurve (\inner -> xComponent (curve . inner))
    #composeSurfaceFunction (\inner -> VectorSurfaceFunction2d.xComponent (curve . inner))

-- | Get the Y coordinate of a 2D curve as a scalar curve.
yComponent :: VectorCurve2d (space @ units) -> Curve units
yComponent curve =
  Curve.new do
    #compiled do
      CompiledFunction.map
        Expression.yComponent
        Vector2d.yComponent
        VectorBounds2d.yComponent
        curve.compiled
    #derivative curve.derivative.yComponent
    #composeCurve (\inner -> yComponent (curve . inner))
    #composeSurfaceFunction (\inner -> VectorSurfaceFunction2d.yComponent (curve . inner))

components :: VectorCurve2d (space @ units) -> (Curve units, Curve units)
components curve = (xComponent curve, yComponent curve)

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

quotient ::
  (Units.Quotient units1 units2 units3, Tolerance units2) =>
  VectorCurve2d (space @ units1) ->
  Curve units2 ->
  VectorCurve2d (space @ units3)
quotient lhs rhs = Units.specialize (quotient' lhs rhs)

quotient' ::
  Tolerance units2 =>
  VectorCurve2d (space @ units1) ->
  Curve units2 ->
  VectorCurve2d (space @ (units1 :/: units2))
quotient' lhs rhs =
  recursive
    @ CompiledFunction.map2 Expression.quotient' (./.) (./.) lhs.compiled rhs.compiled
    @ \self -> quotient' lhs.derivative rhs - self * (Curve.quotient rhs.derivative rhs)

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve2d (space @ units) -> Curve (units :*: units)
squaredMagnitude' curve = Curve.new do
  #compiled do
    CompiledFunction.map
      Expression.VectorCurve2d.squaredMagnitude'
      Vector2d.squaredMagnitude'
      VectorBounds2d.squaredMagnitude'
      curve.compiled
  #derivative (2.0 * curve `dot'` curve.derivative)
  #composeCurve (\inner -> squaredMagnitude' (curve . inner))
  #composeSurfaceFunction (\inner -> VectorSurfaceFunction2d.squaredMagnitude' (curve . inner))

unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve units
unsafeMagnitude curve = Curve.recursive \self -> do
  #compiled do
    CompiledFunction.map
      Expression.VectorCurve2d.magnitude
      Vector2d.magnitude
      VectorBounds2d.magnitude
      curve.compiled
  #derivative (curve.derivative `dot` (Tolerance.exactly (quotient curve self)))
  #composeCurve (\inner -> unsafeMagnitude (curve . inner))
  #composeSurfaceFunction do
    \inner -> Tolerance.exactly do
      SurfaceFunction.sqrt' (VectorSurfaceFunction2d.squaredMagnitude' (curve . inner))

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZero (Curve units)
magnitude curve =
  case zeros curve of
    Success [] -> Success (unsafeMagnitude curve)
    Success List.OneOrMore -> Failure HasZero
    Failure ZeroEverywhere -> Failure HasZero

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, Error.Message)

zeros :: Tolerance units => VectorCurve2d (space @ units) -> Result ZeroEverywhere (List Float)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve.zeros curve.squaredMagnitude') of
    Success zeros1d -> Success (List.map (.location) zeros1d)
    Failure Curve.ZeroEverywhere -> Failure ZeroEverywhere

direction ::
  Tolerance units =>
  VectorCurve2d (space @ units) ->
  Result HasZero (DirectionCurve2d space)
direction curve =
  case zeros curve of
    -- If the vector curve has no zeros, then we can safely compute its direction
    Success [] -> Success (VectorCurve2d.Direction.unsafe curve curve.derivative)
    -- Otherwise, check where the vector curve is zero:
    -- if it's only zero at one or both endpoints,
    -- and the curve's *derivative* is non-zero at those endpoints,
    -- then it's still possible to uniquely determine a tangent direction everywhere
    Success (NonEmpty curveZeros) ->
      if NonEmpty.allSatisfy (isRemovableDegeneracy curve.derivative) curveZeros
        then Success (VectorCurve2d.Direction.unsafe curve curve.derivative)
        else Failure HasZero
    -- Definitely can't get the direction of a vector curve
    -- if that vector curve is zero everywhere!
    Failure ZeroEverywhere -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative tValue =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (tValue == 0.0 || tValue == 1.0) && evaluate curveDerivative tValue != Vector2d.zero

placeIn ::
  Frame2d (global @ frameUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeIn frame curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve2d.placeIn frame)
          (Vector2d.placeIn frame)
          (VectorBounds2d.placeIn frame)
          curve.compiled
  new compiledPlaced (placeIn frame curve.derivative)

relativeTo ::
  Frame2d (global @ frameUnits) (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeTo frame = placeIn (Frame2d.inverse frame)

on ::
  Plane3d (space @ planeUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve3d (space @ units)
on plane curve = VectorCurve3d.on plane curve

convert ::
  Qty (units2 :/: units1) ->
  VectorCurve2d (space @ units1) ->
  VectorCurve2d (space @ units2)
convert factor curve = Units.simplify (curve .*. factor)

unconvert ::
  Qty (units2 :/: units1) ->
  VectorCurve2d (space @ units2) ->
  VectorCurve2d (space @ units1)
unconvert factor curve = Units.simplify (curve ./. factor)
