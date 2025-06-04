module OpenSolid.VectorCurve2d
  ( VectorCurve2d (compiled, derivative)
  , Compiled
  , new
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
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
  , magnitude
  , unsafeMagnitude
  , reverse
  , isZero
  , hasZero
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
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Orientation2d qualified as Orientation2d
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
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

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    { compiled :: Compiled (space @ units)
    , derivative :: ~(VectorCurve2d (space @ units))
    } ->
    VectorCurve2d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Vector2d coordinateSystem)
    (Bounds Unitless)
    (VectorBounds2d coordinateSystem)

instance FFI (VectorCurve2d (space @ Unitless)) where
  representation = FFI.classRepresentation "VectorCurve2d"

instance FFI (VectorCurve2d (space @ Meters)) where
  representation = FFI.classRepresentation "DisplacementCurve2d"

instance HasUnits (VectorCurve2d (space @ units)) units (VectorCurve2d (space @ Unitless))

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  coerce VectorCurve2d{compiled, derivative} =
    VectorCurve2d
      { compiled = Units.coerce compiled
      , derivative = Units.coerce derivative
      }

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
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Qty units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  curve .*. value = curve .*. Curve.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d (space @ units1)) (Curve units2) (VectorCurve2d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorCurve2d (space @ units1))
    (Curve units2)
    (VectorCurve2d (space @ (units1 :/: units2)))
  where
  lhs ./. rhs =
    recursive
      @ lhs.compiled ./. rhs.compiled
      @ \self -> lhs.derivative ./. rhs - self * (rhs.derivative / rhs)

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
  curve ./. value = curve ./. Curve.constant value

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
  lhs `dot'` rhs =
    Curve.new
      @ lhs.compiled `dot'` rhs.compiled
      @ lhs.derivative `dot'` rhs + lhs `dot'` rhs.derivative

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
    Curve.new
      @ lhs.compiled `cross'` rhs.compiled
      @ lhs.derivative `cross'` rhs + lhs `cross'` rhs.derivative

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
  unitless ~ Unitless =>
  Composition
    (Curve unitless)
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  f . g = new (f.compiled . g.compiled) ((f.derivative . g) * g.derivative)

instance
  unitless ~ Unitless =>
  Composition
    (SurfaceFunction unitless)
    (VectorCurve2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  curve . function =
    VectorSurfaceFunction2d.new
      @ curve.compiled . function.compiled
      @ \p -> (curve.derivative . function) * SurfaceFunction.derivative p function

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
    @ CompiledFunction.concrete (Expression.bezierCurve controlPoints)
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
evaluate VectorCurve2d{compiled} tValue = CompiledFunction.evaluate compiled tValue

evaluateBounds :: VectorCurve2d (space @ units) -> Bounds Unitless -> VectorBounds2d (space @ units)
evaluateBounds VectorCurve2d{compiled} tBounds = CompiledFunction.evaluateBounds compiled tBounds

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

instance
  Units.Squared units1 units2 =>
  HasField "squaredMagnitude" (VectorCurve2d (space @ units1)) (Curve units2)
  where
  getField curve = Units.specialize curve.squaredMagnitude'

instance
  HasField
    "squaredMagnitude'"
    (VectorCurve2d (space @ units))
    (Curve (units :*: units))
  where
  getField curve =
    Curve.new
      @ CompiledFunction.map
        Expression.VectorCurve2d.squaredMagnitude'
        Vector2d.squaredMagnitude'
        VectorBounds2d.squaredMagnitude'
        curve.compiled
      @ 2.0 * curve `dot'` curve.derivative

unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve units
unsafeMagnitude curve =
  Curve.recursive
    @ CompiledFunction.map
      Expression.VectorCurve2d.magnitude
      Vector2d.magnitude
      VectorBounds2d.magnitude
      curve.compiled
    @ \self -> curve.derivative `dot` (curve / self)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZero (Curve units)
magnitude curve =
  case zeros curve of
    Success [] -> Success (unsafeMagnitude curve)
    Success List.OneOrMore -> Failure HasZero
    Failure ZeroEverywhere -> Failure HasZero

isZero :: Tolerance units => VectorCurve2d (space @ units) -> Bool
isZero curve = Tolerance.using Tolerance.squared' (curve.squaredMagnitude' ~= Qty.zero)

hasZero :: Tolerance units => VectorCurve2d (space @ units) -> Bool
hasZero curve = Tolerance.using Tolerance.squared' (curve.squaredMagnitude' ^ Qty.zero)

data ZeroEverywhere = ZeroEverywhere deriving (Eq, Show, Error.Message)

zeros :: Tolerance units => VectorCurve2d (space @ units) -> Result ZeroEverywhere (List Float)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve.zeros curve.squaredMagnitude') of
    Success zeros1d -> Success (List.map (.location) zeros1d)
    Failure Curve.ZeroEverywhere -> Failure ZeroEverywhere

instance HasField "xComponent" (VectorCurve2d (space @ units)) (Curve units) where
  getField curve =
    Curve.new
      @ CompiledFunction.map
        Expression.xComponent
        Vector2d.xComponent
        VectorBounds2d.xComponent
        curve.compiled
      @ curve.derivative.xComponent

instance HasField "yComponent" (VectorCurve2d (space @ units)) (Curve units) where
  getField curve =
    Curve.new
      @ CompiledFunction.map
        Expression.yComponent
        Vector2d.yComponent
        VectorBounds2d.yComponent
        curve.compiled
      @ curve.derivative.yComponent

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
  Orientation2d global (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeIn orientation curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve2d.placeIn orientation)
          (Vector2d.placeIn orientation)
          (VectorBounds2d.placeIn orientation)
          curve.compiled
  new compiledPlaced (placeIn orientation curve.derivative)

relativeTo ::
  Orientation2d global (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeTo orientation = placeIn (Orientation2d.inverse orientation)

on ::
  PlaneOrientation3d space (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve3d (space @ units)
on orientation curve = VectorCurve3d.on orientation curve

convert ::
  Qty (units2 :/: units1) ->
  VectorCurve2d (space @ units1) ->
  VectorCurve2d (space @ units2)
convert factor curve = curve !* factor

unconvert ::
  Qty (units2 :/: units1) ->
  VectorCurve2d (space @ units2) ->
  VectorCurve2d (space @ units1)
unconvert factor curve = curve !/ factor
