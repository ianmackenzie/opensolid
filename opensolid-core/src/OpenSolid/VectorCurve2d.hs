module OpenSolid.VectorCurve2d
  ( VectorCurve2d
  , Compiled
  , new
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , compiled
  , derivative
  , zero
  , constant
  , parametric
  , xy
  , line
  , arc
  , quadraticSpline
  , cubicSpline
  , bezierCurve
  , synthetic
  , magnitude
  , unsafeMagnitude
  , squaredMagnitude
  , squaredMagnitude'
  , reverse
  , isZero
  , hasZero
  , zeros
  , HasZero (HasZero)
  , xComponent
  , yComponent
  , direction
  , placeIn
  , relativeTo
  , placeOn
  , transformBy
  , rotateBy
  , convert
  , unconvert
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Bezier qualified as Bezier
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve.Zeros qualified as Curve.Zeros
import {-# SOURCE #-} OpenSolid.Curve2d (Curve2d)
import {-# SOURCE #-} OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import {-# SOURCE #-} OpenSolid.SurfaceFunction (SurfaceFunction)
import {-# SOURCE #-} OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Text qualified as Text
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (Meters)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d.Direction qualified as VectorCurve2d.Direction
import OpenSolid.VectorCurve2d.Zeros qualified as Zeros
import {-# SOURCE #-} OpenSolid.VectorCurve3d (VectorCurve3d)
import {-# SOURCE #-} OpenSolid.VectorCurve3d qualified as VectorCurve3d
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import {-# SOURCE #-} OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d
import Prelude qualified

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
    (Range Unitless)
    (VectorBounds2d coordinateSystem)

-- TODO remove
instance Show (VectorCurve2d (space @ units)) where
  show _ = Text.unpack "VectorCurve2d"

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
  negate curve = new (negate (compiled curve)) (negate (derivative curve))

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
  lhs + rhs = new (compiled lhs + compiled rhs) (derivative lhs + derivative rhs)

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
  lhs - rhs = new (compiled lhs - compiled rhs) (derivative lhs - derivative rhs)

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
    new
      (Curve.compiled lhs .*. compiled rhs)
      (Curve.derivative lhs .*. rhs + lhs .*. derivative rhs)

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
    new
      (compiled lhs .*. Curve.compiled rhs)
      (derivative lhs .*. rhs + lhs .*. Curve.derivative rhs)

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
      (compiled lhs ./. Curve.compiled rhs)
      (\self -> derivative lhs ./. rhs - self * (Curve.derivative rhs / rhs))

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
      (compiled lhs `dot'` compiled rhs)
      (derivative lhs `dot'` rhs + lhs `dot'` derivative rhs)

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
      (compiled lhs `cross'` compiled rhs)
      (derivative lhs `cross'` rhs + lhs `cross'` derivative rhs)

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
  f . g = new (compiled f . Curve.compiled g) ((derivative f . g) * Curve.derivative g)

instance
  unitless ~ Unitless =>
  Composition
    (SurfaceFunction unitless)
    (VectorCurve2d (space @ units))
    (VectorSurfaceFunction2d (space @ units))
  where
  curve . function = VectorSurfaceFunction2d.new (curve :.: function)

instance
  unitless ~ Unitless =>
  VectorSurfaceFunction2d.Interface
    (VectorCurve2d (space @ units) :.: SurfaceFunction unitless)
    (space @ units)
  where
  evaluateImpl (curve :.: function) uvPoint =
    evaluate curve (SurfaceFunction.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    evaluateBounds curve (SurfaceFunction.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (derivative curve . function) * SurfaceFunction.derivative parameter function

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
transformBy transform curve =
  new
    & CompiledFunction.map
      (Expression.VectorCurve2d.transformBy transform)
      (Vector2d.transformBy transform)
      (VectorBounds2d.transformBy transform)
      (compiled curve)
    & transformBy transform (derivative curve)

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

-- TODO remove
parametric :: Expression Float (Vector2d (space @ units)) -> VectorCurve2d (space @ units)
parametric expression =
  new (CompiledFunction.concrete expression) (parametric (Expression.curveDerivative expression))

-- | Create a curve from its X and Y component curves.
xy :: forall space units. Curve units -> Curve units -> VectorCurve2d (space @ units)
xy x y =
  new
    & CompiledFunction.map2
      Expression.xy
      Vector2d
      VectorBounds2d
      (Curve.compiled x)
      (Curve.compiled y)
    & xy (Curve.derivative x) (Curve.derivative y)

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
    & CompiledFunction.concrete (Expression.bezierCurve controlPoints)
    & bezierCurve (Bezier.derivative controlPoints)

synthetic ::
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
synthetic curve curveDerivative = new (compiled curve) curveDerivative

startValue :: VectorCurve2d (space @ units) -> Vector2d (space @ units)
startValue curve = evaluate curve 0.0

endValue :: VectorCurve2d (space @ units) -> Vector2d (space @ units)
endValue curve = evaluate curve 1.0

{-| Evaluate a curve at a given parameter value.

The parameter value should be between 0 and 1.
-}
evaluate :: VectorCurve2d (space @ units) -> Float -> Vector2d (space @ units)
evaluate VectorCurve2d{compiled} tValue = CompiledFunction.evaluate compiled tValue

evaluateBounds :: VectorCurve2d (space @ units) -> Range Unitless -> VectorBounds2d (space @ units)
evaluateBounds VectorCurve2d{compiled} tRange = CompiledFunction.evaluateBounds compiled tRange

reverse :: VectorCurve2d (space @ units) -> VectorCurve2d (space @ units)
reverse curve = curve . (1.0 - Curve.t)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve2d (space @ units) -> Curve (units :*: units)
squaredMagnitude' curve =
  Curve.new
    & CompiledFunction.map
      Expression.VectorCurve2d.squaredMagnitude'
      Vector2d.squaredMagnitude'
      VectorBounds2d.squaredMagnitude'
      (compiled curve)
    & 2.0 * curve `dot'` derivative curve

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (VectorCurve2d coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve units
unsafeMagnitude curve =
  Curve.recursive
    & CompiledFunction.map
      Expression.VectorCurve2d.magnitude
      Vector2d.magnitude
      VectorBounds2d.magnitude
      (compiled curve)
    & \self -> derivative curve `dot` (curve / self)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZero (Curve units)
magnitude curve =
  case zeros curve of
    Success [] -> Success (unsafeMagnitude curve)
    Success List.OneOrMore -> Failure HasZero
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    Failure Zeros.HigherOrderZero -> Failure HasZero

isZero :: Tolerance units => VectorCurve2d (space @ units) -> Bool
isZero curve = Tolerance.using Tolerance.squared' (squaredMagnitude' curve ~= Qty.zero)

hasZero :: Tolerance units => VectorCurve2d (space @ units) -> Bool
hasZero curve = Tolerance.using Tolerance.squared' (squaredMagnitude' curve ^ Qty.zero)

zeros :: Tolerance units => VectorCurve2d (space @ units) -> Result Zeros.Error (List Float)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve.zeros (squaredMagnitude' curve)) of
    Success zeros1d -> Success (List.map Curve.Zero.location zeros1d)
    Failure Curve.Zeros.ZeroEverywhere -> Failure Zeros.ZeroEverywhere
    Failure Curve.Zeros.HigherOrderZero -> Failure Zeros.HigherOrderZero

xComponent :: VectorCurve2d (space @ units) -> Curve units
xComponent curve = curve `dot` Direction2d.x

yComponent :: VectorCurve2d (space @ units) -> Curve units
yComponent curve = curve `dot` Direction2d.y

direction ::
  Tolerance units =>
  VectorCurve2d (space @ units) ->
  Result HasZero (DirectionCurve2d space)
direction curve =
  case zeros curve of
    -- If the vector curve has no zeros, then we can safely compute its direction
    Success [] -> Success (VectorCurve2d.Direction.unsafe curve (derivative curve))
    -- Otherwise, check where the vector curve is zero:
    -- if it's only zero at one or both endpoints,
    -- and the curve's *derivative* is non-zero at those endpoints,
    -- then it's still possible to uniquely determine a tangent direction everywhere
    Success (NonEmpty curveZeros) -> do
      let curveDerivative = derivative curve
      if NonEmpty.allSatisfy (isRemovableDegeneracy curveDerivative) curveZeros
        then Success (VectorCurve2d.Direction.unsafe curve curveDerivative)
        else Failure HasZero
    -- Definitely can't get the direction of a vector curve
    -- if that vector curve is zero everywhere!
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    -- If a curve has a higher-order zero, that still means it has a zero...
    Failure Zeros.HigherOrderZero -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative tValue =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (tValue == 0.0 || tValue == 1.0) && evaluate curveDerivative tValue != Vector2d.zero

placeIn ::
  Basis2d global (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeIn basis curve = do
  let compiledPlaced =
        CompiledFunction.map
          (Expression.VectorCurve2d.placeIn basis)
          (Vector2d.placeIn basis)
          (VectorBounds2d.placeIn basis)
          (compiled curve)
  new compiledPlaced (placeIn basis (derivative curve))

relativeTo ::
  Basis2d global (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeTo basis = placeIn (Basis2d.inverse basis)

placeOn ::
  PlanarBasis3d space (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve3d (space @ units)
placeOn basis curve = VectorCurve3d.planar basis curve

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
