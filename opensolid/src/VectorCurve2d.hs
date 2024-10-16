-- Needed for 'Curve1d * Vector2d = VectorCurve2d'
-- and 'Vector2d * Curve1d = VectorCurve2d' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Curve1d or Vector2d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorCurve2d
  ( VectorCurve2d (Constant)
  , Interface (..)
  , new
  , evaluateAt
  , segmentBounds
  , derivative
  , zero
  , constant
  , xy
  , line
  , arc
  , quadraticSpline
  , cubicSpline
  , bezierCurve
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
  , placeInBasis
  , relativeToBasis
  , transformBy
  , rotateBy
  , expression
  )
where

import Angle qualified
import Arithmetic.Unboxed
import Basis2d (Basis2d)
import Basis2d qualified
import Composition qualified
import CoordinateSystem (Space)
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Root qualified
import Curve1d.Zeros qualified
import Direction2d (Direction2d)
import Direction2d qualified
import {-# SOURCE #-} DirectionCurve2d (DirectionCurve2d)
import Error qualified
import Function (Function)
import Function qualified
import Float qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Function.VectorCurve2d qualified
import List qualified
import Maybe qualified
import NonEmpty qualified
import OpenSolid
import Point2d qualified
import Qty (Qty (Qty#))
import Qty qualified
import Range (Range (Range))
import Range qualified
import Tolerance qualified
import Transform2d (Transform2d)
import Transform2d qualified
import Units qualified
import Vector2d (Vector2d (Vector2d#))
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))
import VectorBounds2d qualified
import VectorCurve2d.Direction qualified
import VectorCurve2d.Zeros qualified as Zeros

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateAtImpl :: Float -> curve -> Vector2d coordinateSystem
  segmentBoundsImpl :: Range Unitless -> curve -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    curve ->
    VectorCurve2d coordinateSystem
  expressionImpl :: curve -> Maybe (Function Float (Vector2d coordinateSystem))

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    Interface curve (space @ units) =>
    curve ->
    VectorCurve2d (space @ units)
  Constant ::
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  Coerce ::
    VectorCurve2d (space @ units1) ->
    VectorCurve2d (space @ units2)
  Reversed ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  XY ::
    Curve1d units ->
    Curve1d units ->
    VectorCurve2d (space @ units)
  Negated ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  Sum ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  Difference ::
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)
  Product1d2d' ::
    Curve1d units1 ->
    VectorCurve2d (space @ units2) ->
    VectorCurve2d (space @ (units1 :*: units2))
  Product2d1d' ::
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ (units1 :*: units2))
  Quotient' ::
    VectorCurve2d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve2d (space @ (units1 :/: units2))
  PlaceInBasis ::
    Basis2d global (Defines local) ->
    VectorCurve2d (local @ units) ->
    VectorCurve2d (global @ units)
  Line ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  Arc ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Angle ->
    Angle ->
    VectorCurve2d (space @ units)
  QuadraticSpline ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  CubicSpline ::
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    Vector2d (space @ units) ->
    VectorCurve2d (space @ units)
  BezierCurve ::
    NonEmpty (Vector2d (space @ units)) ->
    VectorCurve2d (space @ units)
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)

deriving instance Show (VectorCurve2d (space @ units))

instance HasUnits (VectorCurve2d (space @ units)) where
  type UnitsOf (VectorCurve2d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ unitsA)) (VectorCurve2d (space2 @ unitsB))
  where
  coerce (Constant value) = Constant (Units.coerce value)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance Interface (VectorCurve2d (space @ units)) (space @ units) where
  evaluateAtImpl = evaluateAt
  segmentBoundsImpl = segmentBounds
  derivativeImpl = derivative
  transformByImpl = transformBy
  expressionImpl = expression

expression :: VectorCurve2d (space @ units) -> Maybe (Function Float (Vector2d (space @ units)))
expression vectorCurve = case vectorCurve of
  VectorCurve2d v -> expressionImpl v
  Constant v -> Just (Function.VectorCurve2d.constant v)
  Coerce c -> Units.coerce (expression c)
  Reversed c -> Maybe.map (. (1.0 - Function.parameter)) (expression c)
  XY x y -> Maybe.map2 Function.VectorCurve2d.xy (Curve1d.expression x) (Curve1d.expression y)
  Negated c -> Maybe.map negate (expression c)
  Sum c1 c2 -> Maybe.map2 (+) (expression c1) (expression c2)
  Difference c1 c2 -> Maybe.map2 (-) (expression c1) (expression c2)
  Product1d2d' c1 c2 -> Maybe.map2 (.*.) (Curve1d.expression c1) (expression c2)
  Product2d1d' c1 c2 -> Maybe.map2 (.*.) (expression c1) (Curve1d.expression c2)
  Quotient' c1 c2 -> Maybe.map2 (./.) (expression c1) (Curve1d.expression c2)
  PlaceInBasis basis c -> Maybe.map (Function.VectorCurve2d.placeInBasis basis) (expression c)
  Line v1 v2 -> Just (v1 + Function.parameter * (v2 - v1))
  Arc i j a b -> do
    let angle = a + Function.parameter * (b - a)
    Just (i * Function.cos angle + j * Function.sin angle)
  QuadraticSpline v1 v2 v3 -> Just (bezierCurveExpression (NonEmpty.of3 v1 v2 v3))
  CubicSpline v1 v2 v3 v4 -> Just (bezierCurveExpression (NonEmpty.of4 v1 v2 v3 v4))
  BezierCurve controlPoints -> Just (bezierCurveExpression controlPoints)
  Transformed transform c -> Maybe.map (Function.VectorCurve2d.transformBy transform) (expression c)

bezierCurveExpression :: NonEmpty (Vector2d (space @ units)) -> Function Float (Vector2d (space @ units))
bezierCurveExpression = bezierCurveExpressionImpl . NonEmpty.map Function.VectorCurve2d.constant

bezierCurveExpressionImpl ::
  NonEmpty (Function Float (Vector2d (space @ units))) ->
  Function Float (Vector2d (space @ units))
bezierCurveExpressionImpl controlPoints = case controlPoints of
  point :| [] -> point
  _ :| NonEmpty rest -> bezierCurveExpressionImpl (NonEmpty.map2 collapseControlPoints controlPoints rest)

collapseControlPoints ::
  Function Float (Vector2d (space @ units)) ->
  Function Float (Vector2d (space @ units)) ->
  Function Float (Vector2d (space @ units))
collapseControlPoints p1 p2 = Function.VectorCurve2d.interpolateFrom p1 p2 Function.parameter

instance Negation (VectorCurve2d (space @ units)) where
  negate curve = case curve of
    Constant value -> Constant -value
    Coerce c -> Coerce -c
    XY x y -> XY -x -y
    Negated c -> c
    Difference c1 c2 -> Difference c2 c1
    Product1d2d' c1 c2 -> Product1d2d' -c1 c2
    Product2d1d' c1 c2 -> Product2d1d' c1 -c2
    _ -> Negated curve

instance Multiplication Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

instance Multiplication' Sign (VectorCurve2d (space @ units)) where
  type Sign .*. VectorCurve2d (space @ units) = VectorCurve2d (space @ (Unitless :*: units))
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Multiplication (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units))

instance Multiplication' (VectorCurve2d (space @ units)) Sign where
  type VectorCurve2d (space @ units) .*. Sign = VectorCurve2d (space @ (units :*: Unitless))
  curve .*. Positive = Units.coerce curve
  curve .*. Negative = Units.coerce -curve

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 + c2 = Sum c1 c2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorCurve2d (space @ units))
    (Vector2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  curve + vector = curve + constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  vector + curve = constant vector + curve

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  -- TODO add special cases
  c1 - c2 = Difference c1 c2

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorCurve2d (space @ units))
    (Vector2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  curve - vector = curve - constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector2d (space @ units))
    (VectorCurve2d (space_ @ units_))
    (VectorCurve2d (space @ units))
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication' (Curve1d units1) (VectorCurve2d (space @ units2)) where
  type
    Curve1d units1 .*. VectorCurve2d (space @ units2) =
      VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Product1d2d' c1 c2 -- TODO add special cases

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorCurve2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication' (Qty units1) (VectorCurve2d (space @ units2)) where
  type
    Qty units1 .*. VectorCurve2d (space @ units2) =
      VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Curve1d.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Vector2d (space @ units2)) (VectorCurve2d (space @ units3))

instance Multiplication' (Curve1d units1) (Vector2d (space @ units2)) where
  type
    Curve1d units1 .*. Vector2d (space @ units2) =
      VectorCurve2d (space @ (units1 :*: units2))
  curve .*. vector = curve .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Multiplication' (VectorCurve2d (space @ units1)) (Curve1d units2) where
  type
    VectorCurve2d (space @ units1) .*. Curve1d units2 =
      VectorCurve2d (space @ (units1 :*: units2))
  c1 .*. c2 = Product2d1d' c1 c2 -- TODO add special cases

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))

instance Multiplication' (VectorCurve2d (space @ units1)) (Qty units2) where
  type
    VectorCurve2d (space @ units1) .*. Qty units2 =
      VectorCurve2d (space @ (units1 :*: units2))
  curve .*. value = curve .*. Curve1d.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Multiplication' (Vector2d (space @ units1)) (Curve1d units2) where
  type
    Vector2d (space @ units1) .*. Curve1d units2 =
      VectorCurve2d (space @ (units1 :*: units2))
  vector .*. curve = constant vector .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))

instance Division' (VectorCurve2d (space @ units1)) (Curve1d units2) where
  type
    VectorCurve2d (space @ units1) ./. Curve1d units2 =
      VectorCurve2d (space @ (units1 :/: units2))
  c1 ./. c2 = Quotient' c1 c2 -- TODO add special cases

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d (space @ units1)) (Qty units2) (VectorCurve2d (space @ units3))

instance Division' (VectorCurve2d (space @ units1)) (Qty units2) where
  type
    VectorCurve2d (space @ units1) ./. Qty units2 =
      VectorCurve2d (space @ (units1 :/: units2))
  curve ./. value = curve ./. Curve1d.constant value

instance Division' (VectorCurve2d (space @ units)) Int where
  type VectorCurve2d (space @ units) ./. Int = VectorCurve2d (space @ (units :/: Unitless))
  curve ./. value = curve ./. Float.int value

instance Division (VectorCurve2d (space @ units)) Int (VectorCurve2d (space @ units))

instance Multiplication' (VectorCurve2d (space @ units)) Int where
  type VectorCurve2d (space @ units) .*. Int = VectorCurve2d (space @ (units :*: Unitless))
  curve .*. scale = curve .*. Float.int scale

instance Multiplication' Int (VectorCurve2d (space @ units)) where
  type Int .*. VectorCurve2d (space @ units) = VectorCurve2d (space @ (Unitless :*: units))
  scale .*. curve = Float.int scale .*. curve

instance Multiplication (VectorCurve2d (space @ units)) Int (VectorCurve2d (space @ units))

instance Multiplication Int (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units))

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))
  deriving (Show)

instance Curve1d.Interface (DotProductOf space units1 units2) (units1 :*: units2) where
  pointOnImpl (DotProductOf c1 c2) t = evaluateAt t c1 .<>. evaluateAt t c2
  segmentBoundsImpl (DotProductOf c1 c2) t = segmentBounds t c1 .<>. segmentBounds t c2
  derivativeImpl (DotProductOf c1 c2) = derivative c1 .<>. c2 + c1 .<>. derivative c2
  expressionImpl (DotProductOf c1 c2) = Maybe.map2 (.<>.) (expression c1) (expression c2)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  type
    VectorCurve2d (space1 @ units1) .<>. VectorCurve2d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  curve1 .<>. curve2 = Curve1d.new (DotProductOf curve1 curve2) -- TODO add special cases

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Curve1d units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve2d (space1 @ units1)) (Vector2d (space2 @ units2))
  where
  type
    VectorCurve2d (space1 @ units1) .<>. Vector2d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  curve .<>. vector = curve .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (VectorCurve2d (space2 @ units2)) (Curve1d units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  type
    Vector2d (space1 @ units1) .<>. VectorCurve2d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  vector .<>. curve = constant vector .<>. curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2d (space1 @ units)) (Direction2d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve2d (space1 @ units)) (Direction2d space2)
  where
  type VectorCurve2d (space1 @ units) .<>. Direction2d space2 = Curve1d (units :*: Unitless)
  curve .<>. direction2d = curve .<>. Vector2d.unit direction2d

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (VectorCurve2d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (VectorCurve2d (space2 @ units))
  where
  type Direction2d space1 .<>. VectorCurve2d (space2 @ units) = Curve1d (Unitless :*: units)
  direction2d .<>. curve = Vector2d.unit direction2d .<>. curve

data CrossProductOf space units1 units2
  = CrossProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))
  deriving (Show)

instance Curve1d.Interface (CrossProductOf space units1 units2) (units1 :*: units2) where
  pointOnImpl (CrossProductOf c1 c2) t = evaluateAt t c1 .><. evaluateAt t c2
  segmentBoundsImpl (CrossProductOf c1 c2) t = segmentBounds t c1 .><. segmentBounds t c2
  derivativeImpl (CrossProductOf c1 c2) = derivative c1 .><. c2 + c1 .><. derivative c2
  expressionImpl (CrossProductOf c1 c2) = Maybe.map2 (.><.) (expression c1) (expression c2)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  type
    VectorCurve2d (space1 @ units1) .><. VectorCurve2d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  curve1 .><. curve2 = Curve1d.new (CrossProductOf curve1 curve2) -- TODO add special cases

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve1d units3)

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve2d (space1 @ units1)) (Vector2d (space2 @ units2))
  where
  type
    VectorCurve2d (space1 @ units1) .><. Vector2d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  curve .><. vector = curve .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector2d (space1 @ units1)) (VectorCurve2d (space2 @ units2))
  where
  type
    Vector2d (space1 @ units1) .><. VectorCurve2d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  vector .><. curve = constant vector .><. curve

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2d (space1 @ units)) (Direction2d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve2d (space1 @ units)) (Direction2d space2)
  where
  type VectorCurve2d (space1 @ units) .><. Direction2d space2 = Curve1d (units :*: Unitless)
  curve .><. direction2d = curve .><. Vector2d.unit direction2d

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (VectorCurve2d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (VectorCurve2d (space2 @ units))
  where
  type Direction2d space1 .><. VectorCurve2d (space2 @ units) = Curve1d (Unitless :*: units)
  direction2d .><. curve = Vector2d.unit direction2d .><. curve

instance
  Composition
    (Curve1d Unitless)
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  curve1d >> curve2d = VectorCurve2d.new (Composition.Of curve1d curve2d)

instance
  VectorCurve2d.Interface
    (Composition.Of (Curve1d Unitless) (VectorCurve2d (space @ units)))
    (space @ units)
  where
  evaluateAtImpl t (Composition.Of curve1d vectorCurve2d) =
    evaluateAt (Curve1d.pointOn curve1d t) vectorCurve2d
  segmentBoundsImpl t (Composition.Of curve1d vectorCurve2d) =
    segmentBounds (Curve1d.segmentBounds curve1d t) vectorCurve2d
  derivativeImpl (Composition.Of curve1d vectorCurve2d) =
    (derivative vectorCurve2d . curve1d) * Curve1d.derivative curve1d
  transformByImpl transform (Composition.Of curve1d vectorCurve2d) =
    new (Composition.Of curve1d (transformBy transform vectorCurve2d))
  expressionImpl (Composition.Of curve1d vectorCurve2d) =
    Maybe.map2 (.) (expression vectorCurve2d) (Curve1d.expression curve1d)

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
transformBy transform curve = do
  let t = Units.erase (Transform2d.toAffine transform)
  case curve of
    VectorCurve2d c -> VectorCurve2d (transformByImpl transform c)
    Constant v -> Constant (Vector2d.transformBy t v)
    Coerce c -> Coerce (VectorCurve2d.transformBy transform c)
    Reversed c -> Reversed (transformBy transform c)
    XY _ _ -> Transformed t curve
    Negated c -> Negated (transformBy transform c)
    Sum c1 c2 -> Sum (transformBy transform c1) (transformBy transform c2)
    Difference c1 c2 -> Difference (transformBy transform c1) (transformBy transform c2)
    Product1d2d' curve1d curve2d -> Product1d2d' curve1d (transformBy transform curve2d)
    Product2d1d' curve2d curve1d -> Product2d1d' (transformBy transform curve2d) curve1d
    Quotient' curve2d curve1d -> Quotient' (transformBy transform curve2d) curve1d
    PlaceInBasis basis c -> do
      let localTransform = Transform2d.relativeTo (Frame2d.at Point2d.origin basis) transform
      PlaceInBasis basis (transformBy localTransform c)
    Line v1 v2 -> Line (Vector2d.transformBy t v1) (Vector2d.transformBy t v2)
    Arc vx vy a b -> Arc (Vector2d.transformBy t vx) (Vector2d.transformBy t vy) a b
    QuadraticSpline v1 v2 v3 ->
      QuadraticSpline
        (Vector2d.transformBy t v1)
        (Vector2d.transformBy t v2)
        (Vector2d.transformBy t v3)
    CubicSpline v1 v2 v3 v4 ->
      CubicSpline
        (Vector2d.transformBy t v1)
        (Vector2d.transformBy t v2)
        (Vector2d.transformBy t v3)
        (Vector2d.transformBy t v4)
    BezierCurve controlVectors ->
      BezierCurve (NonEmpty.map (Vector2d.transformBy t) controlVectors)
    Transformed existing c -> Transformed (existing >> t) c

rotateBy ::
  forall space units.
  Angle ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
rotateBy angle = transformBy (Transform2d.rotateAround (Point2d.origin @space @units) angle)

new :: Interface curve (space @ units) => curve -> VectorCurve2d (space @ units)
new = VectorCurve2d

zero :: VectorCurve2d (space @ units)
zero = constant Vector2d.zero

constant :: Vector2d (space @ units) -> VectorCurve2d (space @ units)
constant = Constant

xy :: Curve1d units -> Curve1d units -> VectorCurve2d (space @ units)
xy = XY

line :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
line v1 v2 = if v1 == v2 then Constant v1 else Line v1 v2

arc ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  VectorCurve2d (space @ units)
arc v1 v2 a b
  | v1 == Vector2d.zero && v2 == Vector2d.zero = zero
  | a == b = constant (Angle.cos a * v1 + Angle.sin a * v2)
  | otherwise = Arc v1 v2 a b

quadraticSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
quadraticSpline = QuadraticSpline

cubicSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
cubicSpline = CubicSpline

bezierCurve :: NonEmpty (Vector2d (space @ units)) -> VectorCurve2d (space @ units)
bezierCurve = BezierCurve

quadraticBlossom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Float ->
  Vector2d (space @ units)
quadraticBlossom v1 v2 v3 t1 t2 = do
  let !(Vector2d# x1# y1#) = v1
  let !(Vector2d# x2# y2#) = v2
  let !(Vector2d# x3# y3#) = v3
  let !(Qty# t1#) = t1
  let !(Qty# t2#) = t2
  let r1# = 1.0## -# t1#
  let r2# = 1.0## -# t2#
  let s1# = r1# *# r2#
  let s2# = r1# *# t2# +# t1# *# r2#
  let s3# = t1# *# t2#
  let x# = s1# *# x1# +# s2# *# x2# +# s3# *# x3#
  let y# = s1# *# y1# +# s2# *# y2# +# s3# *# y3#
  Vector2d# x# y#

cubicBlossom ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Float ->
  Float ->
  Float ->
  Vector2d (space @ units)
cubicBlossom v1 v2 v3 v4 t1 t2 t3 = do
  let !(Vector2d# x1# y1#) = v1
  let !(Vector2d# x2# y2#) = v2
  let !(Vector2d# x3# y3#) = v3
  let !(Vector2d# x4# y4#) = v4
  let !(Qty# t1#) = t1
  let !(Qty# t2#) = t2
  let !(Qty# t3#) = t3
  let r1# = 1.0## -# t1#
  let r2# = 1.0## -# t2#
  let r3# = 1.0## -# t3#
  let s1# = r1# *# r2# *# r3#
  let s2# = r1# *# r2# *# t3# +# r1# *# t2# *# r3# +# t1# *# r2# *# r3#
  let s3# = t1# *# t2# *# r3# +# t1# *# r2# *# t3# +# r1# *# t2# *# t3#
  let s4# = t1# *# t2# *# t3#
  let x# = s1# *# x1# +# s2# *# x2# +# s3# *# x3# +# s4# *# x4#
  let y# = s1# *# y1# +# s2# *# y2# +# s3# *# y3# +# s4# *# y4#
  Vector2d# x# y#

deCasteljau :: Float -> NonEmpty (Vector2d (space @ units)) -> Vector2d (space @ units)
deCasteljau _ (vector :| []) = vector
deCasteljau t (v1 :| v2 : rest) = deCasteljau t (deCasteljauStep t v1 v2 rest)

deCasteljauStep ::
  Float ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
deCasteljauStep t v1 v2 rest = do
  let vector = Vector2d.interpolateFrom v1 v2 t
  case rest of
    [] -> NonEmpty.singleton vector
    v3 : remaining -> NonEmpty.prepend vector (deCasteljauStep t v2 v3 remaining)

segmentControlVectors ::
  Float ->
  Float ->
  NonEmpty (Vector2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
segmentControlVectors a b controlVectors =
  NonEmpty.map (segmentControlVector a b controlVectors) $
    NonEmpty.range 0 (NonEmpty.length controlVectors - 1)

segmentControlVector ::
  Float ->
  Float ->
  NonEmpty (Vector2d (space @ units)) ->
  Int ->
  Vector2d (space @ units)
segmentControlVector _ _ (vector :| []) _ = vector
segmentControlVector a b (p1 :| p2 : ps) n = do
  let t = if n > 0 then b else a
  let reduced = deCasteljauStep t p1 p2 ps
  segmentControlVector a b reduced (n - 1)

controlVectorDifferences ::
  Float ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  List (Vector2d (space @ units)) ->
  NonEmpty (Vector2d (space @ units))
controlVectorDifferences scale v1 v2 rest = do
  let difference = scale * (v2 - v1)
  case rest of
    [] -> NonEmpty.singleton difference
    v3 : remaining -> NonEmpty.prepend difference (controlVectorDifferences scale v2 v3 remaining)

evaluateAt :: Float -> VectorCurve2d (space @ units) -> Vector2d (space @ units)
evaluateAt t curve = case curve of
  VectorCurve2d c -> evaluateAtImpl t c
  Constant value -> value
  Coerce c -> Units.coerce (evaluateAt t c)
  Reversed c -> evaluateAt (1 - t) c
  XY x y -> Vector2d.xy (Curve1d.pointOn x t) (Curve1d.pointOn y t)
  Negated c -> -(evaluateAt t c)
  Sum c1 c2 -> evaluateAt t c1 + evaluateAt t c2
  Difference c1 c2 -> evaluateAt t c1 - evaluateAt t c2
  Product1d2d' c1 c2 -> Curve1d.pointOn c1 t .*. evaluateAt t c2
  Product2d1d' c1 c2 -> evaluateAt t c1 .*. Curve1d.pointOn c2 t
  Quotient' c1 c2 -> evaluateAt t c1 ./. Curve1d.pointOn c2 t
  PlaceInBasis basis c -> Vector2d.placeInBasis basis (evaluateAt t c)
  Line v1 v2 -> Vector2d.interpolateFrom v1 v2 t
  Arc v1 v2 a b -> do
    let theta = Qty.interpolateFrom a b t
    Angle.cos theta * v1 + Angle.sin theta * v2
  QuadraticSpline v1 v2 v3 -> quadraticBlossom v1 v2 v3 t t
  CubicSpline v1 v2 v3 v4 -> cubicBlossom v1 v2 v3 v4 t t t
  BezierCurve controlVectors -> deCasteljau t controlVectors
  Transformed transform c -> Vector2d.transformBy transform (evaluateAt t c)

segmentBounds :: Range Unitless -> VectorCurve2d (space @ units) -> VectorBounds2d (space @ units)
segmentBounds t@(Range tl th) curve = case curve of
  VectorCurve2d c -> segmentBoundsImpl t c
  Constant value -> VectorBounds2d.constant value
  Coerce c -> Units.coerce (segmentBounds t c)
  Reversed c -> segmentBounds (1 - t) c
  XY x y -> VectorBounds2d (Curve1d.segmentBounds x t) (Curve1d.segmentBounds y t)
  Negated c -> -(segmentBounds t c)
  Sum c1 c2 -> segmentBounds t c1 + segmentBounds t c2
  Difference c1 c2 -> segmentBounds t c1 - segmentBounds t c2
  Product1d2d' c1 c2 -> Curve1d.segmentBounds c1 t .*. segmentBounds t c2
  Product2d1d' c1 c2 -> segmentBounds t c1 .*. Curve1d.segmentBounds c2 t
  Quotient' c1 c2 -> segmentBounds t c1 ./. Curve1d.segmentBounds c2 t
  PlaceInBasis basis c -> VectorBounds2d.placeInBasis basis (segmentBounds t c)
  Line v1 v2 ->
    VectorBounds2d.hull2
      (Vector2d.interpolateFrom v1 v2 tl)
      (Vector2d.interpolateFrom v1 v2 th)
  Arc v1 v2 a b -> do
    let theta = a + (b - a) * t
    Range.cos theta * v1 + Range.sin theta * v2
  QuadraticSpline v1 v2 v3 ->
    VectorBounds2d.hull3
      (quadraticBlossom v1 v2 v3 tl tl)
      (quadraticBlossom v1 v2 v3 tl th)
      (quadraticBlossom v1 v2 v3 th th)
  CubicSpline v1 v2 v3 v4 ->
    VectorBounds2d.hull4
      (cubicBlossom v1 v2 v3 v4 tl tl tl)
      (cubicBlossom v1 v2 v3 v4 tl tl th)
      (cubicBlossom v1 v2 v3 v4 tl th th)
      (cubicBlossom v1 v2 v3 v4 th th th)
  BezierCurve controlVectors ->
    VectorBounds2d.hullN (segmentControlVectors tl th controlVectors)
  Transformed transform c -> VectorBounds2d.transformBy transform (segmentBounds t c)

derivative ::
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
derivative curve = case curve of
  VectorCurve2d c -> derivativeImpl c
  Constant _ -> zero
  Coerce c -> Units.coerce (derivative c)
  Reversed c -> negate (reverse (derivative c))
  XY x y -> XY (Curve1d.derivative x) (Curve1d.derivative y)
  Negated c -> -(derivative c)
  Sum c1 c2 -> derivative c1 + derivative c2
  Difference c1 c2 -> derivative c1 - derivative c2
  Product1d2d' c1 c2 -> Curve1d.derivative c1 .*. c2 + c1 .*. derivative c2
  Product2d1d' c1 c2 -> derivative c1 .*. c2 + c1 .*. Curve1d.derivative c2
  Quotient' c1 c2 ->
    (derivative c1 .*. c2 - c1 .*. Curve1d.derivative c2) .!/.! Curve1d.squared' c2
  PlaceInBasis basis c -> PlaceInBasis basis (derivative c)
  Line v1 v2 -> constant (v2 - v1)
  Arc v1 v2 a b -> do
    let scale = Angle.inRadians (b - a)
    Arc (v2 * scale) (-v1 * scale) a b
  QuadraticSpline v1 v2 v3 -> line (2 * (v2 - v1)) (2 * (v3 - v2))
  CubicSpline v1 v2 v3 v4 ->
    quadraticSpline (3 * (v2 - v1)) (3 * (v3 - v2)) (3 * (v4 - v3))
  BezierCurve (_ :| []) -> zero
  BezierCurve (v1 :| v2 : rest) -> do
    let degree = 1 + List.length rest
    BezierCurve (controlVectorDifferences (Float.int degree) v1 v2 rest)
  Transformed transform c -> transformBy transform (derivative c)

reverse ::
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
reverse curve = case curve of
  VectorCurve2d _ -> Reversed curve
  Constant _ -> curve
  Coerce c -> Units.coerce (reverse c)
  Reversed c -> c
  XY x y -> XY (Curve1d.reverse x) (Curve1d.reverse y)
  Negated c -> Negated (reverse c)
  Sum c1 c2 -> Sum (reverse c1) (reverse c2)
  Difference c1 c2 -> Difference (reverse c1) (reverse c2)
  Product1d2d' c1 c2 -> Product1d2d' (Curve1d.reverse c1) (reverse c2)
  Product2d1d' c1 c2 -> Product2d1d' (reverse c1) (Curve1d.reverse c2)
  Quotient' c1 c2 -> Quotient' (reverse c1) (Curve1d.reverse c2)
  PlaceInBasis basis c -> PlaceInBasis basis (reverse c)
  Line v1 v2 -> Line v2 v1
  Arc v1 v2 a b -> Arc v1 v2 b a
  QuadraticSpline v1 v2 v3 -> QuadraticSpline v3 v2 v1
  CubicSpline v1 v2 v3 v4 -> CubicSpline v4 v3 v2 v1
  BezierCurve controlVectors -> BezierCurve (NonEmpty.reverse controlVectors)
  Transformed transform c -> Transformed transform (reverse c)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

instance Curve1d.Interface (SquaredMagnitude' (space @ units)) (units :*: units) where
  pointOnImpl (SquaredMagnitude' curve) t =
    Vector2d.squaredMagnitude' (evaluateAt t curve)
  segmentBoundsImpl (SquaredMagnitude' curve) t =
    VectorBounds2d.squaredMagnitude' (segmentBounds t curve)
  derivativeImpl (SquaredMagnitude' curve) =
    2 * curve .<>. derivative curve
  expressionImpl (SquaredMagnitude' curve) = Maybe.map Function.VectorCurve2d.squaredMagnitude' (expression curve)

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve2d (space @ units) -> Curve1d (units :*: units)
squaredMagnitude' curve = Curve1d.new (SquaredMagnitude' curve)

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (VectorCurve2d coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

instance Curve1d.Interface (NonZeroMagnitude (space @ units)) units where
  pointOnImpl (NonZeroMagnitude curve) t =
    Vector2d.magnitude (VectorCurve2d.evaluateAt t curve)
  segmentBoundsImpl (NonZeroMagnitude curve) t =
    VectorBounds2d.magnitude (VectorCurve2d.segmentBounds t curve)
  derivativeImpl (NonZeroMagnitude curve) =
    (VectorCurve2d.derivative curve .<>. curve) .!/! Curve1d.new (NonZeroMagnitude curve)
  expressionImpl (NonZeroMagnitude curve) = Maybe.map Function.VectorCurve2d.magnitude (expression curve)

unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve1d units
unsafeMagnitude curve = Curve1d.new (NonZeroMagnitude curve)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZero (Curve1d units)
magnitude curve =
  case zeros curve of
    Success [] -> Success (Curve1d.new (NonZeroMagnitude curve))
    Success List.OneOrMore -> Failure HasZero
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    Failure Zeros.HigherOrderZero -> Failure HasZero

isZero :: Tolerance units => VectorCurve2d (space @ units) -> Bool
isZero curve = Tolerance.using Tolerance.squared' (squaredMagnitude' curve ~= Qty.zero)

hasZero :: Tolerance units => VectorCurve2d (space @ units) -> Bool
hasZero curve = Tolerance.using Tolerance.squared' (squaredMagnitude' curve ^ Qty.zero)

zeros :: Tolerance units => VectorCurve2d (space @ units) -> Result Zeros.Error (List Float)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve1d.zeros (squaredMagnitude' curve)) of
    Success roots -> Success (List.map Curve1d.Root.value roots)
    Failure Curve1d.Zeros.ZeroEverywhere -> Failure Zeros.ZeroEverywhere
    Failure Curve1d.Zeros.HigherOrderZero -> Failure Zeros.HigherOrderZero

xComponent :: VectorCurve2d (space @ units) -> Curve1d units
xComponent curve = curve <> Direction2d.x

yComponent :: VectorCurve2d (space @ units) -> Curve1d units
yComponent curve = curve <> Direction2d.y

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
    Success roots -> do
      let curveDerivative = derivative curve
      if List.allSatisfy (isRemovableDegeneracy curveDerivative) roots
        then Success (VectorCurve2d.Direction.unsafe curve curveDerivative)
        else Failure HasZero
    -- Definitely can't get the direction of a vector curve
    -- if that vector curve is zero everywhere!
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    -- If a curve has a higher-order zero, that still means it has a zeros...
    Failure Zeros.HigherOrderZero -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative t =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (t == 0.0 || t == 1.0) && evaluateAt t curveDerivative != Vector2d.zero

placeIn ::
  Frame2d (global @ originUnits) (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeIn globalFrame = placeInBasis (Frame2d.basis globalFrame)

relativeTo ::
  Frame2d (global @ originUnits) (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeTo globalFrame = relativeToBasis (Frame2d.basis globalFrame)

placeInBasis ::
  Basis2d global (Defines local) ->
  VectorCurve2d (local @ units) ->
  VectorCurve2d (global @ units)
placeInBasis globalBasis (PlaceInBasis basis curve) =
  PlaceInBasis (Basis2d.placeInBasis globalBasis basis) curve
placeInBasis globalBasis curve =
  PlaceInBasis globalBasis curve

relativeToBasis ::
  Basis2d global (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
