-- Needed for 'Curve1d * Vector3d = VectorCurve3d'
-- and 'Vector3d * Curve1d = VectorCurve3d' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Curve1d or Vector3d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module VectorCurve3d
  ( VectorCurve3d (Parametric)
  , Interface (..)
  , new
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , derivative
  , zero
  , constant
  , xyz
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
  , zComponent
  , direction
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import Angle qualified
import Basis3d (Basis3d)
import Basis3d qualified
import Composition
import CoordinateSystem (Space)
import Curve1d (Curve1d)
import Curve1d qualified
import Curve1d.Zero qualified
import Curve1d.Zeros qualified
import Direction3d (Direction3d)
import Direction3d qualified
import {-# SOURCE #-} DirectionCurve3d (DirectionCurve3d)
import Error qualified
import Expression (Expression)
import Expression qualified
import Expression.Curve1d qualified
import Expression.VectorCurve3d qualified
import Frame3d (Frame3d)
import Frame3d qualified
import List qualified
import NonEmpty qualified
import OpenSolid
import Point3d qualified
import Qty qualified
import Range (Range)
import Tolerance qualified
import Transform3d (Transform3d)
import Transform3d qualified
import Units qualified
import Vector3d (Vector3d)
import Vector3d qualified
import VectorBounds3d (VectorBounds3d (VectorBounds3d))
import VectorBounds3d qualified
import VectorCurve3d.Direction qualified
import VectorCurve3d.Zeros qualified as Zeros

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateImpl :: curve -> Float -> Vector3d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> VectorBounds3d coordinateSystem
  derivativeImpl :: curve -> VectorCurve3d coordinateSystem
  transformByImpl ::
    Transform3d tag (Space coordinateSystem @ translationUnits) ->
    curve ->
    VectorCurve3d coordinateSystem

data VectorCurve3d (coordinateSystem :: CoordinateSystem) where
  VectorCurve3d ::
    Interface curve (space @ units) =>
    curve ->
    VectorCurve3d (space @ units)
  Parametric ::
    Expression Float (Vector3d (space @ units)) ->
    VectorCurve3d (space @ units)
  Coerce ::
    VectorCurve3d (space @ units1) ->
    VectorCurve3d (space @ units2)
  Reversed ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  XYZ ::
    Curve1d units ->
    Curve1d units ->
    Curve1d units ->
    VectorCurve3d (space @ units)
  Negated ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Sum ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Difference ::
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Product1d3d' ::
    Curve1d units1 ->
    VectorCurve3d (space @ units2) ->
    VectorCurve3d (space @ (units1 :*: units2))
  Product3d1d' ::
    VectorCurve3d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve3d (space @ (units1 :*: units2))
  Quotient' ::
    VectorCurve3d (space @ units1) ->
    Curve1d units2 ->
    VectorCurve3d (space @ (units1 :/: units2))
  CrossProduct' ::
    VectorCurve3d (space @ units1) ->
    VectorCurve3d (space @ units2) ->
    VectorCurve3d (space @ (units1 :*: units2))
  PlaceInBasis ::
    Basis3d global (Defines local) ->
    VectorCurve3d (local @ units) ->
    VectorCurve3d (global @ units)
  Transformed ::
    Transform3d.Affine (space @ Unitless) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)

deriving instance Show (VectorCurve3d (space @ units))

instance HasUnits (VectorCurve3d (space @ units)) where
  type UnitsOf (VectorCurve3d (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve3d (space1 @ unitsA)) (VectorCurve3d (space2 @ unitsB))
  where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance Interface (VectorCurve3d (space @ units)) (space @ units) where
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative
  transformByImpl = transformBy

instance Negation (VectorCurve3d (space @ units)) where
  negate curve = case curve of
    Parametric expression -> Parametric -expression
    Coerce c -> Coerce -c
    XYZ x y z -> XYZ -x -y -z
    Negated c -> c
    Difference c1 c2 -> Difference c2 c1
    Product1d3d' c1 c2 -> Product1d3d' -c1 c2
    Product3d1d' c1 c2 -> Product3d1d' c1 -c2
    _ -> Negated curve

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units))

instance Multiplication' Sign (VectorCurve3d (space @ units)) where
  type Sign .*. VectorCurve3d (space @ units) = VectorCurve3d (space @ (Unitless :*: units))
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units))

instance Multiplication' (VectorCurve3d (space @ units)) Sign where
  type VectorCurve3d (space @ units) .*. Sign = VectorCurve3d (space @ (units :*: Unitless))
  curve .*. Positive = Units.coerce curve
  curve .*. Negative = Units.coerce -curve

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorCurve3d (space @ units))
    (VectorCurve3d (space_ @ units_))
    (VectorCurve3d (space @ units))
  where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (VectorCurve3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorCurve3d (space @ units))
  where
  curve + vector = curve + constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector3d (space @ units))
    (VectorCurve3d (space_ @ units_))
    (VectorCurve3d (space @ units))
  where
  vector + curve = constant vector + curve

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorCurve3d (space @ units))
    (VectorCurve3d (space_ @ units_))
    (VectorCurve3d (space @ units))
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (VectorCurve3d (space @ units))
    (Vector3d (space_ @ units_))
    (VectorCurve3d (space @ units))
  where
  curve - vector = curve - constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector3d (space @ units))
    (VectorCurve3d (space_ @ units_))
    (VectorCurve3d (space @ units))
  where
  vector - curve = constant vector - curve

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))

instance Multiplication' (Curve1d units1) (VectorCurve3d (space @ units2)) where
  type
    Curve1d units1 .*. VectorCurve3d (space @ units2) =
      VectorCurve3d (space @ (units1 :*: units2))
  Curve1d.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d3d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))

instance Multiplication' (Qty units1) (VectorCurve3d (space @ units2)) where
  type
    Qty units1 .*. VectorCurve3d (space @ units2) =
      VectorCurve3d (space @ (units1 :*: units2))
  c1 .*. c2 = Curve1d.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d units1) (Vector3d (space @ units2)) (VectorCurve3d (space @ units3))

instance Multiplication' (Curve1d units1) (Vector3d (space @ units2)) where
  type
    Curve1d units1 .*. Vector3d (space @ units2) =
      VectorCurve3d (space @ (units1 :*: units2))
  curve .*. vector = curve .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3))

instance Multiplication' (VectorCurve3d (space @ units1)) (Curve1d units2) where
  type
    VectorCurve3d (space @ units1) .*. Curve1d units2 =
      VectorCurve3d (space @ (units1 :*: units2))
  Parametric lhs .*. Curve1d.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product3d1d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve3d (space @ units1)) (Qty units2) (VectorCurve3d (space @ units3))

instance Multiplication' (VectorCurve3d (space @ units1)) (Qty units2) where
  type
    VectorCurve3d (space @ units1) .*. Qty units2 =
      VectorCurve3d (space @ (units1 :*: units2))
  curve .*. value = curve .*. Curve1d.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Curve1d units2) where
  type
    Vector3d (space @ units1) .*. Curve1d units2 =
      VectorCurve3d (space @ (units1 :*: units2))
  vector .*. curve = constant vector .*. curve

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Curve1d units2) (VectorCurve3d (space @ units3))

instance Division' (VectorCurve3d (space @ units1)) (Curve1d units2) where
  type
    VectorCurve3d (space @ units1) ./. Curve1d units2 =
      VectorCurve3d (space @ (units1 :/: units2))
  Parametric lhs ./. Curve1d.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve3d (space @ units1)) (Qty units2) (VectorCurve3d (space @ units3))

instance Division' (VectorCurve3d (space @ units1)) (Qty units2) where
  type
    VectorCurve3d (space @ units1) ./. Qty units2 =
      VectorCurve3d (space @ (units1 :/: units2))
  curve ./. value = curve ./. Curve1d.constant value

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve3d (space @ units1)) (VectorCurve3d (space @ units2))
  deriving (Show)

instance Curve1d.Interface (DotProductOf space units1 units2) (units1 :*: units2) where
  evaluateImpl (DotProductOf c1 c2) tValue =
    evaluate c1 tValue .<>. evaluate c2 tValue

  evaluateBoundsImpl (DotProductOf c1 c2) tRange =
    evaluateBounds c1 tRange .<>. evaluateBounds c2 tRange

  derivativeImpl (DotProductOf c1 c2) =
    derivative c1 .<>. c2 + c1 .<>. derivative c2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve1d units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve3d (space1 @ units1)) (VectorCurve3d (space2 @ units2))
  where
  type
    VectorCurve3d (space1 @ units1) .<>. VectorCurve3d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  Parametric lhs .<>. Parametric rhs = Curve1d.Parametric (lhs .<>. rhs)
  lhs .<>. rhs = Curve1d.new (DotProductOf lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve3d (space1 @ units1)) (Vector3d (space2 @ units2)) (Curve1d units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve3d (space1 @ units1)) (Vector3d (space2 @ units2))
  where
  type
    VectorCurve3d (space1 @ units1) .<>. Vector3d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  curve .<>. vector = curve .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d (space1 @ units1)) (VectorCurve3d (space2 @ units2)) (Curve1d units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector3d (space1 @ units1)) (VectorCurve3d (space2 @ units2))
  where
  type
    Vector3d (space1 @ units1) .<>. VectorCurve3d (space2 @ units2) =
      Curve1d (units1 :*: units2)
  vector .<>. curve = constant vector .<>. curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (Direction3d space2) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve3d (space1 @ units)) (Direction3d space2)
  where
  type VectorCurve3d (space1 @ units) .<>. Direction3d space2 = Curve1d (units :*: Unitless)
  curve .<>. direction3d = curve .<>. Vector3d.unit direction3d

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (VectorCurve3d (space2 @ units)) (Curve1d units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction3d space1) (VectorCurve3d (space2 @ units))
  where
  type Direction3d space1 .<>. VectorCurve3d (space2 @ units) = Curve1d (Unitless :*: units)
  direction3d .<>. curve = Vector3d.unit direction3d .<>. curve

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve3d (space1 @ units1)) (VectorCurve3d (space2 @ units2))
  where
  type
    VectorCurve3d (space1 @ units1) .><. VectorCurve3d (space2 @ units2) =
      VectorCurve3d (space1 @ (units1 :*: units2))
  Parametric lhs .><. Parametric rhs = Parametric (lhs .><. rhs)
  lhs .><. rhs = CrossProduct' lhs rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve3d (space1 @ units1)) (Vector3d (space2 @ units2))
  where
  type
    VectorCurve3d (space1 @ units1) .><. Vector3d (space2 @ units2) =
      VectorCurve3d (space1 @ (units1 :*: units2))
  curve .><. vector = curve .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector3d (space1 @ units1)) (VectorCurve3d (space2 @ units2))
  where
  type
    Vector3d (space1 @ units1) .><. VectorCurve3d (space2 @ units2) =
      VectorCurve3d (space1 @ (units1 :*: units2))
  vector .><. curve = constant vector .><. curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (Direction3d space2)
    (VectorCurve3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve3d (space1 @ units)) (Direction3d space2)
  where
  type
    VectorCurve3d (space1 @ units) .><. Direction3d space2 =
      VectorCurve3d (space1 @ (units :*: Unitless))
  curve .><. direction3d = curve .><. Vector3d.unit direction3d

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction3d space1) (VectorCurve3d (space2 @ units))
  where
  type
    Direction3d space1 .><. VectorCurve3d (space2 @ units) =
      VectorCurve3d (space1 @ (Unitless :*: units))
  direction3d .><. curve = Vector3d.unit direction3d .><. curve

instance
  Composition
    (Curve1d Unitless)
    (VectorCurve3d (space @ units))
    (VectorCurve3d (space @ units))
  where
  Parametric outer . Curve1d.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance
  VectorCurve3d.Interface
    (VectorCurve3d (space @ units) :.: Curve1d Unitless)
    (space @ units)
  where
  evaluateImpl (vectorCurve3d :.: curve1d) tValue =
    evaluate vectorCurve3d (Curve1d.evaluate curve1d tValue)

  evaluateBoundsImpl (vectorCurve3d :.: curve1d) tRange =
    evaluateBounds vectorCurve3d (Curve1d.evaluateBounds curve1d tRange)

  derivativeImpl (vectorCurve3d :.: curve1d) =
    (derivative vectorCurve3d . curve1d) * Curve1d.derivative curve1d

  transformByImpl transform (vectorCurve3d :.: curve1d) =
    new (transformBy transform vectorCurve3d :.: curve1d)

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
transformBy transform curve = do
  let t = Units.erase (Transform3d.toAffine transform)
  case curve of
    VectorCurve3d c -> transformByImpl transform c
    Parametric expression -> Parametric (Expression.VectorCurve3d.transformBy t expression)
    Coerce c -> Coerce (VectorCurve3d.transformBy transform c)
    Reversed c -> Reversed (transformBy transform c)
    XYZ{} -> Transformed t curve
    Negated c -> Negated (transformBy transform c)
    Sum c1 c2 -> Sum (transformBy transform c1) (transformBy transform c2)
    Difference c1 c2 -> Difference (transformBy transform c1) (transformBy transform c2)
    Product1d3d' curve1d curve3d -> Product1d3d' curve1d (transformBy transform curve3d)
    Product3d1d' curve3d curve1d -> Product3d1d' (transformBy transform curve3d) curve1d
    Quotient' curve3d curve1d -> Quotient' (transformBy transform curve3d) curve1d
    CrossProduct' c1 c2 -> CrossProduct' (transformBy transform c1) (transformBy transform c2)
    PlaceInBasis basis c -> do
      let localTransform = Transform3d.relativeTo (Frame3d.at Point3d.origin basis) transform
      PlaceInBasis basis (transformBy localTransform c)
    Transformed existing c -> Transformed (existing >> t) c

new :: Interface curve (space @ units) => curve -> VectorCurve3d (space @ units)
new = VectorCurve3d

zero :: VectorCurve3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
constant = Parametric . Expression.constant

xyz :: Curve1d units -> Curve1d units -> Curve1d units -> VectorCurve3d (space @ units)
xyz (Curve1d.Parametric x) (Curve1d.Parametric y) (Curve1d.Parametric z) =
  Parametric (Expression.xyz x y z)
xyz x y z = XYZ x y z

line :: Vector3d (space @ units) -> Vector3d (space @ units) -> VectorCurve3d (space @ units)
line v1 v2 =
  Parametric $
    Expression.VectorCurve3d.constant v1
      + Expression.t * Expression.VectorCurve3d.constant (v2 - v1)

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
      let angle = Expression.Curve1d.constant a + Expression.t * Expression.Curve1d.constant (b - a)
      Parametric $
        Expression.VectorCurve3d.constant v1 * Expression.cos angle
          + Expression.VectorCurve3d.constant v2 * Expression.sin angle

quadraticSpline ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorCurve3d (space @ units)
quadraticSpline v1 v2 v3 =
  Parametric (Expression.quadraticSpline v1 v2 v3)

cubicSpline ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  VectorCurve3d (space @ units)
cubicSpline v1 v2 v3 v4 =
  Parametric (Expression.cubicSpline v1 v2 v3 v4)

bezierCurve :: NonEmpty (Vector3d (space @ units)) -> VectorCurve3d (space @ units)
bezierCurve = Parametric . Expression.bezierCurve

startValue :: VectorCurve3d (space @ units) -> Vector3d (space @ units)
startValue curve = evaluate curve 0.0

endValue :: VectorCurve3d (space @ units) -> Vector3d (space @ units)
endValue curve = evaluate curve 1.0

evaluate :: VectorCurve3d (space @ units) -> Float -> Vector3d (space @ units)
evaluate curve tValue = case curve of
  VectorCurve3d c -> evaluateImpl c tValue
  Parametric expression -> Expression.evaluate expression tValue
  Coerce c -> Units.coerce (evaluate c tValue)
  Reversed c -> evaluate c (1.0 - tValue)
  XYZ x y z ->
    Vector3d.xyz (Curve1d.evaluate x tValue) (Curve1d.evaluate y tValue) (Curve1d.evaluate z tValue)
  Negated c -> negate (evaluate c tValue)
  Sum c1 c2 -> evaluate c1 tValue + evaluate c2 tValue
  Difference c1 c2 -> evaluate c1 tValue - evaluate c2 tValue
  Product1d3d' c1 c2 -> Curve1d.evaluate c1 tValue .*. evaluate c2 tValue
  Product3d1d' c1 c2 -> evaluate c1 tValue .*. Curve1d.evaluate c2 tValue
  Quotient' c1 c2 -> evaluate c1 tValue ./. Curve1d.evaluate c2 tValue
  CrossProduct' c1 c2 -> evaluate c1 tValue .><. evaluate c2 tValue
  PlaceInBasis basis c -> Vector3d.placeInBasis basis (evaluate c tValue)
  Transformed transform c -> Vector3d.transformBy transform (evaluate c tValue)

evaluateBounds :: VectorCurve3d (space @ units) -> Range Unitless -> VectorBounds3d (space @ units)
evaluateBounds curve tRange = case curve of
  VectorCurve3d c -> evaluateBoundsImpl c tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Coerce c -> Units.coerce (evaluateBounds c tRange)
  Reversed c -> evaluateBounds c (1.0 - tRange)
  XYZ x y z ->
    VectorBounds3d
      (Curve1d.evaluateBounds x tRange)
      (Curve1d.evaluateBounds y tRange)
      (Curve1d.evaluateBounds z tRange)
  Negated c -> negate (evaluateBounds c tRange)
  Sum c1 c2 -> evaluateBounds c1 tRange + evaluateBounds c2 tRange
  Difference c1 c2 -> evaluateBounds c1 tRange - evaluateBounds c2 tRange
  Product1d3d' c1 c2 -> Curve1d.evaluateBounds c1 tRange .*. evaluateBounds c2 tRange
  Product3d1d' c1 c2 -> evaluateBounds c1 tRange .*. Curve1d.evaluateBounds c2 tRange
  Quotient' c1 c2 -> evaluateBounds c1 tRange ./. Curve1d.evaluateBounds c2 tRange
  CrossProduct' c1 c2 -> evaluateBounds c1 tRange .><. evaluateBounds c2 tRange
  PlaceInBasis basis c -> VectorBounds3d.placeInBasis basis (evaluateBounds c tRange)
  Transformed transform c -> VectorBounds3d.transformBy transform (evaluateBounds c tRange)

derivative ::
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
derivative curve = case curve of
  VectorCurve3d c -> derivativeImpl c
  Parametric expression -> Parametric (Expression.curveDerivative expression)
  Coerce c -> Units.coerce (derivative c)
  Reversed c -> negate (reverse (derivative c))
  XYZ x y z -> XYZ (Curve1d.derivative x) (Curve1d.derivative y) (Curve1d.derivative z)
  Negated c -> -(derivative c)
  Sum c1 c2 -> derivative c1 + derivative c2
  Difference c1 c2 -> derivative c1 - derivative c2
  Product1d3d' c1 c2 -> Curve1d.derivative c1 .*. c2 + c1 .*. derivative c2
  Product3d1d' c1 c2 -> derivative c1 .*. c2 + c1 .*. Curve1d.derivative c2
  Quotient' c1 c2 ->
    (derivative c1 .*. c2 - c1 .*. Curve1d.derivative c2) .!/.! Curve1d.squared' c2
  CrossProduct' c1 c2 -> derivative c1 .><. c2 + c1 .><. derivative c2
  PlaceInBasis basis c -> PlaceInBasis basis (derivative c)
  Transformed transform c -> transformBy transform (derivative c)

reverse ::
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
reverse curve = case curve of
  VectorCurve3d _ -> Reversed curve
  Parametric expression -> Parametric (expression . Expression.r)
  Coerce c -> Units.coerce (reverse c)
  Reversed c -> c
  XYZ x y z -> XYZ (Curve1d.reverse x) (Curve1d.reverse y) (Curve1d.reverse z)
  Negated c -> Negated (reverse c)
  Sum c1 c2 -> Sum (reverse c1) (reverse c2)
  Difference c1 c2 -> Difference (reverse c1) (reverse c2)
  Product1d3d' c1 c2 -> Product1d3d' (Curve1d.reverse c1) (reverse c2)
  Product3d1d' c1 c2 -> Product3d1d' (reverse c1) (Curve1d.reverse c2)
  Quotient' c1 c2 -> Quotient' (reverse c1) (Curve1d.reverse c2)
  CrossProduct' c1 c2 -> CrossProduct' (reverse c1) (reverse c2)
  PlaceInBasis basis c -> PlaceInBasis basis (reverse c)
  Transformed transform c -> Transformed transform (reverse c)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (VectorCurve3d coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

instance Curve1d.Interface (SquaredMagnitude' (space @ units)) (units :*: units) where
  evaluateImpl (SquaredMagnitude' curve) tValue =
    Vector3d.squaredMagnitude' (evaluate curve tValue)

  evaluateBoundsImpl (SquaredMagnitude' curve) tRange =
    VectorBounds3d.squaredMagnitude' (evaluateBounds curve tRange)

  derivativeImpl (SquaredMagnitude' curve) =
    2.0 * curve .<>. derivative curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve3d (space @ units) -> Curve1d (units :*: units)
squaredMagnitude' (Parametric expression) =
  Curve1d.Parametric (Expression.VectorCurve3d.squaredMagnitude' expression)
squaredMagnitude' curve = Curve1d.new (SquaredMagnitude' curve)

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (VectorCurve3d coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

instance Curve1d.Interface (NonZeroMagnitude (space @ units)) units where
  evaluateImpl (NonZeroMagnitude curve) tValue =
    Vector3d.magnitude (VectorCurve3d.evaluate curve tValue)

  evaluateBoundsImpl (NonZeroMagnitude curve) tRange =
    VectorBounds3d.magnitude (VectorCurve3d.evaluateBounds curve tRange)

  derivativeImpl (NonZeroMagnitude curve) =
    (VectorCurve3d.derivative curve .<>. curve) .!/! Curve1d.new (NonZeroMagnitude curve)

unsafeMagnitude :: VectorCurve3d (space @ units) -> Curve1d units
unsafeMagnitude (Parametric expression) =
  Curve1d.Parametric (Expression.VectorCurve3d.magnitude expression)
unsafeMagnitude curve = Curve1d.new (NonZeroMagnitude curve)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve3d (space @ units) -> Result HasZero (Curve1d units)
magnitude curve =
  case zeros curve of
    Success [] -> Success (unsafeMagnitude curve)
    Success List.OneOrMore -> Failure HasZero
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    Failure Zeros.HigherOrderZero -> Failure HasZero

isZero :: Tolerance units => VectorCurve3d (space @ units) -> Bool
isZero curve = Tolerance.using Tolerance.squared' (squaredMagnitude' curve ~= Qty.zero)

hasZero :: Tolerance units => VectorCurve3d (space @ units) -> Bool
hasZero curve = Tolerance.using Tolerance.squared' (squaredMagnitude' curve ^ Qty.zero)

zeros :: Tolerance units => VectorCurve3d (space @ units) -> Result Zeros.Error (List Float)
zeros curve =
  case Tolerance.using Tolerance.squared' (Curve1d.zeros (squaredMagnitude' curve)) of
    Success zeros1d -> Success (List.map Curve1d.Zero.location zeros1d)
    Failure Curve1d.Zeros.ZeroEverywhere -> Failure Zeros.ZeroEverywhere
    Failure Curve1d.Zeros.HigherOrderZero -> Failure Zeros.HigherOrderZero

xComponent :: VectorCurve3d (space @ units) -> Curve1d units
xComponent curve = curve <> Direction3d.x

yComponent :: VectorCurve3d (space @ units) -> Curve1d units
yComponent curve = curve <> Direction3d.y

zComponent :: VectorCurve3d (space @ units) -> Curve1d units
zComponent curve = curve <> Direction3d.z

direction ::
  Tolerance units =>
  VectorCurve3d (space @ units) ->
  Result HasZero (DirectionCurve3d space)
direction curve =
  case zeros curve of
    -- If the vector curve has no zeros, then we can safely compute its direction
    Success [] -> Success (VectorCurve3d.Direction.unsafe curve (derivative curve))
    -- Otherwise, check where the vector curve is zero:
    -- if it's only zero at one or both endpoints,
    -- and the curve's *derivative* is non-zero at those endpoints,
    -- then it's still possible to uniquely determine a tangent direction everywhere
    Success (NonEmpty curveZeros) -> do
      let curveDerivative = derivative curve
      if NonEmpty.allSatisfy (isRemovableDegeneracy curveDerivative) curveZeros
        then Success (VectorCurve3d.Direction.unsafe curve curveDerivative)
        else Failure HasZero
    -- Definitely can't get the direction of a vector curve
    -- if that vector curve is zero everywhere!
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    -- If a curve has a higher-order zero, that still means it has a zeros...
    Failure Zeros.HigherOrderZero -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve3d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative tValue =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (tValue == 0.0 || tValue == 1.0) && evaluate curveDerivative tValue != Vector3d.zero

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  VectorCurve3d (local @ units) ->
  VectorCurve3d (global @ units)
placeIn globalFrame = placeInBasis (Frame3d.basis globalFrame)

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  VectorCurve3d (global @ units) ->
  VectorCurve3d (local @ units)
relativeTo globalFrame = relativeToBasis (Frame3d.basis globalFrame)

placeInBasis ::
  Basis3d global (Defines local) ->
  VectorCurve3d (local @ units) ->
  VectorCurve3d (global @ units)
placeInBasis globalBasis (Parametric expression) =
  Parametric (Expression.VectorCurve3d.placeInBasis globalBasis expression)
placeInBasis globalBasis (PlaceInBasis basis curve) =
  PlaceInBasis (Basis3d.placeInBasis globalBasis basis) curve
placeInBasis globalBasis curve = PlaceInBasis globalBasis curve

relativeToBasis ::
  Basis3d global (Defines local) ->
  VectorCurve3d (global @ units) ->
  VectorCurve3d (local @ units)
relativeToBasis basis = placeInBasis (Basis3d.inverse basis)
