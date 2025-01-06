module OpenSolid.VectorCurve2d
  ( VectorCurve2d (Parametric, Transformed)
  , Interface (..)
  , new
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
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
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Composition
import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Curve1d (Curve1d)
import OpenSolid.Curve1d qualified as Curve1d
import OpenSolid.Curve1d.Zero qualified as Curve1d.Zero
import OpenSolid.Curve1d.Zeros qualified as Curve1d.Zeros
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import {-# SOURCE #-} OpenSolid.DirectionCurve2d (DirectionCurve2d)
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import {-# SOURCE #-} OpenSolid.Surface1d.Function qualified as Surface1d.Function
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorCurve2d.Direction qualified as VectorCurve2d.Direction
import OpenSolid.VectorCurve2d.Zeros qualified as Zeros
import {-# SOURCE #-} OpenSolid.VectorSurface2d.Function qualified as VectorSurface2d.Function

class
  Show curve =>
  Interface curve (coordinateSystem :: CoordinateSystem)
    | curve -> coordinateSystem
  where
  evaluateImpl :: curve -> Float -> Vector2d coordinateSystem
  evaluateBoundsImpl :: curve -> Range Unitless -> VectorBounds2d coordinateSystem
  derivativeImpl :: curve -> VectorCurve2d coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    curve ->
    VectorCurve2d coordinateSystem

data VectorCurve2d (coordinateSystem :: CoordinateSystem) where
  VectorCurve2d ::
    Interface curve (space @ units) =>
    curve ->
    VectorCurve2d (space @ units)
  Parametric ::
    Expression Float (Vector2d (space @ units)) ->
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
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    VectorCurve2d (space @ units) ->
    VectorCurve2d (space @ units)

deriving instance Show (VectorCurve2d (space @ units))

instance HasUnits (VectorCurve2d (space @ units)) units

instance
  space1 ~ space2 =>
  Units.Coercion (VectorCurve2d (space1 @ unitsA)) (VectorCurve2d (space2 @ unitsB))
  where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce curve) = Coerce curve
  coerce curve = Coerce curve

instance Interface (VectorCurve2d (space @ units)) (space @ units) where
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative
  transformByImpl = transformBy

instance Negation (VectorCurve2d (space @ units)) where
  negate curve = case curve of
    Parametric expression -> Parametric -expression
    Coerce c -> Coerce -c
    XY x y -> XY -x -y
    Negated c -> c
    Difference c1 c2 -> Difference c2 c1
    Product1d2d' c1 c2 -> Product1d2d' -c1 c2
    Product2d1d' c1 c2 -> Product2d1d' c1 -c2
    _ -> Negated curve

instance Multiplication Sign (VectorCurve2d (space @ units)) (VectorCurve2d (space @ units)) where
  Positive * curve = curve
  Negative * curve = -curve

instance
  Multiplication'
    Sign
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ (Unitless :*: units)))
  where
  Positive .*. curve = Units.coerce curve
  Negative .*. curve = Units.coerce -curve

instance Multiplication (VectorCurve2d (space @ units)) Sign (VectorCurve2d (space @ units)) where
  curve * Positive = curve
  curve * Negative = -curve

instance
  Multiplication'
    (VectorCurve2d (space @ units))
    Sign
    (VectorCurve2d (space @ (units :*: Unitless)))
  where
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
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

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
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

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
  Multiplication
    (Curve1d units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve1d units1)
    (VectorCurve2d (space @ units2))
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  Curve1d.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d2d' lhs rhs

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
  c1 .*. c2 = Curve1d.constant c1 .*. c2

instance
  Units.Product units1 units2 units3 =>
  Multiplication (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (VectorCurve2d (space @ units1))
    (Curve1d units2)
    (VectorCurve2d (space @ (units1 :*: units2)))
  where
  Parametric lhs .*. Curve1d.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product2d1d' lhs rhs

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
  curve .*. value = curve .*. Curve1d.constant value

instance
  Units.Quotient units1 units2 units3 =>
  Division (VectorCurve2d (space @ units1)) (Curve1d units2) (VectorCurve2d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorCurve2d (space @ units1))
    (Curve1d units2)
    (VectorCurve2d (space @ (units1 :/: units2)))
  where
  Parametric lhs ./. Curve1d.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

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
  curve ./. value = curve ./. Curve1d.constant value

data DotProductOf space units1 units2
  = DotProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))
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
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d (units1 :*: units2))
  where
  Parametric lhs .<>. Parametric rhs = Curve1d.Parametric (lhs .<>. rhs)
  lhs .<>. rhs = Curve1d.new (DotProductOf lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve2d (space1 @ units1)) (Vector2d (space2 @ units2)) (Curve1d units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve1d (units1 :*: units2))
  where
  curve .<>. vector = curve .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector2d (space1 @ units1)) (VectorCurve2d (space2 @ units2)) (Curve1d units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d (units1 :*: units2))
  where
  vector .<>. curve = constant vector .<>. curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2d (space1 @ units)) (Direction2d space2) (Curve1d units)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve2d (space1 @ units))
    (Direction2d space2)
    (Curve1d (units :*: Unitless))
  where
  curve .<>. direction2d = curve .<>. Vector2d.unit direction2d

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (VectorCurve2d (space2 @ units)) (Curve1d units)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Direction2d space1)
    (VectorCurve2d (space2 @ units))
    (Curve1d (Unitless :*: units))
  where
  direction2d .<>. curve = Vector2d.unit direction2d .<>. curve

data CrossProductOf space units1 units2
  = CrossProductOf (VectorCurve2d (space @ units1)) (VectorCurve2d (space @ units2))
  deriving (Show)

instance Curve1d.Interface (CrossProductOf space units1 units2) (units1 :*: units2) where
  evaluateImpl (CrossProductOf c1 c2) tValue =
    evaluate c1 tValue .><. evaluate c2 tValue

  evaluateBoundsImpl (CrossProductOf c1 c2) tRange =
    evaluateBounds c1 tRange .><. evaluateBounds c2 tRange

  derivativeImpl (CrossProductOf c1 c2) =
    derivative c1 .><. c2 + c1 .><. derivative c2

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d (units1 :*: units2))
  where
  Parametric lhs .><. Parametric rhs = Curve1d.Parametric (lhs .><. rhs)
  lhs .><. rhs = Curve1d.new (CrossProductOf lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve1d units3)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve2d (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve1d (units1 :*: units2))
  where
  curve .><. vector = curve .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d units3)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector2d (space1 @ units1))
    (VectorCurve2d (space2 @ units2))
    (Curve1d (units1 :*: units2))
  where
  vector .><. curve = constant vector .><. curve

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2d (space1 @ units)) (Direction2d space2) (Curve1d units)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve2d (space1 @ units))
    (Direction2d space2)
    (Curve1d (units :*: Unitless))
  where
  curve .><. direction2d = curve .><. Vector2d.unit direction2d

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (VectorCurve2d (space2 @ units)) (Curve1d units)
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Direction2d space1)
    (VectorCurve2d (space2 @ units))
    (Curve1d (Unitless :*: units))
  where
  direction2d .><. curve = Vector2d.unit direction2d .><. curve

instance
  Composition
    (Curve1d Unitless)
    (VectorCurve2d (space @ units))
    (VectorCurve2d (space @ units))
  where
  Parametric outer . Curve1d.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance
  Interface
    (VectorCurve2d (space @ units) :.: Curve1d Unitless)
    (space @ units)
  where
  evaluateImpl (vectorCurve2d :.: curve1d) tValue =
    evaluate vectorCurve2d (Curve1d.evaluate curve1d tValue)

  evaluateBoundsImpl (vectorCurve2d :.: curve1d) tRange =
    evaluateBounds vectorCurve2d (Curve1d.evaluateBounds curve1d tRange)

  derivativeImpl (vectorCurve2d :.: curve1d) =
    (derivative vectorCurve2d . curve1d) * Curve1d.derivative curve1d

  transformByImpl transform (vectorCurve2d :.: curve1d) =
    new (transformBy transform vectorCurve2d :.: curve1d)

instance
  Composition
    (Surface1d.Function.Function Unitless)
    (VectorCurve2d (space @ units))
    (VectorSurface2d.Function.Function (space @ units))
  where
  Parametric curve . Surface1d.Function.Parametric function =
    VectorSurface2d.Function.Parametric (curve . function)
  curve . function = VectorSurface2d.Function.new (curve :.: function)

instance
  VectorSurface2d.Function.Interface
    (VectorCurve2d (space @ units) :.: Surface1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (curve :.: function) uvPoint =
    evaluate curve (Surface1d.Function.evaluate function uvPoint)

  evaluateBoundsImpl (curve :.: function) uvBounds =
    evaluateBounds curve (Surface1d.Function.evaluateBounds function uvBounds)

  derivativeImpl parameter (curve :.: function) =
    (derivative curve . function) * Surface1d.Function.derivative parameter function

  transformByImpl transform (curve :.: function) =
    transformBy transform curve . function

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
transformBy transform curve = do
  let t = Units.erase (Transform2d.toAffine transform)
  case curve of
    VectorCurve2d c -> transformByImpl transform c
    Parametric expression -> Parametric (Expression.VectorCurve2d.transformBy t expression)
    Coerce c -> Coerce (transformBy transform c)
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
constant = Parametric . Expression.constant

xy :: Curve1d units -> Curve1d units -> VectorCurve2d (space @ units)
xy (Curve1d.Parametric x) (Curve1d.Parametric y) = Parametric (Expression.xy x y)
xy x y = XY x y

line :: Vector2d (space @ units) -> Vector2d (space @ units) -> VectorCurve2d (space @ units)
line v1 v2 =
  Parametric $
    Expression.VectorCurve2d.constant v1 + Expression.t * Expression.VectorCurve2d.constant (v2 - v1)

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
      let angle = Expression.Curve1d.constant a + Expression.t * Expression.Curve1d.constant (b - a)
      Parametric $
        Expression.VectorCurve2d.constant v1 * Expression.cos angle
          + Expression.VectorCurve2d.constant v2 * Expression.sin angle

quadraticSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
quadraticSpline v1 v2 v3 =
  Parametric (Expression.quadraticSpline v1 v2 v3)

cubicSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  VectorCurve2d (space @ units)
cubicSpline v1 v2 v3 v4 =
  Parametric (Expression.cubicSpline v1 v2 v3 v4)

bezierCurve :: NonEmpty (Vector2d (space @ units)) -> VectorCurve2d (space @ units)
bezierCurve = Parametric . Expression.bezierCurve

startValue :: VectorCurve2d (space @ units) -> Vector2d (space @ units)
startValue curve = evaluate curve 0.0

endValue :: VectorCurve2d (space @ units) -> Vector2d (space @ units)
endValue curve = evaluate curve 1.0

evaluate :: VectorCurve2d (space @ units) -> Float -> Vector2d (space @ units)
evaluate curve tValue = case curve of
  VectorCurve2d c -> evaluateImpl c tValue
  Parametric expression -> Expression.evaluate expression tValue
  Coerce c -> Units.coerce (evaluate c tValue)
  Reversed c -> evaluate c (1.0 - tValue)
  XY x y -> Vector2d.xy (Curve1d.evaluate x tValue) (Curve1d.evaluate y tValue)
  Negated c -> negate (evaluate c tValue)
  Sum c1 c2 -> evaluate c1 tValue + evaluate c2 tValue
  Difference c1 c2 -> evaluate c1 tValue - evaluate c2 tValue
  Product1d2d' c1 c2 -> Curve1d.evaluate c1 tValue .*. evaluate c2 tValue
  Product2d1d' c1 c2 -> evaluate c1 tValue .*. Curve1d.evaluate c2 tValue
  Quotient' c1 c2 -> evaluate c1 tValue ./. Curve1d.evaluate c2 tValue
  PlaceInBasis basis c -> Vector2d.placeInBasis basis (evaluate c tValue)
  Transformed transform c -> Vector2d.transformBy transform (evaluate c tValue)

evaluateBounds :: VectorCurve2d (space @ units) -> Range Unitless -> VectorBounds2d (space @ units)
evaluateBounds curve tRange = case curve of
  VectorCurve2d c -> evaluateBoundsImpl c tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Coerce c -> Units.coerce (evaluateBounds c tRange)
  Reversed c -> evaluateBounds c (1.0 - tRange)
  XY x y -> VectorBounds2d (Curve1d.evaluateBounds x tRange) (Curve1d.evaluateBounds y tRange)
  Negated c -> negate (evaluateBounds c tRange)
  Sum c1 c2 -> evaluateBounds c1 tRange + evaluateBounds c2 tRange
  Difference c1 c2 -> evaluateBounds c1 tRange - evaluateBounds c2 tRange
  Product1d2d' c1 c2 -> Curve1d.evaluateBounds c1 tRange .*. evaluateBounds c2 tRange
  Product2d1d' c1 c2 -> evaluateBounds c1 tRange .*. Curve1d.evaluateBounds c2 tRange
  Quotient' c1 c2 -> evaluateBounds c1 tRange ./. Curve1d.evaluateBounds c2 tRange
  PlaceInBasis basis c -> VectorBounds2d.placeInBasis basis (evaluateBounds c tRange)
  Transformed transform c -> VectorBounds2d.transformBy transform (evaluateBounds c tRange)

derivative ::
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
derivative curve = case curve of
  VectorCurve2d c -> derivativeImpl c
  Parametric expression -> Parametric (Expression.curveDerivative expression)
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
  Transformed transform c -> transformBy transform (derivative c)

reverse ::
  VectorCurve2d (space @ units) ->
  VectorCurve2d (space @ units)
reverse curve = case curve of
  VectorCurve2d _ -> Reversed curve
  Parametric expression -> Parametric (expression . Expression.r)
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
  Transformed transform c -> Transformed transform (reverse c)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (VectorCurve2d coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

instance Curve1d.Interface (SquaredMagnitude' (space @ units)) (units :*: units) where
  evaluateImpl (SquaredMagnitude' curve) tValue =
    Vector2d.squaredMagnitude' (evaluate curve tValue)

  evaluateBoundsImpl (SquaredMagnitude' curve) tRange =
    VectorBounds2d.squaredMagnitude' (evaluateBounds curve tRange)

  derivativeImpl (SquaredMagnitude' curve) =
    2.0 * curve .<>. derivative curve

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve2d (space @ units1) -> Curve1d units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve2d (space @ units) -> Curve1d (units :*: units)
squaredMagnitude' (Parametric expression) =
  Curve1d.Parametric (Expression.VectorCurve2d.squaredMagnitude' expression)
squaredMagnitude' curve = Curve1d.new (SquaredMagnitude' curve)

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (VectorCurve2d coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

instance Curve1d.Interface (NonZeroMagnitude (space @ units)) units where
  evaluateImpl (NonZeroMagnitude curve) tValue =
    Vector2d.magnitude (evaluate curve tValue)

  evaluateBoundsImpl (NonZeroMagnitude curve) tRange =
    VectorBounds2d.magnitude (evaluateBounds curve tRange)

  derivativeImpl (NonZeroMagnitude curve) =
    (derivative curve .<>. curve) .!/! Curve1d.new (NonZeroMagnitude curve)

unsafeMagnitude :: VectorCurve2d (space @ units) -> Curve1d units
unsafeMagnitude (Parametric expression) =
  Curve1d.Parametric (Expression.VectorCurve2d.magnitude expression)
unsafeMagnitude curve = Curve1d.new (NonZeroMagnitude curve)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve2d (space @ units) -> Result HasZero (Curve1d units)
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
  case Tolerance.using Tolerance.squared' (Curve1d.zeros (squaredMagnitude' curve)) of
    Success zeros1d -> Success (List.map Curve1d.Zero.location zeros1d)
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
    Success (NonEmpty curveZeros) -> do
      let curveDerivative = derivative curve
      if NonEmpty.allSatisfy (isRemovableDegeneracy curveDerivative) curveZeros
        then Success (VectorCurve2d.Direction.unsafe curve curveDerivative)
        else Failure HasZero
    -- Definitely can't get the direction of a vector curve
    -- if that vector curve is zero everywhere!
    Failure Zeros.ZeroEverywhere -> Failure HasZero
    -- If a curve has a higher-order zero, that still means it has a zeros...
    Failure Zeros.HigherOrderZero -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve2d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative tValue =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (tValue == 0.0 || tValue == 1.0) && evaluate curveDerivative tValue != Vector2d.zero

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
placeInBasis globalBasis (Parametric expression) =
  Parametric (Expression.VectorCurve2d.placeInBasis globalBasis expression)
placeInBasis globalBasis (PlaceInBasis basis curve) =
  PlaceInBasis (Basis2d.placeInBasis globalBasis basis) curve
placeInBasis globalBasis curve =
  PlaceInBasis globalBasis curve

relativeToBasis ::
  Basis2d global (Defines local) ->
  VectorCurve2d (global @ units) ->
  VectorCurve2d (local @ units)
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
