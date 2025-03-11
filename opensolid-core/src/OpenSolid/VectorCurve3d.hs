module OpenSolid.VectorCurve3d
  ( VectorCurve3d (Parametric, Transformed, Planar)
  , Interface (..)
  , new
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , compiled
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
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.CompiledFunction (CompiledFunction)
import OpenSolid.CompiledFunction qualified as CompiledFunction
import OpenSolid.Composition
import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Curve (Curve)
import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.Zero qualified as Curve.Zero
import OpenSolid.Curve.Zeros qualified as Curve.Zeros
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import {-# SOURCE #-} OpenSolid.DirectionCurve3d (DirectionCurve3d)
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.VectorCurve3d qualified as Expression.VectorCurve3d
import OpenSolid.Frame3d (Frame3d (Frame3d))
import OpenSolid.List qualified as List
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.PlanarBasis3d (PlanarBasis3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceFunction qualified as SurfaceFunction
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import OpenSolid.VectorCurve2d (VectorCurve2d)
import OpenSolid.VectorCurve2d qualified as VectorCurve2d
import OpenSolid.VectorCurve3d.Direction qualified as VectorCurve3d.Direction
import OpenSolid.VectorCurve3d.Zeros qualified as Zeros
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

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
    Curve units ->
    Curve units ->
    Curve units ->
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
    Curve units1 ->
    VectorCurve3d (space @ units2) ->
    VectorCurve3d (space @ (units1 :*: units2))
  Product3d1d' ::
    VectorCurve3d (space @ units1) ->
    Curve units2 ->
    VectorCurve3d (space @ (units1 :*: units2))
  Quotient' ::
    VectorCurve3d (space @ units1) ->
    Curve units2 ->
    VectorCurve3d (space @ (units1 :/: units2))
  CrossProduct' ::
    VectorCurve3d (space @ units1) ->
    VectorCurve3d (space @ units2) ->
    VectorCurve3d (space @ (units1 :*: units2))
  PlaceIn ::
    Basis3d global (Defines local) ->
    VectorCurve3d (local @ units) ->
    VectorCurve3d (global @ units)
  Transformed ::
    Transform3d tag (space @ translationUnits) ->
    VectorCurve3d (space @ units) ->
    VectorCurve3d (space @ units)
  Planar ::
    PlanarBasis3d space (Defines local) ->
    VectorCurve2d (local @ units) ->
    VectorCurve3d (space @ units)

type Compiled (coordinateSystem :: CoordinateSystem) =
  CompiledFunction
    Float
    (Vector3d coordinateSystem)
    (Range Unitless)
    (VectorBounds3d coordinateSystem)

deriving instance Show (VectorCurve3d (space @ units))

instance HasUnits (VectorCurve3d (space @ units)) units (VectorCurve3d (space @ Unitless))

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

instance Multiplication Sign (VectorCurve3d (space @ units)) (VectorCurve3d (space @ units)) where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication (VectorCurve3d (space @ units)) Sign (VectorCurve3d (space @ units)) where
  curve * Positive = curve
  curve * Negative = -curve

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
  Multiplication (Curve units1) (VectorCurve3d (space @ units2)) (VectorCurve3d (space @ units3))
  where
  lhs * rhs = Units.specialize (lhs .*. rhs)

instance
  Multiplication'
    (Curve units1)
    (VectorCurve3d (space @ units2))
    (VectorCurve3d (space @ (units1 :*: units2)))
  where
  lhs .*. rhs = Product1d3d' lhs rhs

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
  lhs .*. rhs = Product3d1d' lhs rhs

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
  Division (VectorCurve3d (space @ units1)) (Curve units2) (VectorCurve3d (space @ units3))
  where
  lhs / rhs = Units.specialize (lhs ./. rhs)

instance
  Division'
    (VectorCurve3d (space @ units1))
    (Curve units2)
    (VectorCurve3d (space @ (units1 :/: units2)))
  where
  lhs ./. rhs = Quotient' lhs rhs

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
  curve ./. value = curve ./. Curve.constant value

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  lhs .<>. rhs =
    Curve.new (compiled lhs .<>. compiled rhs) (derivative lhs .<>. rhs + lhs .<>. derivative rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (VectorCurve3d (space1 @ units1)) (Vector3d (space2 @ units2)) (Curve units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  curve .<>. vector = curve .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication (Vector3d (space1 @ units1)) (VectorCurve3d (space2 @ units2)) (Curve units3)
  where
  lhs <> rhs = Units.specialize (lhs .<>. rhs)

instance
  space1 ~ space2 =>
  DotMultiplication'
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (Curve (units1 :*: units2))
  where
  vector .<>. curve = constant vector .<>. curve

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve3d (space1 @ units)) (Direction3d space2) (Curve units)
  where
  lhs <> rhs = lhs <> Vector3d.unit rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (VectorCurve3d (space2 @ units)) (Curve units)
  where
  lhs <> rhs = Vector3d.unit lhs <> rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 :*: units2)))
  where
  Parametric lhs .><. Parametric rhs = Parametric (lhs .><. rhs)
  lhs .><. rhs = CrossProduct' lhs rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (VectorCurve3d (space1 @ units1))
    (Vector3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 :*: units2)))
  where
  curve .><. vector = curve .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ units3))
  where
  lhs >< rhs = Units.specialize (lhs .><. rhs)

instance
  space1 ~ space2 =>
  CrossMultiplication'
    (Vector3d (space1 @ units1))
    (VectorCurve3d (space2 @ units2))
    (VectorCurve3d (space1 @ (units1 :*: units2)))
  where
  vector .><. curve = constant vector .><. curve

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d (space1 @ units))
    (Direction3d space2)
    (VectorCurve3d (space1 @ units))
  where
  lhs >< rhs = lhs >< Vector3d.unit rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (VectorCurve3d (space2 @ units))
    (VectorCurve3d (space1 @ units))
  where
  lhs >< rhs = Vector3d.unit lhs >< rhs

instance
  unitless ~ Unitless =>
  Composition
    (Curve unitless)
    (VectorCurve3d (space @ units))
    (VectorCurve3d (space @ units))
  where
  outer . inner = new (outer :.: inner)

instance
  unitless ~ Unitless =>
  Interface
    (VectorCurve3d (space @ units) :.: Curve unitless)
    (space @ units)
  where
  evaluateImpl (vectorCurve3d :.: curve1d) tValue =
    evaluate vectorCurve3d (Curve.evaluate curve1d tValue)

  evaluateBoundsImpl (vectorCurve3d :.: curve1d) tRange =
    evaluateBounds vectorCurve3d (Curve.evaluateBounds curve1d tRange)

  derivativeImpl (vectorCurve3d :.: curve1d) =
    (derivative vectorCurve3d . curve1d) * Curve.derivative curve1d

  transformByImpl transform (vectorCurve3d :.: curve1d) =
    new (transformBy transform vectorCurve3d :.: curve1d)

instance
  unitless ~ Unitless =>
  Composition
    (SurfaceFunction unitless)
    (VectorCurve3d (space @ units))
    (VectorSurfaceFunction3d (space @ units))
  where
  curve . function = VectorSurfaceFunction3d.new (curve :.: function)

instance
  unitless ~ Unitless =>
  VectorSurfaceFunction3d.Interface
    (VectorCurve3d (space @ units) :.: SurfaceFunction unitless)
    (space @ units)
  where
  evaluateImpl (curveFunction :.: surfaceFunction) uvPoint =
    evaluate curveFunction $
      SurfaceFunction.evaluate surfaceFunction uvPoint

  evaluateBoundsImpl (curveFunction :.: surfaceFunction) uvBounds =
    evaluateBounds curveFunction $
      SurfaceFunction.evaluateBounds surfaceFunction uvBounds

  derivativeImpl parameter (curveFunction :.: surfaceFunction) =
    (derivative curveFunction . surfaceFunction)
      * SurfaceFunction.derivative parameter surfaceFunction

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
transformBy transform c = case c of
  VectorCurve3d curve -> transformByImpl transform curve
  Parametric expression -> Parametric (Expression.VectorCurve3d.transformBy transform expression)
  Coerce curve -> Coerce (transformBy transform curve)
  Reversed curve -> Reversed (transformBy transform curve)
  XYZ{} -> Transformed transform c
  Negated curve -> Negated (transformBy transform curve)
  Sum lhs rhs -> Sum (transformBy transform lhs) (transformBy transform rhs)
  Difference lhs rhs -> Difference (transformBy transform lhs) (transformBy transform rhs)
  Product1d3d' curve1d curve3d -> Product1d3d' curve1d (transformBy transform curve3d)
  Product3d1d' curve3d curve1d -> Product3d1d' (transformBy transform curve3d) curve1d
  Quotient' curve3d curve1d -> Quotient' (transformBy transform curve3d) curve1d
  CrossProduct'{} -> Transformed transform c
  PlaceIn basis localCurve -> do
    let localTransform = Transform3d.relativeTo (Frame3d Point3d.origin basis) transform
    PlaceIn basis (transformBy localTransform localCurve)
  Transformed existing curve -> do
    let transform1 = Units.erase (Transform3d.toAffine existing)
    let transform2 = Units.erase (Transform3d.toAffine transform)
    Transformed (transform1 >> transform2) curve
  Planar{} -> Transformed transform c

new :: Interface curve (space @ units) => curve -> VectorCurve3d (space @ units)
new = VectorCurve3d

zero :: VectorCurve3d (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> VectorCurve3d (space @ units)
constant = Parametric . Expression.constant

xyz :: Curve units -> Curve units -> Curve units -> VectorCurve3d (space @ units)
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
      let angle = Curve.line a b
      v1 * Curve.cos angle + v2 * Curve.sin angle

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
    Vector3d.xyz (Curve.evaluate x tValue) (Curve.evaluate y tValue) (Curve.evaluate z tValue)
  Negated c -> negate (evaluate c tValue)
  Sum c1 c2 -> evaluate c1 tValue + evaluate c2 tValue
  Difference c1 c2 -> evaluate c1 tValue - evaluate c2 tValue
  Product1d3d' c1 c2 -> Curve.evaluate c1 tValue .*. evaluate c2 tValue
  Product3d1d' c1 c2 -> evaluate c1 tValue .*. Curve.evaluate c2 tValue
  Quotient' c1 c2 -> evaluate c1 tValue ./. Curve.evaluate c2 tValue
  CrossProduct' c1 c2 -> evaluate c1 tValue .><. evaluate c2 tValue
  PlaceIn basis c -> Vector3d.placeIn basis (evaluate c tValue)
  Transformed transform c -> Vector3d.transformBy transform (evaluate c tValue)
  Planar plane curve2d -> Vector2d.placeOn plane (VectorCurve2d.evaluate curve2d tValue)

evaluateBounds :: VectorCurve3d (space @ units) -> Range Unitless -> VectorBounds3d (space @ units)
evaluateBounds curve tRange = case curve of
  VectorCurve3d c -> evaluateBoundsImpl c tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Coerce c -> Units.coerce (evaluateBounds c tRange)
  Reversed c -> evaluateBounds c (1.0 - tRange)
  XYZ x y z ->
    VectorBounds3d
      (Curve.evaluateBounds x tRange)
      (Curve.evaluateBounds y tRange)
      (Curve.evaluateBounds z tRange)
  Negated c -> negate (evaluateBounds c tRange)
  Sum c1 c2 -> evaluateBounds c1 tRange + evaluateBounds c2 tRange
  Difference c1 c2 -> evaluateBounds c1 tRange - evaluateBounds c2 tRange
  Product1d3d' c1 c2 -> Curve.evaluateBounds c1 tRange .*. evaluateBounds c2 tRange
  Product3d1d' c1 c2 -> evaluateBounds c1 tRange .*. Curve.evaluateBounds c2 tRange
  Quotient' c1 c2 -> evaluateBounds c1 tRange ./. Curve.evaluateBounds c2 tRange
  CrossProduct' c1 c2 -> evaluateBounds c1 tRange .><. evaluateBounds c2 tRange
  PlaceIn basis c -> VectorBounds3d.placeIn basis (evaluateBounds c tRange)
  Transformed transform c -> VectorBounds3d.transformBy transform (evaluateBounds c tRange)
  Planar basis curve2d -> VectorBounds2d.placeOn basis (VectorCurve2d.evaluateBounds curve2d tRange)

compiled :: VectorCurve3d (space @ units) -> Compiled (space @ units)
compiled c = case c of
  Parametric expression -> CompiledFunction.concrete expression
  curve -> CompiledFunction.abstract (evaluate curve) (evaluateBounds curve)

derivative ::
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
derivative curve = case curve of
  VectorCurve3d c -> derivativeImpl c
  Parametric expression -> Parametric (Expression.curveDerivative expression)
  Coerce c -> Units.coerce (derivative c)
  Reversed c -> negate (reverse (derivative c))
  XYZ x y z -> XYZ (Curve.derivative x) (Curve.derivative y) (Curve.derivative z)
  Negated c -> -(derivative c)
  Sum c1 c2 -> derivative c1 + derivative c2
  Difference c1 c2 -> derivative c1 - derivative c2
  Product1d3d' c1 c2 -> Curve.derivative c1 .*. c2 + c1 .*. derivative c2
  Product3d1d' c1 c2 -> derivative c1 .*. c2 + c1 .*. Curve.derivative c2
  Quotient' c1 c2 ->
    (derivative c1 .*. c2 - c1 .*. Curve.derivative c2) .!/.! Curve.squared' c2
  CrossProduct' c1 c2 -> derivative c1 .><. c2 + c1 .><. derivative c2
  PlaceIn basis c -> PlaceIn basis (derivative c)
  Transformed transform c -> transformBy transform (derivative c)
  Planar plane curve2d -> Planar plane (VectorCurve2d.derivative curve2d)

reverse ::
  VectorCurve3d (space @ units) ->
  VectorCurve3d (space @ units)
reverse curve = case curve of
  VectorCurve3d _ -> Reversed curve
  Parametric expression -> Parametric (expression . Expression.r)
  Coerce c -> Units.coerce (reverse c)
  Reversed c -> c
  XYZ x y z -> XYZ (Curve.reverse x) (Curve.reverse y) (Curve.reverse z)
  Negated c -> Negated (reverse c)
  Sum c1 c2 -> Sum (reverse c1) (reverse c2)
  Difference c1 c2 -> Difference (reverse c1) (reverse c2)
  Product1d3d' c1 c2 -> Product1d3d' (Curve.reverse c1) (reverse c2)
  Product3d1d' c1 c2 -> Product3d1d' (reverse c1) (Curve.reverse c2)
  Quotient' c1 c2 -> Quotient' (reverse c1) (Curve.reverse c2)
  CrossProduct' c1 c2 -> CrossProduct' (reverse c1) (reverse c2)
  PlaceIn basis c -> PlaceIn basis (reverse c)
  Transformed transform c -> Transformed transform (reverse c)
  Planar plane curve2d -> Planar plane (VectorCurve2d.reverse curve2d)

squaredMagnitude :: Units.Squared units1 units2 => VectorCurve3d (space @ units1) -> Curve units2
squaredMagnitude curve = Units.specialize (squaredMagnitude' curve)

squaredMagnitude' :: VectorCurve3d (space @ units) -> Curve (units :*: units)
squaredMagnitude' curve = do
  let compiledSquaredMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3d.squaredMagnitude'
          Vector3d.squaredMagnitude'
          VectorBounds3d.squaredMagnitude'
          (compiled curve)
  let squaredMagnitudeDerivative = 2.0 * curve .<>. derivative curve
  Curve.new compiledSquaredMagnitude squaredMagnitudeDerivative

unsafeMagnitude :: VectorCurve3d (space @ units) -> Curve units
unsafeMagnitude curve = do
  let compiledMagnitude =
        CompiledFunction.map
          Expression.VectorCurve3d.magnitude
          Vector3d.magnitude
          VectorBounds3d.magnitude
          (compiled curve)
  let magnitudeDerivative self = derivative curve <> (curve / self)
  Curve.recursive compiledMagnitude magnitudeDerivative

data HasZero = HasZero deriving (Eq, Show, Error.Message)

magnitude :: Tolerance units => VectorCurve3d (space @ units) -> Result HasZero (Curve units)
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
  case Tolerance.using Tolerance.squared' (Curve.zeros (squaredMagnitude' curve)) of
    Success zeros1d -> Success (List.map Curve.Zero.location zeros1d)
    Failure Curve.Zeros.ZeroEverywhere -> Failure Zeros.ZeroEverywhere
    Failure Curve.Zeros.HigherOrderZero -> Failure Zeros.HigherOrderZero

xComponent :: VectorCurve3d (space @ units) -> Curve units
xComponent curve = curve <> Direction3d.x

yComponent :: VectorCurve3d (space @ units) -> Curve units
yComponent curve = curve <> Direction3d.y

zComponent :: VectorCurve3d (space @ units) -> Curve units
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
    -- If a curve has a higher-order zero, that still means it has a zero...
    Failure Zeros.HigherOrderZero -> Failure HasZero

isRemovableDegeneracy :: Tolerance units => VectorCurve3d (space @ units) -> Float -> Bool
isRemovableDegeneracy curveDerivative tValue =
  -- A degeneracy (zero value of a vector curve) when computing the direction of that vector curve
  -- is removable at an endpoint if the curve derivative at that endpoint is non-zero,
  -- since in that case we can substitute the curve derivative value for the curve value itself
  (tValue == 0.0 || tValue == 1.0) && evaluate curveDerivative tValue != Vector3d.zero

placeIn ::
  Basis3d global (Defines local) ->
  VectorCurve3d (local @ units) ->
  VectorCurve3d (global @ units)
placeIn globalBasis (Parametric expression) =
  Parametric (Expression.VectorCurve3d.placeIn globalBasis expression)
placeIn globalBasis (PlaceIn basis curve) =
  PlaceIn (Basis3d.placeIn globalBasis basis) curve
placeIn globalBasis curve = PlaceIn globalBasis curve

relativeTo ::
  Basis3d global (Defines local) ->
  VectorCurve3d (global @ units) ->
  VectorCurve3d (local @ units)
relativeTo basis = placeIn (Basis3d.inverse basis)
