-- Needed for 'Curve1d * Vector3d = VectorCurve3d'
-- and 'Vector3d * Curve1d = VectorCurve3d' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Curve1d or Vector3d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorCurve3d.Function
  ( Function (Parametric)
  , Interface (..)
  , new
  , evaluate
  , evaluateBounds
  , derivative
  , zero
  , constant
  , parametric
  , xyz
  , line
  , arc
  , quadraticSpline
  , cubicSpline
  , bezierCurve
  , unsafeMagnitude
  , squaredMagnitude
  , squaredMagnitude'
  , reverse
  , isZero
  , xComponent
  , yComponent
  , zComponent
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , transformBy
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Composition
import OpenSolid.CoordinateSystem (Space)
import OpenSolid.Curve1d.Function qualified as Curve1d.Function
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.Expression.VectorCurve3d qualified as Expression.VectorCurve3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d (Transform3d)
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))
import OpenSolid.VectorBounds3d qualified as VectorBounds3d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Vector3d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> VectorBounds3d coordinateSystem
  derivativeImpl :: function -> Function coordinateSystem
  transformByImpl ::
    Transform3d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Parametric ::
    Expression Float (Vector3d (space @ units)) ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Reversed ::
    Function (space @ units) ->
    Function (space @ units)
  XYZ ::
    Curve1d.Function.Function units ->
    Curve1d.Function.Function units ->
    Curve1d.Function.Function units ->
    Function (space @ units)
  Negated ::
    Function (space @ units) ->
    Function (space @ units)
  Sum ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Difference ::
    Function (space @ units) ->
    Function (space @ units) ->
    Function (space @ units)
  Product1d3d' ::
    Curve1d.Function.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product3d1d' ::
    Function (space @ units1) ->
    Curve1d.Function.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Curve1d.Function.Function units2 ->
    Function (space @ (units1 :/: units2))
  CrossProduct' ::
    Function (space @ units1) ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  PlaceInBasis ::
    Basis3d global (Defines local) ->
    Function (local @ units) ->
    Function (global @ units)
  Transformed ::
    Transform3d.Affine (space @ Unitless) ->
    Function (space @ units) ->
    Function (space @ units)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ units1)) (Function (space2 @ units2))
  where
  coerce (Parametric expression) = Parametric (Units.coerce expression)
  coerce (Coerce function) = Coerce function
  coerce function = Coerce function

instance Interface (Function (space @ units)) (space @ units) where
  evaluateImpl = evaluate
  evaluateBoundsImpl = evaluateBounds
  derivativeImpl = derivative
  transformByImpl = transformBy

instance Negation (Function (space @ units)) where
  negate f = case f of
    Parametric expression -> Parametric -expression
    Coerce function -> Coerce -function
    XYZ x y z -> XYZ -x -y -z
    Negated function -> function
    Difference lhs rhs -> Difference rhs lhs
    Product1d3d' scalarFunction vectorFunction -> Product1d3d' -scalarFunction vectorFunction
    Product3d1d' vectorFunction scalarFunction -> Product3d1d' vectorFunction -scalarFunction
    function -> Negated function

instance Multiplication Sign (Function (space @ units)) (Function (space @ units))

instance Multiplication' Sign (Function (space @ units)) where
  type Sign .*. Function (space @ units) = Function (space @ (Unitless :*: units))
  Positive .*. function = Units.coerce function
  Negative .*. function = Units.coerce -function

instance Multiplication (Function (space @ units)) Sign (Function (space @ units))

instance Multiplication' (Function (space @ units)) Sign where
  type Function (space @ units) .*. Sign = Function (space @ (units :*: Unitless))
  function .*. Positive = Units.coerce function
  function .*. Negative = Units.coerce -function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Function (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Function (space1 @ units1))
  where
  function + vector = function + constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Addition
    (Vector3d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  vector + function = constant vector + function

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Function (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Function (space1 @ units1))
  where
  function - vector = function - constant vector

instance
  (space1 ~ space2, units1 ~ units2) =>
  Subtraction
    (Vector3d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units1))
  where
  vector - function = constant vector - function

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Curve1d.Function.Function units1)
    (Function (space @ units2))
    (Function (space @ units3))

instance Multiplication' (Curve1d.Function.Function units1) (Function (space @ units2)) where
  type
    Curve1d.Function.Function units1 .*. Function (space @ units2) =
      Function (space @ (units1 :*: units2))
  Curve1d.Function.Parametric lhs .*. Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product1d3d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Qty units1) (Function (space @ units2)) (Function (space @ units3))

instance Multiplication' (Qty units1) (Function (space @ units2)) where
  type
    Qty units1 .*. Function (space @ units2) =
      Function (space @ (units1 :*: units2))
  value .*. function = Curve1d.Function.constant value .*. function

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Curve1d.Function.Function units1) (Vector3d (space @ units2)) (Function (space @ units3))

instance Multiplication' (Curve1d.Function.Function units1) (Vector3d (space @ units2)) where
  type
    Curve1d.Function.Function units1 .*. Vector3d (space @ units2) =
      Function (space @ (units1 :*: units2))
  scalarFunction .*. vector = scalarFunction .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Curve1d.Function.Function units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Curve1d.Function.Function units2) where
  type
    Function (space @ units1) .*. Curve1d.Function.Function units2 =
      Function (space @ (units1 :*: units2))
  Parametric lhs .*. Curve1d.Function.Parametric rhs = Parametric (lhs .*. rhs)
  lhs .*. rhs = Product3d1d' lhs rhs

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Qty units2) where
  type
    Function (space @ units1) .*. Qty units2 =
      Function (space @ (units1 :*: units2))
  function .*. value = function .*. Curve1d.Function.constant value

instance
  Units.Product units1 units2 units3 =>
  Multiplication (Vector3d (space @ units1)) (Curve1d.Function.Function units2) (Function (space @ units3))

instance Multiplication' (Vector3d (space @ units1)) (Curve1d.Function.Function units2) where
  type
    Vector3d (space @ units1) .*. Curve1d.Function.Function units2 =
      Function (space @ (units1 :*: units2))
  vector .*. scalarFunction = constant vector .*. scalarFunction

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Curve1d.Function.Function units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Curve1d.Function.Function units2) where
  type
    Function (space @ units1) ./. Curve1d.Function.Function units2 =
      Function (space @ (units1 :/: units2))
  Parametric lhs ./. Curve1d.Function.Parametric rhs = Parametric (lhs ./. rhs)
  lhs ./. rhs = Quotient' lhs rhs

instance
  Units.Quotient units1 units2 units3 =>
  Division (Function (space @ units1)) (Qty units2) (Function (space @ units3))

instance Division' (Function (space @ units1)) (Qty units2) where
  type
    Function (space @ units1) ./. Qty units2 =
      Function (space @ (units1 :/: units2))
  function ./. value = function ./. Curve1d.Function.constant value

data DotProductOf space units1 units2
  = DotProductOf (Function (space @ units1)) (Function (space @ units2))
  deriving (Show)

instance Curve1d.Function.Interface (DotProductOf space units1 units2) (units1 :*: units2) where
  evaluateImpl (DotProductOf lhs rhs) tValue =
    evaluate lhs tValue .<>. evaluate rhs tValue

  evaluateBoundsImpl (DotProductOf lhs rhs) tRange =
    evaluateBounds lhs tRange .<>. evaluateBounds rhs tRange

  derivativeImpl (DotProductOf lhs rhs) =
    derivative lhs .<>. rhs + lhs .<>. derivative rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Function (space1 @ units1) .<>. Function (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  Parametric lhs .<>. Parametric rhs = Curve1d.Function.Parametric (lhs .<>. rhs)
  lhs .<>. rhs = Curve1d.Function.new (DotProductOf lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Function (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function (space1 @ units1)) (Vector3d (space2 @ units2))
  where
  type
    Function (space1 @ units1) .<>. Vector3d (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector3d (space1 @ units1))
    (Function (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector3d (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Vector3d (space1 @ units1) .<>. Function (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  vector .<>. function = constant vector .<>. function

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function (space1 @ units))
    (Direction3d space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function (space1 @ units)) (Direction3d space2)
  where
  type
    Function (space1 @ units) .<>. Direction3d space2 =
      Curve1d.Function.Function (units :*: Unitless)
  function .<>. direction3d = function .<>. Vector3d.unit direction3d

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction3d space1)
    (Function (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction3d space1) (Function (space2 @ units))
  where
  type
    Direction3d space1 .<>. Function (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  direction3d .<>. function = Vector3d.unit direction3d .<>. function

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units3))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Function (space1 @ units1) .><. Function (space2 @ units2) =
      Function (space1 @ (units1 :*: units2))
  Parametric lhs .><. Parametric rhs = Parametric (lhs .><. rhs)
  lhs .><. rhs = CrossProduct' lhs rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Function (space1 @ units1))
    (Vector3d (space2 @ units2))
    (Function (space1 @ units3))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function (space1 @ units1)) (Vector3d (space2 @ units2))
  where
  type
    Function (space1 @ units1) .><. Vector3d (space2 @ units2) =
      Function (space1 @ (units1 :*: units2))
  function .><. vector = function .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector3d (space1 @ units1))
    (Function (space2 @ units2))
    (Function (space1 @ units3))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector3d (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Vector3d (space1 @ units1) .><. Function (space2 @ units2) =
      Function (space1 @ (units1 :*: units2))
  vector .><. function = constant vector .><. function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function (space1 @ units))
    (Direction3d space2)
    (Function (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function (space1 @ units)) (Direction3d space2)
  where
  type
    Function (space1 @ units) .><. Direction3d space2 =
      Function (space1 @ (units :*: Unitless))
  function .><. direction = function .><. Vector3d.unit direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (Function (space2 @ units))
    (Function (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction3d space1) (Function (space2 @ units))
  where
  type
    Direction3d space1 .><. Function (space2 @ units) =
      Function (space1 @ (Unitless :*: units))
  direction .><. function = Vector3d.unit direction .><. function

instance
  Composition
    (Curve1d.Function.Function Unitless)
    (Function (space @ units))
    (Function (space @ units))
  where
  Parametric vectorExpression . Curve1d.Function.Parametric scalarExpression =
    Parametric (vectorExpression . scalarExpression)
  vectorFunction . scalarFunction = new (vectorFunction :.: scalarFunction)

instance
  Interface
    (Function (space @ units) :.: Curve1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (vectorFunction :.: scalarFunction) tValue =
    evaluate vectorFunction (Curve1d.Function.evaluate scalarFunction tValue)

  evaluateBoundsImpl (vectorFunction :.: scalarFunction) tRange =
    evaluateBounds vectorFunction (Curve1d.Function.evaluateBounds scalarFunction tRange)

  derivativeImpl (vectorFunction :.: scalarFunction) =
    (derivative vectorFunction . scalarFunction) * Curve1d.Function.derivative scalarFunction

  transformByImpl transform (vectorFunction :.: scalarFunction) =
    new (transformBy transform vectorFunction :.: scalarFunction)

transformBy ::
  Transform3d tag (space @ translationUnits) ->
  Function (space @ units) ->
  Function (space @ units)
transformBy transform f = do
  let erased = Units.erase (Transform3d.toAffine transform)
  case f of
    Function function -> transformByImpl transform function
    Parametric expression -> Parametric (Expression.VectorCurve3d.transformBy erased expression)
    Coerce function -> Coerce (transformBy transform function)
    Reversed function -> Reversed (transformBy transform function)
    XYZ{} -> Transformed erased f
    Negated function -> Negated (transformBy transform function)
    Sum lhs rhs -> Sum (transformBy transform lhs) (transformBy transform rhs)
    Difference lhs rhs -> Difference (transformBy transform lhs) (transformBy transform rhs)
    Product1d3d' scalarFunction vectorFunction ->
      Product1d3d' scalarFunction (transformBy transform vectorFunction)
    Product3d1d' vectorFunction scalarFunction ->
      Product3d1d' (transformBy transform vectorFunction) scalarFunction
    Quotient' vectorFunction scalarFunction ->
      Quotient' (transformBy transform vectorFunction) scalarFunction
    CrossProduct' lhs rhs -> CrossProduct' (transformBy transform lhs) (transformBy transform rhs)
    PlaceInBasis basis function -> do
      let localTransform = Transform3d.relativeTo (Frame3d.at Point3d.origin basis) transform
      PlaceInBasis basis (transformBy localTransform function)
    Transformed existing function -> Transformed (existing >> erased) function

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector3d.zero

constant :: Vector3d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

parametric :: Expression Float (Vector3d (space @ units)) -> Function (space @ units)
parametric = Parametric

xyz ::
  Curve1d.Function.Function units ->
  Curve1d.Function.Function units ->
  Curve1d.Function.Function units ->
  Function (space @ units)
xyz (Curve1d.Function.Parametric x) (Curve1d.Function.Parametric y) (Curve1d.Function.Parametric z) =
  Parametric (Expression.xyz x y z)
xyz x y z = XYZ x y z

line :: Vector3d (space @ units) -> Vector3d (space @ units) -> Function (space @ units)
line v1 v2 =
  Parametric $
    Expression.VectorCurve3d.constant v1
      + Expression.t * Expression.VectorCurve3d.constant (v2 - v1)

arc ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Angle ->
  Angle ->
  Function (space @ units)
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
  Function (space @ units)
quadraticSpline v1 v2 v3 =
  Parametric (Expression.quadraticSpline v1 v2 v3)

cubicSpline ::
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Vector3d (space @ units) ->
  Function (space @ units)
cubicSpline v1 v2 v3 v4 =
  Parametric (Expression.cubicSpline v1 v2 v3 v4)

bezierCurve :: NonEmpty (Vector3d (space @ units)) -> Function (space @ units)
bezierCurve = Parametric . Expression.bezierCurve

evaluate :: Function (space @ units) -> Float -> Vector3d (space @ units)
evaluate f tValue = case f of
  Function function -> evaluateImpl function tValue
  Parametric expression -> Expression.evaluate expression tValue
  Coerce function -> Units.coerce (evaluate function tValue)
  Reversed function -> evaluate function (1.0 - tValue)
  XYZ x y z ->
    Vector3d.xyz
      (Curve1d.Function.evaluate x tValue)
      (Curve1d.Function.evaluate y tValue)
      (Curve1d.Function.evaluate z tValue)
  Negated function -> negate (evaluate function tValue)
  Sum lhs rhs -> evaluate lhs tValue + evaluate rhs tValue
  Difference lhs rhs -> evaluate lhs tValue - evaluate rhs tValue
  Product1d3d' scalarFunction vectorFunction ->
    Curve1d.Function.evaluate scalarFunction tValue .*. evaluate vectorFunction tValue
  Product3d1d' vectorFunction scalarFunction ->
    evaluate vectorFunction tValue .*. Curve1d.Function.evaluate scalarFunction tValue
  Quotient' vectorFunction scalarFunction ->
    evaluate vectorFunction tValue ./. Curve1d.Function.evaluate scalarFunction tValue
  CrossProduct' lhs rhs -> evaluate lhs tValue .><. evaluate rhs tValue
  PlaceInBasis basis function -> Vector3d.placeInBasis basis (evaluate function tValue)
  Transformed transform function -> Vector3d.transformBy transform (evaluate function tValue)

evaluateBounds :: Function (space @ units) -> Range Unitless -> VectorBounds3d (space @ units)
evaluateBounds f tRange = case f of
  Function function -> evaluateBoundsImpl function tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Coerce function -> Units.coerce (evaluateBounds function tRange)
  Reversed function -> evaluateBounds function (1.0 - tRange)
  XYZ x y z ->
    VectorBounds3d
      (Curve1d.Function.evaluateBounds x tRange)
      (Curve1d.Function.evaluateBounds y tRange)
      (Curve1d.Function.evaluateBounds z tRange)
  Negated function -> negate (evaluateBounds function tRange)
  Sum lhs rhs -> evaluateBounds lhs tRange + evaluateBounds rhs tRange
  Difference lhs rhs -> evaluateBounds lhs tRange - evaluateBounds rhs tRange
  Product1d3d' scalarFunction vectorFunction ->
    Curve1d.Function.evaluateBounds scalarFunction tRange .*. evaluateBounds vectorFunction tRange
  Product3d1d' vectorFunction scalarFunction ->
    evaluateBounds vectorFunction tRange .*. Curve1d.Function.evaluateBounds scalarFunction tRange
  Quotient' vectorFunction scalarFunction ->
    evaluateBounds vectorFunction tRange ./. Curve1d.Function.evaluateBounds scalarFunction tRange
  CrossProduct' lhs rhs -> evaluateBounds lhs tRange .><. evaluateBounds rhs tRange
  PlaceInBasis basis function -> VectorBounds3d.placeInBasis basis (evaluateBounds function tRange)
  Transformed transform function ->
    VectorBounds3d.transformBy transform (evaluateBounds function tRange)

derivative :: Function (space @ units) -> Function (space @ units)
derivative f = case f of
  Function function -> derivativeImpl function
  Parametric expression -> Parametric (Expression.curveDerivative expression)
  Coerce function -> Units.coerce (derivative function)
  Reversed function -> negate (reverse (derivative function))
  XYZ x y z ->
    XYZ
      (Curve1d.Function.derivative x)
      (Curve1d.Function.derivative y)
      (Curve1d.Function.derivative z)
  Negated function -> -(derivative function)
  Sum lhs rhs -> derivative lhs + derivative rhs
  Difference lhs rhs -> derivative lhs - derivative rhs
  Product1d3d' scalarFunction vectorFunction ->
    Curve1d.Function.derivative scalarFunction .*. vectorFunction
      + scalarFunction .*. derivative vectorFunction
  Product3d1d' vectorFunction scalarFunction ->
    derivative vectorFunction .*. scalarFunction
      + vectorFunction .*. Curve1d.Function.derivative scalarFunction
  Quotient' vectorFunction scalarFunction ->
    ( derivative vectorFunction .*. scalarFunction
        - vectorFunction .*. Curve1d.Function.derivative scalarFunction
    )
      .!/.! Curve1d.Function.squared' scalarFunction
  CrossProduct' lhs rhs -> derivative lhs .><. rhs + lhs .><. derivative rhs
  PlaceInBasis basis function -> PlaceInBasis basis (derivative function)
  Transformed transform function -> transformBy transform (derivative function)

reverse :: Function (space @ units) -> Function (space @ units)
reverse f = case f of
  Function _ -> Reversed f
  Parametric expression -> Parametric (expression . Expression.r)
  Coerce function -> Units.coerce (reverse function)
  Reversed function -> function
  XYZ x y z ->
    XYZ (Curve1d.Function.reverse x) (Curve1d.Function.reverse y) (Curve1d.Function.reverse z)
  Negated function -> Negated (reverse function)
  Sum lhs rhs -> Sum (reverse lhs) (reverse rhs)
  Difference lhs rhs -> Difference (reverse lhs) (reverse rhs)
  Product1d3d' scalarFunction vectorFunction ->
    Product1d3d' (Curve1d.Function.reverse scalarFunction) (reverse vectorFunction)
  Product3d1d' vectorFunction scalarFunction ->
    Product3d1d' (reverse vectorFunction) (Curve1d.Function.reverse scalarFunction)
  Quotient' vectorFunction scalarFunction ->
    Quotient' (reverse vectorFunction) (Curve1d.Function.reverse scalarFunction)
  CrossProduct' lhs rhs -> CrossProduct' (reverse lhs) (reverse rhs)
  PlaceInBasis basis function -> PlaceInBasis basis (reverse function)
  Transformed transform function -> Transformed transform (reverse function)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (Function coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

instance Curve1d.Function.Interface (SquaredMagnitude' (space @ units)) (units :*: units) where
  evaluateImpl (SquaredMagnitude' function) tValue =
    Vector3d.squaredMagnitude' (evaluate function tValue)

  evaluateBoundsImpl (SquaredMagnitude' function) tRange =
    VectorBounds3d.squaredMagnitude' (evaluateBounds function tRange)

  derivativeImpl (SquaredMagnitude' function) =
    2.0 * function .<>. derivative function

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Function (space @ units1) ->
  Curve1d.Function.Function units2
squaredMagnitude function = Units.specialize (squaredMagnitude' function)

squaredMagnitude' :: Function (space @ units) -> Curve1d.Function.Function (units :*: units)
squaredMagnitude' (Parametric expression) =
  Curve1d.Function.Parametric (Expression.VectorCurve3d.squaredMagnitude' expression)
squaredMagnitude' function = Curve1d.Function.new (SquaredMagnitude' function)

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (Function coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

instance Curve1d.Function.Interface (NonZeroMagnitude (space @ units)) units where
  evaluateImpl (NonZeroMagnitude function) tValue =
    Vector3d.magnitude (evaluate function tValue)

  evaluateBoundsImpl (NonZeroMagnitude function) tRange =
    VectorBounds3d.magnitude (evaluateBounds function tRange)

  derivativeImpl (NonZeroMagnitude function) =
    (derivative function .<>. function) .!/! Curve1d.Function.new (NonZeroMagnitude function)

unsafeMagnitude :: Function (space @ units) -> Curve1d.Function.Function units
unsafeMagnitude (Parametric expression) =
  Curve1d.Function.Parametric (Expression.VectorCurve3d.magnitude expression)
unsafeMagnitude function = Curve1d.Function.new (NonZeroMagnitude function)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

isZero :: Tolerance units => Function (space @ units) -> Bool
isZero function = Tolerance.using Tolerance.squared' (squaredMagnitude' function ~= Qty.zero)

xComponent :: Function (space @ units) -> Curve1d.Function.Function units
xComponent function = function <> Direction3d.x

yComponent :: Function (space @ units) -> Curve1d.Function.Function units
yComponent function = function <> Direction3d.y

zComponent :: Function (space @ units) -> Curve1d.Function.Function units
zComponent function = function <> Direction3d.z

placeIn ::
  Frame3d (global @ originUnits) (Defines local) ->
  Function (local @ units) ->
  Function (global @ units)
placeIn globalFrame = placeInBasis (Frame3d.basis globalFrame)

relativeTo ::
  Frame3d (global @ originUnits) (Defines local) ->
  Function (global @ units) ->
  Function (local @ units)
relativeTo globalFrame = relativeToBasis (Frame3d.basis globalFrame)

placeInBasis ::
  Basis3d global (Defines local) ->
  Function (local @ units) ->
  Function (global @ units)
placeInBasis globalBasis (Parametric expression) =
  Parametric (Expression.VectorCurve3d.placeInBasis globalBasis expression)
placeInBasis globalBasis (PlaceInBasis basis function) =
  PlaceInBasis (Basis3d.placeInBasis globalBasis basis) function
placeInBasis globalBasis function = PlaceInBasis globalBasis function

relativeToBasis ::
  Basis3d global (Defines local) ->
  Function (global @ units) ->
  Function (local @ units)
relativeToBasis basis = placeInBasis (Basis3d.inverse basis)
