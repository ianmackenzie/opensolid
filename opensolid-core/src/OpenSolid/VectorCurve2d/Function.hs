-- Needed for 'Curve1d * Vector2d = VectorCurve2d'
-- and 'Vector2d * Curve1d = VectorCurve2d' instances,
-- which lead to unresolvable circular dependencies
-- if they're defined in the Curve1d or Vector2d modules
-- and really conceptually make more sense
-- to define in this module anyways
{-# OPTIONS_GHC -Wno-orphans #-}

module OpenSolid.VectorCurve2d.Function
  ( Function (Parametric)
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
  , unsafeMagnitude
  , squaredMagnitude
  , squaredMagnitude'
  , reverse
  , isZero
  , HasZero (HasZero)
  , xComponent
  , yComponent
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
import OpenSolid.Curve1d.Function qualified as Curve1d.Function
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Error qualified as Error
import OpenSolid.Expression (Expression)
import OpenSolid.Expression qualified as Expression
import OpenSolid.Expression.Curve1d qualified as Expression.Curve1d
import OpenSolid.Expression.VectorCurve2d qualified as Expression.VectorCurve2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.Range (Range)
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d (Transform2d)
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d

class
  Show function =>
  Interface function (coordinateSystem :: CoordinateSystem)
    | function -> coordinateSystem
  where
  evaluateImpl :: function -> Float -> Vector2d coordinateSystem
  evaluateBoundsImpl :: function -> Range Unitless -> VectorBounds2d coordinateSystem
  derivativeImpl :: function -> Function coordinateSystem
  transformByImpl ::
    Transform2d tag (Space coordinateSystem @ translationUnits) ->
    function ->
    Function coordinateSystem

data Function (coordinateSystem :: CoordinateSystem) where
  Function ::
    Interface function (space @ units) =>
    function ->
    Function (space @ units)
  Parametric ::
    Expression Float (Vector2d (space @ units)) ->
    Function (space @ units)
  Coerce ::
    Function (space @ units1) ->
    Function (space @ units2)
  Reversed ::
    Function (space @ units) ->
    Function (space @ units)
  XY ::
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
  Product1d2d' ::
    Curve1d.Function.Function units1 ->
    Function (space @ units2) ->
    Function (space @ (units1 :*: units2))
  Product2d1d' ::
    Function (space @ units1) ->
    Curve1d.Function.Function units2 ->
    Function (space @ (units1 :*: units2))
  Quotient' ::
    Function (space @ units1) ->
    Curve1d.Function.Function units2 ->
    Function (space @ (units1 :/: units2))
  PlaceInBasis ::
    Basis2d global (Defines local) ->
    Function (local @ units) ->
    Function (global @ units)
  Transformed ::
    Transform2d.Affine (space @ Unitless) ->
    Function (space @ units) ->
    Function (space @ units)

deriving instance Show (Function (space @ units))

instance HasUnits (Function (space @ units)) where
  type UnitsOf (Function (space @ units)) = units

instance
  space1 ~ space2 =>
  Units.Coercion (Function (space1 @ unitsA)) (Function (space2 @ unitsB))
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
  negate function = case function of
    Parametric expression -> Parametric -expression
    Coerce f -> Coerce -f
    XY x y -> XY -x -y
    Negated f -> f
    Difference lhs rhs -> Difference rhs lhs
    Product1d2d' scalarFunction vectorFunction -> Product1d2d' -scalarFunction vectorFunction
    Product2d1d' vectorFunction scalarFunction -> Product2d1d' vectorFunction -scalarFunction
    _ -> Negated function

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
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  Parametric lhs + Parametric rhs = Parametric (lhs + rhs)
  lhs + rhs = Sum lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Function (space @ units))
    (Vector2d (space_ @ units_))
    (Function (space @ units))
  where
  function + vector = function + constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Addition
    (Vector2d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  vector + function = constant vector + function

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
  where
  Parametric lhs - Parametric rhs = Parametric (lhs - rhs)
  lhs - rhs = Difference lhs rhs

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Function (space @ units))
    (Vector2d (space_ @ units_))
    (Function (space @ units))
  where
  function - vector = function - constant vector

instance
  ( space ~ space_
  , units ~ units_
  ) =>
  Subtraction
    (Vector2d (space @ units))
    (Function (space_ @ units_))
    (Function (space @ units))
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
  scalarFunction .*. vectorFunction = Product1d2d' scalarFunction vectorFunction

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
  Multiplication
    (Curve1d.Function.Function units1)
    (Vector2d (space @ units2))
    (Function (space @ units3))

instance Multiplication' (Curve1d.Function.Function units1) (Vector2d (space @ units2)) where
  type
    Curve1d.Function.Function units1 .*. Vector2d (space @ units2) =
      Function (space @ (units1 :*: units2))
  scalarFunction .*. vector = scalarFunction .*. constant vector

instance
  Units.Product units1 units2 units3 =>
  Multiplication
    (Function (space @ units1))
    (Curve1d.Function.Function units2)
    (Function (space @ units3))

instance Multiplication' (Function (space @ units1)) (Curve1d.Function.Function units2) where
  type
    Function (space @ units1) .*. Curve1d.Function.Function units2 =
      Function (space @ (units1 :*: units2))
  Parametric lhs .*. Curve1d.Function.Parametric rhs = Parametric (lhs .*. rhs)
  vectorFunction .*. scalarFunction = Product2d1d' vectorFunction scalarFunction

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
  Multiplication
    (Vector2d (space @ units1))
    (Curve1d.Function.Function units2)
    (Function (space @ units3))

instance Multiplication' (Vector2d (space @ units1)) (Curve1d.Function.Function units2) where
  type
    Vector2d (space @ units1) .*. Curve1d.Function.Function units2 =
      Function (space @ (units1 :*: units2))
  vector .*. function = constant vector .*. function

instance
  Units.Quotient units1 units2 units3 =>
  Division
    (Function (space @ units1))
    (Curve1d.Function.Function units2)
    (Function (space @ units3))

instance Division' (Function (space @ units1)) (Curve1d.Function.Function units2) where
  type
    Function (space @ units1) ./. Curve1d.Function.Function units2 =
      Function (space @ (units1 :/: units2))
  Parametric lhs ./. Curve1d.Function.Parametric rhs = Parametric (lhs ./. rhs)
  vectorFunction ./. scalarFunction = Quotient' vectorFunction scalarFunction

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
  evaluateImpl (DotProductOf c1 c2) tValue =
    evaluate c1 tValue .<>. evaluate c2 tValue

  evaluateBoundsImpl (DotProductOf c1 c2) tRange =
    evaluateBounds c1 tRange .<>. evaluateBounds c2 tRange

  derivativeImpl (DotProductOf c1 c2) =
    derivative c1 .<>. c2 + c1 .<>. derivative c2

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
    (Vector2d (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function (space1 @ units1)) (Vector2d (space2 @ units2))
  where
  type
    Function (space1 @ units1) .<>. Vector2d (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  function .<>. vector = function .<>. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  DotMultiplication
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector2d (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Vector2d (space1 @ units1) .<>. Function (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  vector .<>. function = constant vector .<>. function

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function (space1 @ units))
    (Direction2d space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function (space1 @ units)) (Direction2d space2)
  where
  type
    Function (space1 @ units) .<>. Direction2d space2 =
      Curve1d.Function.Function (units :*: Unitless)
  function .<>. direction2d = function .<>. Vector2d.unit direction2d

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (Function (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (Function (space2 @ units))
  where
  type
    Direction2d space1 .<>. Function (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  direction2d .<>. function = Vector2d.unit direction2d .<>. function

data CrossProductOf space units1 units2
  = CrossProductOf (Function (space @ units1)) (Function (space @ units2))
  deriving (Show)

instance Curve1d.Function.Interface (CrossProductOf space units1 units2) (units1 :*: units2) where
  evaluateImpl (CrossProductOf lhs rhs) tValue =
    evaluate lhs tValue .><. evaluate rhs tValue

  evaluateBoundsImpl (CrossProductOf lhs rhs) tRange =
    evaluateBounds lhs tRange .><. evaluateBounds rhs tRange

  derivativeImpl (CrossProductOf lhs rhs) =
    derivative lhs .><. rhs + lhs .><. derivative rhs

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Function (space1 @ units1))
    (Function (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Function (space1 @ units1) .><. Function (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  Parametric lhs .><. Parametric rhs = Curve1d.Function.Parametric (lhs .><. rhs)
  lhs .><. rhs = Curve1d.Function.new (CrossProductOf lhs rhs)

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Function (space1 @ units1))
    (Vector2d (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function (space1 @ units1)) (Vector2d (space2 @ units2))
  where
  type
    Function (space1 @ units1) .><. Vector2d (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  function .><. vector = function .><. constant vector

instance
  (Units.Product units1 units2 units3, space1 ~ space2) =>
  CrossMultiplication
    (Vector2d (space1 @ units1))
    (Function (space2 @ units2))
    (Curve1d.Function.Function units3)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector2d (space1 @ units1)) (Function (space2 @ units2))
  where
  type
    Vector2d (space1 @ units1) .><. Function (space2 @ units2) =
      Curve1d.Function.Function (units1 :*: units2)
  vector .><. function = constant vector .><. function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function (space1 @ units))
    (Direction2d space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function (space1 @ units)) (Direction2d space2)
  where
  type
    Function (space1 @ units) .><. Direction2d space2 =
      Curve1d.Function.Function (units :*: Unitless)
  function .><. direction2d = function .><. Vector2d.unit direction2d

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (Function (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (Function (space2 @ units))
  where
  type
    Direction2d space1 .><. Function (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  direction2d .><. function = Vector2d.unit direction2d .><. function

instance
  Composition
    (Curve1d.Function.Function Unitless)
    (Function (space @ units))
    (Function (space @ units))
  where
  Parametric outer . Curve1d.Function.Parametric inner = Parametric (outer . inner)
  outer . inner = new (outer :.: inner)

instance
  Interface
    (Function (space @ units) :.: Curve1d.Function.Function Unitless)
    (space @ units)
  where
  evaluateImpl (outer :.: inner) tValue =
    evaluate outer (Curve1d.Function.evaluate inner tValue)

  evaluateBoundsImpl (outer :.: inner) tRange =
    evaluateBounds outer (Curve1d.Function.evaluateBounds inner tRange)

  derivativeImpl (outer :.: inner) =
    (derivative outer . inner) * Curve1d.Function.derivative inner

  transformByImpl transform (outer :.: inner) =
    new (transformBy transform outer :.: inner)

transformBy ::
  Transform2d tag (space @ translationUnits) ->
  Function (space @ units) ->
  Function (space @ units)
transformBy transform function = do
  let erased = Units.erase (Transform2d.toAffine transform)
  case function of
    Function f -> transformByImpl transform f
    Parametric expression -> Parametric (Expression.VectorCurve2d.transformBy erased expression)
    Coerce f -> Coerce (transformBy transform f)
    Reversed f -> Reversed (transformBy transform f)
    XY _ _ -> Transformed erased function
    Negated f -> Negated (transformBy transform f)
    Sum lhs rhs -> Sum (transformBy transform lhs) (transformBy transform rhs)
    Difference lhs rhs -> Difference (transformBy transform lhs) (transformBy transform rhs)
    Product1d2d' scalarFunction vectorFunction ->
      Product1d2d' scalarFunction (transformBy transform vectorFunction)
    Product2d1d' vectorFunction scalarFunction ->
      Product2d1d' (transformBy transform vectorFunction) scalarFunction
    Quotient' vectorFunction scalarFunction ->
      Quotient' (transformBy transform vectorFunction) scalarFunction
    PlaceInBasis basis f -> do
      let localTransform = Transform2d.relativeTo (Frame2d.at Point2d.origin basis) transform
      PlaceInBasis basis (transformBy localTransform f)
    Transformed existing f -> Transformed (existing >> erased) f

rotateBy :: forall space units. Angle -> Function (space @ units) -> Function (space @ units)
rotateBy angle = transformBy (Transform2d.rotateAround (Point2d.origin @space @units) angle)

new :: Interface function (space @ units) => function -> Function (space @ units)
new = Function

zero :: Function (space @ units)
zero = constant Vector2d.zero

constant :: Vector2d (space @ units) -> Function (space @ units)
constant = Parametric . Expression.constant

xy :: Curve1d.Function.Function units -> Curve1d.Function.Function units -> Function (space @ units)
xy (Curve1d.Function.Parametric x) (Curve1d.Function.Parametric y) = Parametric (Expression.xy x y)
xy x y = XY x y

line :: Vector2d (space @ units) -> Vector2d (space @ units) -> Function (space @ units)
line v1 v2 =
  Parametric $
    Expression.VectorCurve2d.constant v1
      + Expression.t * Expression.VectorCurve2d.constant (v2 - v1)

arc ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Angle ->
  Angle ->
  Function (space @ units)
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
  Function (space @ units)
quadraticSpline v1 v2 v3 =
  Parametric (Expression.quadraticSpline v1 v2 v3)

cubicSpline ::
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Vector2d (space @ units) ->
  Function (space @ units)
cubicSpline v1 v2 v3 v4 =
  Parametric (Expression.cubicSpline v1 v2 v3 v4)

bezierCurve :: NonEmpty (Vector2d (space @ units)) -> Function (space @ units)
bezierCurve = Parametric . Expression.bezierCurve

startValue :: Function (space @ units) -> Vector2d (space @ units)
startValue function = evaluate function 0.0

endValue :: Function (space @ units) -> Vector2d (space @ units)
endValue function = evaluate function 1.0

evaluate :: Function (space @ units) -> Float -> Vector2d (space @ units)
evaluate function tValue = case function of
  Function f -> evaluateImpl f tValue
  Parametric expression -> Expression.evaluate expression tValue
  Coerce f -> Units.coerce (evaluate f tValue)
  Reversed f -> evaluate f (1.0 - tValue)
  XY x y -> Vector2d.xy (Curve1d.Function.evaluate x tValue) (Curve1d.Function.evaluate y tValue)
  Negated f -> negate (evaluate f tValue)
  Sum lhs rhs -> evaluate lhs tValue + evaluate rhs tValue
  Difference lhs rhs -> evaluate lhs tValue - evaluate rhs tValue
  Product1d2d' scalarFunction vectorFunction ->
    Curve1d.Function.evaluate scalarFunction tValue .*. evaluate vectorFunction tValue
  Product2d1d' vectorFunction scalarFunction ->
    evaluate vectorFunction tValue .*. Curve1d.Function.evaluate scalarFunction tValue
  Quotient' vectorFunction scalarFunction ->
    evaluate vectorFunction tValue ./. Curve1d.Function.evaluate scalarFunction tValue
  PlaceInBasis basis f -> Vector2d.placeInBasis basis (evaluate f tValue)
  Transformed transform f -> Vector2d.transformBy transform (evaluate f tValue)

evaluateBounds :: Function (space @ units) -> Range Unitless -> VectorBounds2d (space @ units)
evaluateBounds function tRange = case function of
  Function f -> evaluateBoundsImpl f tRange
  Parametric expression -> Expression.evaluateBounds expression tRange
  Coerce f -> Units.coerce (evaluateBounds f tRange)
  Reversed f -> evaluateBounds f (1.0 - tRange)
  XY x y ->
    VectorBounds2d
      (Curve1d.Function.evaluateBounds x tRange)
      (Curve1d.Function.evaluateBounds y tRange)
  Negated f -> negate (evaluateBounds f tRange)
  Sum lhs rhs -> evaluateBounds lhs tRange + evaluateBounds rhs tRange
  Difference lhs rhs -> evaluateBounds lhs tRange - evaluateBounds rhs tRange
  Product1d2d' scalarFunction vectorFunction ->
    Curve1d.Function.evaluateBounds scalarFunction tRange .*. evaluateBounds vectorFunction tRange
  Product2d1d' vectorFunction scalarFunction ->
    evaluateBounds vectorFunction tRange .*. Curve1d.Function.evaluateBounds scalarFunction tRange
  Quotient' vectorFunction scalarFunction ->
    evaluateBounds vectorFunction tRange ./. Curve1d.Function.evaluateBounds scalarFunction tRange
  PlaceInBasis basis f -> VectorBounds2d.placeInBasis basis (evaluateBounds f tRange)
  Transformed transform f -> VectorBounds2d.transformBy transform (evaluateBounds f tRange)

derivative ::
  Function (space @ units) ->
  Function (space @ units)
derivative function = case function of
  Function f -> derivativeImpl f
  Parametric expression -> Parametric (Expression.curveDerivative expression)
  Coerce f -> Units.coerce (derivative f)
  Reversed f -> negate (reverse (derivative f))
  XY x y -> XY (Curve1d.Function.derivative x) (Curve1d.Function.derivative y)
  Negated f -> -(derivative f)
  Sum lhs rhs -> derivative lhs + derivative rhs
  Difference lhs rhs -> derivative lhs - derivative rhs
  Product1d2d' scalarFunction vectorFunction ->
    Curve1d.Function.derivative scalarFunction .*. vectorFunction
      + scalarFunction .*. derivative vectorFunction
  Product2d1d' vectorFunction scalarFunction ->
    derivative vectorFunction .*. scalarFunction
      + vectorFunction .*. Curve1d.Function.derivative scalarFunction
  Quotient' vectorFunction scalarFunction ->
    ( derivative vectorFunction .*. scalarFunction
        - vectorFunction .*. Curve1d.Function.derivative scalarFunction
    )
      .!/.! Curve1d.Function.squared' scalarFunction
  PlaceInBasis basis f -> PlaceInBasis basis (derivative f)
  Transformed transform f -> transformBy transform (derivative f)

reverse ::
  Function (space @ units) ->
  Function (space @ units)
reverse function = case function of
  Function _ -> Reversed function
  Parametric expression -> Parametric (expression . Expression.r)
  Coerce f -> Units.coerce (reverse f)
  Reversed f -> f
  XY x y -> XY (Curve1d.Function.reverse x) (Curve1d.Function.reverse y)
  Negated f -> Negated (reverse f)
  Sum lhs rhs -> Sum (reverse lhs) (reverse rhs)
  Difference lhs rhs -> Difference (reverse lhs) (reverse rhs)
  Product1d2d' scalarFunction vectorFunction ->
    Product1d2d' (Curve1d.Function.reverse scalarFunction) (reverse vectorFunction)
  Product2d1d' vectorFunction scalarFunction ->
    Product2d1d' (reverse vectorFunction) (Curve1d.Function.reverse scalarFunction)
  Quotient' vectorFunction scalarFunction ->
    Quotient' (reverse vectorFunction) (Curve1d.Function.reverse scalarFunction)
  PlaceInBasis basis f -> PlaceInBasis basis (reverse f)
  Transformed transform f -> Transformed transform (reverse f)

newtype SquaredMagnitude' (coordinateSystem :: CoordinateSystem)
  = SquaredMagnitude' (Function coordinateSystem)

deriving instance Show (SquaredMagnitude' (space @ units))

instance Curve1d.Function.Interface (SquaredMagnitude' (space @ units)) (units :*: units) where
  evaluateImpl (SquaredMagnitude' function) tValue =
    Vector2d.squaredMagnitude' (evaluate function tValue)

  evaluateBoundsImpl (SquaredMagnitude' function) tRange =
    VectorBounds2d.squaredMagnitude' (evaluateBounds function tRange)

  derivativeImpl (SquaredMagnitude' function) =
    2.0 * function .<>. derivative function

squaredMagnitude ::
  Units.Squared units1 units2 =>
  Function (space @ units1) ->
  Curve1d.Function.Function units2
squaredMagnitude function = Units.specialize (squaredMagnitude' function)

squaredMagnitude' :: Function (space @ units) -> Curve1d.Function.Function (units :*: units)
squaredMagnitude' (Parametric expression) =
  Curve1d.Function.Parametric (Expression.VectorCurve2d.squaredMagnitude' expression)
squaredMagnitude' function = Curve1d.Function.new (SquaredMagnitude' function)

newtype NonZeroMagnitude (coordinateSystem :: CoordinateSystem)
  = NonZeroMagnitude (Function coordinateSystem)

deriving instance Show (NonZeroMagnitude (space @ units))

instance Curve1d.Function.Interface (NonZeroMagnitude (space @ units)) units where
  evaluateImpl (NonZeroMagnitude function) tValue =
    Vector2d.magnitude (evaluate function tValue)

  evaluateBoundsImpl (NonZeroMagnitude function) tRange =
    VectorBounds2d.magnitude (evaluateBounds function tRange)

  derivativeImpl (NonZeroMagnitude function) =
    (derivative function .<>. function) .!/! Curve1d.Function.new (NonZeroMagnitude function)

unsafeMagnitude :: Function (space @ units) -> Curve1d.Function.Function units
unsafeMagnitude (Parametric expression) =
  Curve1d.Function.Parametric (Expression.VectorCurve2d.magnitude expression)
unsafeMagnitude function = Curve1d.Function.new (NonZeroMagnitude function)

data HasZero = HasZero deriving (Eq, Show, Error.Message)

isZero :: Tolerance units => Function (space @ units) -> Bool
isZero function = Tolerance.using Tolerance.squared' (squaredMagnitude' function ~= Qty.zero)

xComponent :: Function (space @ units) -> Curve1d.Function.Function units
xComponent function = function <> Direction2d.x

yComponent :: Function (space @ units) -> Curve1d.Function.Function units
yComponent function = function <> Direction2d.y

placeIn ::
  Frame2d (global @ originUnits) (Defines local) ->
  Function (local @ units) ->
  Function (global @ units)
placeIn globalFrame = placeInBasis (Frame2d.basis globalFrame)

relativeTo ::
  Frame2d (global @ originUnits) (Defines local) ->
  Function (global @ units) ->
  Function (local @ units)
relativeTo globalFrame = relativeToBasis (Frame2d.basis globalFrame)

placeInBasis ::
  Basis2d global (Defines local) ->
  Function (local @ units) ->
  Function (global @ units)
placeInBasis globalBasis (Parametric expression) =
  Parametric (Expression.VectorCurve2d.placeInBasis globalBasis expression)
placeInBasis globalBasis (PlaceInBasis basis function) =
  PlaceInBasis (Basis2d.placeInBasis globalBasis basis) function
placeInBasis globalBasis function =
  PlaceInBasis globalBasis function

relativeToBasis ::
  Basis2d global (Defines local) ->
  Function (global @ units) ->
  Function (local @ units)
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
