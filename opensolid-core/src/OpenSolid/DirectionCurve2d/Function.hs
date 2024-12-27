module OpenSolid.DirectionCurve2d.Function
  ( Function
  , unsafe
  , unwrap
  , startValue
  , endValue
  , evaluate
  , evaluateBounds
  , derivative
  , constant
  , arc
  , reverse
  , xComponent
  , yComponent
  , placeIn
  , placeInBasis
  , relativeTo
  , relativeToBasis
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Basis2d (Basis2d)
import OpenSolid.Basis2d qualified as Basis2d
import OpenSolid.Curve1d.Function qualified as Curve1d.Function
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionBounds2d (DirectionBounds2d)
import OpenSolid.DirectionBounds2d qualified as DirectionBounds2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorCurve2d.Function qualified as VectorCurve2d.Function

newtype Function space = Function (VectorCurve2d.Function.Function (space @ Unitless))
  deriving (Show)

instance HasUnits (Function space) where
  type UnitsOf (Function space) = Unitless

instance
  space1 ~ space2 =>
  Units.Coercion (Function space1) (Function space2)
  where
  coerce = identity

unsafe :: VectorCurve2d.Function.Function (space @ Unitless) -> Function space
unsafe = Function

unwrap :: Function space -> VectorCurve2d.Function.Function (space @ Unitless)
unwrap (Function function) = function

startValue :: Function space -> Direction2d space
startValue function = evaluate function 0.0

endValue :: Function space -> Direction2d space
endValue function = evaluate function 1.0

evaluate :: Function space -> Float -> Direction2d space
evaluate (Function function) tValue =
  Direction2d.unsafe (VectorCurve2d.Function.evaluate function tValue)

evaluateBounds :: Function space -> Range Unitless -> DirectionBounds2d space
evaluateBounds (Function function) tRange =
  DirectionBounds2d.unsafe (VectorCurve2d.Function.evaluateBounds function tRange)

derivative :: Function space -> VectorCurve2d.Function.Function (space @ Unitless)
derivative (Function function) = VectorCurve2d.Function.derivative function

constant :: Direction2d space -> Function space
constant direction = Function (VectorCurve2d.Function.constant (Vector2d.unit direction))

arc :: Angle -> Angle -> Function space
arc a b = Function (VectorCurve2d.Function.arc (Vector2d.xy 1.0 0.0) (Vector2d.xy 0.0 1.0) a b)

reverse :: Function space -> Function space
reverse (Function function) = Function (VectorCurve2d.Function.reverse function)

instance Negation (Function space) where
  negate (Function function) = Function (negate function)

instance Multiplication Sign (Function space) (Function space)

instance Multiplication' Sign (Function space) where
  type Sign .*. Function space = Function space
  Positive .*. function = function
  Negative .*. function = -function

instance Multiplication (Function space) Sign (Function space)

instance Multiplication' (Function space) Sign where
  type Function space .*. Sign = Function space
  function .*. Positive = function
  function .*. Negative = -function

instance
  Multiplication
    (Qty units)
    (Function space)
    (VectorCurve2d.Function.Function (space @ units))

instance Multiplication' (Qty units) (Function space) where
  type
    Qty units .*. Function space =
      VectorCurve2d.Function.Function (space @ (units :*: Unitless))
  value .*. Function function = value .*. function

instance
  Multiplication
    (Function space)
    (Qty units)
    (VectorCurve2d.Function.Function (space @ units))

instance Multiplication' (Function space) (Qty units) where
  type
    Function space .*. Qty units =
      VectorCurve2d.Function.Function (space @ (Unitless :*: units))
  Function function .*. value = function .*. value

instance
  Multiplication
    (Curve1d.Function.Function units)
    (Function space)
    (VectorCurve2d.Function.Function (space @ units))

instance Multiplication' (Curve1d.Function.Function units) (Function space) where
  type
    Curve1d.Function.Function units .*. Function space =
      VectorCurve2d.Function.Function (space @ (units :*: Unitless))
  scalarFunction .*. Function vectorFunction = scalarFunction .*. vectorFunction

instance
  Multiplication
    (Function space)
    (Curve1d.Function.Function units)
    (VectorCurve2d.Function.Function (space @ units))

instance Multiplication' (Function space) (Curve1d.Function.Function units) where
  type
    Function space .*. Curve1d.Function.Function units =
      VectorCurve2d.Function.Function (space @ (Unitless :*: units))
  Function vectorFunction .*. scalarFunction = vectorFunction .*. scalarFunction

instance
  space1 ~ space2 =>
  DotMultiplication (Function space1) (Function space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (Function space2)
  where
  type Function space1 .<>. Function space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  Function function1 .<>. Function function2 = function1 .<>. function2

instance
  space1 ~ space2 =>
  DotMultiplication
    (Function space1)
    (VectorCurve2d.Function.Function (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (VectorCurve2d.Function.Function (space2 @ units))
  where
  type
    Function space1 .<>. VectorCurve2d.Function.Function (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  Function function1 .<>. function2 = function1 .<>. function2

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorCurve2d.Function.Function (space1 @ units))
    (Function space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve2d.Function.Function (space1 @ units)) (Function space2)
  where
  type
    VectorCurve2d.Function.Function (space1 @ units) .<>. Function space2 =
      Curve1d.Function.Function (units :*: Unitless)
  function1 .<>. Function function2 = function1 .<>. function2

instance
  space1 ~ space2 =>
  DotMultiplication (Function space1) (Direction2d space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (Direction2d space2)
  where
  type Function space1 .<>. Direction2d space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  Function function .<>. direction = function .<>. direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2d space1) (Function space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction2d space1) (Function space2)
  where
  type Direction2d space1 .<>. Function space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  direction .<>. Function function = direction .<>. function

instance
  space1 ~ space2 =>
  DotMultiplication (Function space1) (Vector2d (space2 @ units)) (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (Vector2d (space2 @ units))
  where
  type
    Function space1 .<>. Vector2d (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  Function function .<>. vector = function .<>. vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2d (space1 @ units)) (Function space2) (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector2d (space1 @ units)) (Function space2)
  where
  type
    Vector2d (space1 @ units) .<>. Function space2 =
      Curve1d.Function.Function (units :*: Unitless)
  vector .<>. Function function = vector .<>. function

instance
  space1 ~ space2 =>
  CrossMultiplication (Function space1) (Function space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (Function space2)
  where
  type Function space1 .><. Function space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  Function function1 .><. Function function2 = function1 .><. function2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function space1)
    (VectorCurve2d.Function.Function (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (VectorCurve2d.Function.Function (space2 @ units))
  where
  type
    Function space1 .><. VectorCurve2d.Function.Function (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  Function function1 .><. function2 = function1 .><. function2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve2d.Function.Function (space1 @ units))
    (Function space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve2d.Function.Function (space1 @ units)) (Function space2)
  where
  type
    VectorCurve2d.Function.Function (space1 @ units) .><. Function space2 =
      Curve1d.Function.Function (units :*: Unitless)
  function1 .><. Function function2 = function1 .><. function2

instance
  space1 ~ space2 =>
  CrossMultiplication (Function space1) (Direction2d space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (Direction2d space2)
  where
  type Function space1 .><. Direction2d space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  Function function .><. direction = function .><. direction

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2d space1) (Function space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction2d space1) (Function space2)
  where
  type Direction2d space1 .><. Function space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  direction .><. Function function = direction .><. function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function space1)
    (Vector2d (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (Vector2d (space2 @ units))
  where
  type
    Function space1 .><. Vector2d (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  Function function .><. vector = function .><. vector

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector2d (space1 @ units))
    (Function space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector2d (space1 @ units)) (Function space2)
  where
  type
    Vector2d (space1 @ units) .><. Function space2 =
      Curve1d.Function.Function (units :*: Unitless)
  vector .><. Function function = vector .><. function

instance Composition (Curve1d.Function.Function Unitless) (Function space) (Function space) where
  Function vectorFunction . scalarFunction = Function (vectorFunction . scalarFunction)

xComponent :: Function space -> Curve1d.Function.Function Unitless
xComponent function = function <> Direction2d.x

yComponent :: Function space -> Curve1d.Function.Function Unitless
yComponent function = function <> Direction2d.y

placeIn :: Frame2d (global @ units) (Defines local) -> Function local -> Function global
placeIn frame = placeInBasis (Frame2d.basis frame)

relativeTo :: Frame2d (global @ units) (Defines local) -> Function global -> Function local
relativeTo frame = relativeToBasis (Frame2d.basis frame)

placeInBasis :: Basis2d global (Defines local) -> Function local -> Function global
placeInBasis basis (Function function) =
  Function (VectorCurve2d.Function.placeInBasis basis function)

relativeToBasis :: Basis2d global (Defines local) -> Function global -> Function local
relativeToBasis basis = placeInBasis (Basis2d.inverse basis)
