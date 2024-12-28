module OpenSolid.DirectionCurve3d.Function
  ( Function
  , unsafe
  , unwrap
  , evaluate
  , evaluateBounds
  , derivative
  , constant
  , reverse
  , xComponent
  , yComponent
  , zComponent
  , placeIn
  , placeInBasis
  , relativeTo
  , relativeToBasis
  )
where

import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Curve1d.Function qualified as Curve1d.Function
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.DirectionBounds3d (DirectionBounds3d)
import OpenSolid.DirectionBounds3d qualified as DirectionBounds3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Prelude
import OpenSolid.Range (Range)
import OpenSolid.Units qualified as Units
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorCurve3d.Function qualified as VectorCurve3d.Function

newtype Function space = Function (VectorCurve3d.Function.Function (space @ Unitless))
  deriving (Show)

instance HasUnits (Function space) where
  type UnitsOf (Function space) = Unitless

instance
  space1 ~ space2 =>
  Units.Coercion (Function space1) (Function space2)
  where
  coerce = identity

unsafe :: VectorCurve3d.Function.Function (space @ Unitless) -> Function space
unsafe = Function

unwrap :: Function space -> VectorCurve3d.Function.Function (space @ Unitless)
unwrap (Function function) = function

evaluate :: Function space -> Float -> Direction3d space
evaluate (Function function) tValue =
  Direction3d.unsafe (VectorCurve3d.Function.evaluate function tValue)

evaluateBounds :: Function space -> Range Unitless -> DirectionBounds3d space
evaluateBounds (Function function) tRange =
  DirectionBounds3d.unsafe (VectorCurve3d.Function.evaluateBounds function tRange)

derivative :: Function space -> VectorCurve3d.Function.Function (space @ Unitless)
derivative (Function function) = VectorCurve3d.Function.derivative function

constant :: Direction3d space -> Function space
constant direction = Function (VectorCurve3d.Function.constant (Vector3d.unit direction))

reverse :: Function space -> Function space
reverse (Function function) = Function (VectorCurve3d.Function.reverse function)

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
    (VectorCurve3d.Function.Function (space @ units))

instance Multiplication' (Qty units) (Function space) where
  type
    Qty units .*. Function space =
      VectorCurve3d.Function.Function (space @ (units :*: Unitless))
  value .*. Function function = value .*. function

instance
  Multiplication
    (Function space)
    (Qty units)
    (VectorCurve3d.Function.Function (space @ units))

instance Multiplication' (Function space) (Qty units) where
  type
    Function space .*. Qty units =
      VectorCurve3d.Function.Function (space @ (Unitless :*: units))
  Function function .*. value = function .*. value

instance
  Multiplication
    (Curve1d.Function.Function units)
    (Function space)
    (VectorCurve3d.Function.Function (space @ units))

instance Multiplication' (Curve1d.Function.Function units) (Function space) where
  type
    Curve1d.Function.Function units .*. Function space =
      VectorCurve3d.Function.Function (space @ (units :*: Unitless))
  scalarFunction .*. Function vectorFunction = scalarFunction .*. vectorFunction

instance
  Multiplication
    (Function space)
    (Curve1d.Function.Function units)
    (VectorCurve3d.Function.Function (space @ units))

instance Multiplication' (Function space) (Curve1d.Function.Function units) where
  type
    Function space .*. Curve1d.Function.Function units =
      VectorCurve3d.Function.Function (space @ (Unitless :*: units))
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
    (VectorCurve3d.Function.Function (space2 @ units))
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (VectorCurve3d.Function.Function (space2 @ units))
  where
  type
    Function space1 .<>. VectorCurve3d.Function.Function (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  Function function1 .<>. function2 = function1 .<>. function2

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorCurve3d.Function.Function (space1 @ units))
    (Function space2)
    (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (VectorCurve3d.Function.Function (space1 @ units)) (Function space2)
  where
  type
    VectorCurve3d.Function.Function (space1 @ units) .<>. Function space2 =
      Curve1d.Function.Function (units :*: Unitless)
  function1 .<>. Function function2 = function1 .<>. function2

instance
  space1 ~ space2 =>
  DotMultiplication (Function space1) (Direction3d space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (Direction3d space2)
  where
  type Function space1 .<>. Direction3d space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  Function function .<>. direction = function .<>. direction

instance
  space1 ~ space2 =>
  DotMultiplication (Direction3d space1) (Function space2) (Curve1d.Function.Function Unitless)

instance
  space1 ~ space2 =>
  DotMultiplication' (Direction3d space1) (Function space2)
  where
  type Direction3d space1 .<>. Function space2 = Curve1d.Function.Function (Unitless :*: Unitless)
  direction .<>. Function function = direction .<>. function

instance
  space1 ~ space2 =>
  DotMultiplication (Function space1) (Vector3d (space2 @ units)) (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Function space1) (Vector3d (space2 @ units))
  where
  type
    Function space1 .<>. Vector3d (space2 @ units) =
      Curve1d.Function.Function (Unitless :*: units)
  Function function .<>. vector = function .<>. vector

instance
  space1 ~ space2 =>
  DotMultiplication (Vector3d (space1 @ units)) (Function space2) (Curve1d.Function.Function units)

instance
  space1 ~ space2 =>
  DotMultiplication' (Vector3d (space1 @ units)) (Function space2)
  where
  type
    Vector3d (space1 @ units) .<>. Function space2 =
      Curve1d.Function.Function (units :*: Unitless)
  vector .<>. Function function = vector .<>. function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function space1)
    (Function space2)
    (VectorCurve3d.Function.Function (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (Function space2)
  where
  type
    Function space1 .><. Function space2 =
      VectorCurve3d.Function.Function (space1 @ (Unitless :*: Unitless))
  Function function1 .><. Function function2 = function1 .><. function2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function space1)
    (VectorCurve3d.Function.Function (space2 @ units))
    (VectorCurve3d.Function.Function (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (VectorCurve3d.Function.Function (space2 @ units))
  where
  type
    Function space1 .><. VectorCurve3d.Function.Function (space2 @ units) =
      VectorCurve3d.Function.Function (space1 @ (Unitless :*: units))
  Function function1 .><. function2 = function1 .><. function2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorCurve3d.Function.Function (space1 @ units))
    (Function space2)
    (VectorCurve3d.Function.Function (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (VectorCurve3d.Function.Function (space1 @ units)) (Function space2)
  where
  type
    VectorCurve3d.Function.Function (space1 @ units) .><. Function space2 =
      VectorCurve3d.Function.Function (space1 @ (units :*: Unitless))
  function1 .><. Function function2 = function1 .><. function2

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function space1)
    (Direction3d space2)
    (VectorCurve3d.Function.Function (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (Direction3d space2)
  where
  type
    Function space1 .><. Direction3d space2 =
      VectorCurve3d.Function.Function (space1 @ (Unitless :*: Unitless))
  Function function .><. direction = function .><. direction

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (Function space2)
    (VectorCurve3d.Function.Function (space1 @ Unitless))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Direction3d space1) (Function space2)
  where
  type
    Direction3d space1 .><. Function space2 =
      VectorCurve3d.Function.Function (space1 @ (Unitless :*: Unitless))
  direction .><. Function function = direction .><. function

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Function space1)
    (Vector3d (space2 @ units))
    (VectorCurve3d.Function.Function (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Function space1) (Vector3d (space2 @ units))
  where
  type
    Function space1 .><. Vector3d (space2 @ units) =
      VectorCurve3d.Function.Function (space1 @ (Unitless :*: units))
  Function function .><. vector = function .><. vector

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (Function space2)
    (VectorCurve3d.Function.Function (space1 @ units))

instance
  space1 ~ space2 =>
  CrossMultiplication' (Vector3d (space1 @ units)) (Function space2)
  where
  type
    Vector3d (space1 @ units) .><. Function space2 =
      VectorCurve3d.Function.Function (space1 @ (units :*: Unitless))
  vector .><. Function function = vector .><. function

instance Composition (Curve1d.Function.Function Unitless) (Function space) (Function space) where
  Function vectorFunction . scalarFunction = Function (vectorFunction . scalarFunction)

xComponent :: Function space -> Curve1d.Function.Function Unitless
xComponent function = function <> Direction3d.x

yComponent :: Function space -> Curve1d.Function.Function Unitless
yComponent function = function <> Direction3d.y

zComponent :: Function space -> Curve1d.Function.Function Unitless
zComponent function = function <> Direction3d.z

placeIn :: Frame3d (global @ units) (Defines local) -> Function local -> Function global
placeIn frame = placeInBasis (Frame3d.basis frame)

relativeTo :: Frame3d (global @ units) (Defines local) -> Function global -> Function local
relativeTo frame = relativeToBasis (Frame3d.basis frame)

placeInBasis :: Basis3d global (Defines local) -> Function local -> Function global
placeInBasis basis (Function function) =
  Function (VectorCurve3d.Function.placeInBasis basis function)

relativeToBasis :: Basis3d global (Defines local) -> Function global -> Function local
relativeToBasis basis = placeInBasis (Basis3d.inverse basis)
