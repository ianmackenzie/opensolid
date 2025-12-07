module OpenSolid.DirectionSurfaceFunction2d
  ( DirectionSurfaceFunction2d
  , unsafe
  , unwrap
  , evaluate
  , evaluateBounds
  , derivative
  , constant
  , placeIn
  , relativeTo
  )
where

import GHC.Records (HasField (getField))
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.DirectionBounds2d (DirectionBounds2d)
import OpenSolid.DirectionBounds2d qualified as DirectionBounds2d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Polymorphic.Vector2d (Vector2d)
import OpenSolid.Polymorphic.Vector2d qualified as Vector2d
import OpenSolid.VectorSurfaceFunction2d (VectorSurfaceFunction2d)
import OpenSolid.VectorSurfaceFunction2d qualified as VectorSurfaceFunction2d

newtype DirectionSurfaceFunction2d space
  = DirectionSurfaceFunction2d (VectorSurfaceFunction2d Unitless space)

unsafe :: VectorSurfaceFunction2d Unitless space -> DirectionSurfaceFunction2d space
unsafe = DirectionSurfaceFunction2d

unwrap :: DirectionSurfaceFunction2d space -> VectorSurfaceFunction2d Unitless space
unwrap (DirectionSurfaceFunction2d vectorSurfaceFunction) = vectorSurfaceFunction

evaluate :: DirectionSurfaceFunction2d space -> UvPoint -> Direction2d space
evaluate (DirectionSurfaceFunction2d vectorSurfaceFunction) uvPoint =
  Direction2d.unsafe (VectorSurfaceFunction2d.evaluate vectorSurfaceFunction uvPoint)

evaluateBounds :: DirectionSurfaceFunction2d space -> UvBounds -> DirectionBounds2d space
evaluateBounds (DirectionSurfaceFunction2d vectorSurfaceFunction) uvBounds =
  DirectionBounds2d.unsafe (VectorSurfaceFunction2d.evaluateBounds vectorSurfaceFunction uvBounds)

derivative ::
  SurfaceParameter ->
  DirectionSurfaceFunction2d space ->
  VectorSurfaceFunction2d Unitless space
derivative parameter (DirectionSurfaceFunction2d vectorSurfaceFunction) =
  VectorSurfaceFunction2d.derivative parameter vectorSurfaceFunction

constant :: Direction2d space -> DirectionSurfaceFunction2d space
constant direction =
  DirectionSurfaceFunction2d (VectorSurfaceFunction2d.constant (Vector2d.unit direction))

instance Negation (DirectionSurfaceFunction2d space) where
  negative (DirectionSurfaceFunction2d vectorSurfaceFunction) =
    DirectionSurfaceFunction2d (negative vectorSurfaceFunction)

instance
  Multiplication
    Sign
    (DirectionSurfaceFunction2d space)
    (DirectionSurfaceFunction2d space)
  where
  Positive .*. function = function
  Negative .*. function = negative function

instance
  Multiplication_
    Sign
    (DirectionSurfaceFunction2d space)
    (DirectionSurfaceFunction2d space)
  where
  Positive ?*? function = function
  Negative ?*? function = negative function

instance
  Multiplication
    (DirectionSurfaceFunction2d space)
    Sign
    (DirectionSurfaceFunction2d space)
  where
  function .*. Positive = function
  function .*. Negative = negative function

instance
  Multiplication_
    (DirectionSurfaceFunction2d space)
    Sign
    (DirectionSurfaceFunction2d space)
  where
  function ?*? Positive = function
  function ?*? Negative = negative function

instance
  Multiplication
    (Quantity units)
    (DirectionSurfaceFunction2d space)
    (VectorSurfaceFunction2d units space)
  where
  value .*. DirectionSurfaceFunction2d vectorSurfaceFunction = value .*. vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction2d space)
    (Quantity units)
    (VectorSurfaceFunction2d units space)
  where
  DirectionSurfaceFunction2d vectorSurfaceFunction .*. value = vectorSurfaceFunction .*. value

instance
  Multiplication
    (SurfaceFunction units)
    (DirectionSurfaceFunction2d space)
    (VectorSurfaceFunction2d units space)
  where
  scalarSurfaceFunction .*. DirectionSurfaceFunction2d vectorSurfaceFunction =
    scalarSurfaceFunction .*. vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction2d space)
    (SurfaceFunction units)
    (VectorSurfaceFunction2d units space)
  where
  DirectionSurfaceFunction2d vectorSurfaceFunction .*. scalarSurfaceFunction =
    vectorSurfaceFunction .*. scalarSurfaceFunction

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2d space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction Unitless)
  where
  DirectionSurfaceFunction2d lhs `dot` DirectionSurfaceFunction2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2d space1)
    (VectorSurfaceFunction2d units space2)
    (SurfaceFunction units)
  where
  DirectionSurfaceFunction2d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction2d units space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction units)
  where
  lhs `dot` DirectionSurfaceFunction2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2d space1)
    (Direction2d space2)
    (SurfaceFunction Unitless)
  where
  DirectionSurfaceFunction2d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2d space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction Unitless)
  where
  lhs `dot` DirectionSurfaceFunction2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2d space1)
    (Vector2d units space2)
    (SurfaceFunction units)
  where
  DirectionSurfaceFunction2d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Vector2d units space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction units)
  where
  lhs `dot` DirectionSurfaceFunction2d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2d space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction Unitless)
  where
  DirectionSurfaceFunction2d lhs `cross` DirectionSurfaceFunction2d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2d space1)
    (VectorSurfaceFunction2d units space2)
    (SurfaceFunction units)
  where
  DirectionSurfaceFunction2d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction2d units space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction units)
  where
  lhs `cross` DirectionSurfaceFunction2d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2d space1)
    (Direction2d space2)
    (SurfaceFunction Unitless)
  where
  DirectionSurfaceFunction2d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2d space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction Unitless)
  where
  lhs `cross` DirectionSurfaceFunction2d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2d space1)
    (Vector2d units space2)
    (SurfaceFunction units)
  where
  DirectionSurfaceFunction2d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector2d units space1)
    (DirectionSurfaceFunction2d space2)
    (SurfaceFunction units)
  where
  lhs `cross` DirectionSurfaceFunction2d rhs = lhs `cross` rhs

instance HasField "xComponent" (DirectionSurfaceFunction2d space) (SurfaceFunction Unitless) where
  getField (DirectionSurfaceFunction2d function) = function.xComponent

instance HasField "yComponent" (DirectionSurfaceFunction2d space) (SurfaceFunction Unitless) where
  getField (DirectionSurfaceFunction2d function) = function.yComponent

placeIn ::
  Frame2d frameUnits global local ->
  DirectionSurfaceFunction2d local ->
  DirectionSurfaceFunction2d global
placeIn frame (DirectionSurfaceFunction2d function) =
  DirectionSurfaceFunction2d (VectorSurfaceFunction2d.placeIn frame function)

relativeTo ::
  Frame2d frameUnits global local ->
  DirectionSurfaceFunction2d global ->
  DirectionSurfaceFunction2d local
relativeTo frame = placeIn (Frame2d.inverse frame)
