module OpenSolid.DirectionSurfaceFunction2D
  ( DirectionSurfaceFunction2D
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
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorSurfaceFunction2D (VectorSurfaceFunction2D)
import OpenSolid.VectorSurfaceFunction2D qualified as VectorSurfaceFunction2D

newtype DirectionSurfaceFunction2D space
  = DirectionSurfaceFunction2D (VectorSurfaceFunction2D Unitless space)

unsafe :: VectorSurfaceFunction2D Unitless space -> DirectionSurfaceFunction2D space
unsafe = DirectionSurfaceFunction2D

unwrap :: DirectionSurfaceFunction2D space -> VectorSurfaceFunction2D Unitless space
unwrap (DirectionSurfaceFunction2D vectorSurfaceFunction) = vectorSurfaceFunction

evaluate :: DirectionSurfaceFunction2D space -> UvPoint -> Direction2D space
evaluate (DirectionSurfaceFunction2D vectorSurfaceFunction) uvPoint =
  Direction2D.unsafe (VectorSurfaceFunction2D.evaluate vectorSurfaceFunction uvPoint)

evaluateBounds :: DirectionSurfaceFunction2D space -> UvBounds -> DirectionBounds2D space
evaluateBounds (DirectionSurfaceFunction2D vectorSurfaceFunction) uvBounds =
  DirectionBounds2D.unsafe (VectorSurfaceFunction2D.evaluateBounds vectorSurfaceFunction uvBounds)

derivative ::
  SurfaceParameter ->
  DirectionSurfaceFunction2D space ->
  VectorSurfaceFunction2D Unitless space
derivative parameter (DirectionSurfaceFunction2D vectorSurfaceFunction) =
  VectorSurfaceFunction2D.derivative parameter vectorSurfaceFunction

constant :: Direction2D space -> DirectionSurfaceFunction2D space
constant direction =
  DirectionSurfaceFunction2D (VectorSurfaceFunction2D.constant (Vector2D.unit direction))

instance Negation (DirectionSurfaceFunction2D space) where
  negative (DirectionSurfaceFunction2D vectorSurfaceFunction) =
    DirectionSurfaceFunction2D (negative vectorSurfaceFunction)

instance
  Multiplication
    Sign
    (DirectionSurfaceFunction2D space)
    (DirectionSurfaceFunction2D space)
  where
  Positive .*. function = function
  Negative .*. function = negative function

instance
  Multiplication_
    Sign
    (DirectionSurfaceFunction2D space)
    (DirectionSurfaceFunction2D space)
  where
  Positive ?*? function = function
  Negative ?*? function = negative function

instance
  Multiplication
    (DirectionSurfaceFunction2D space)
    Sign
    (DirectionSurfaceFunction2D space)
  where
  function .*. Positive = function
  function .*. Negative = negative function

instance
  Multiplication_
    (DirectionSurfaceFunction2D space)
    Sign
    (DirectionSurfaceFunction2D space)
  where
  function ?*? Positive = function
  function ?*? Negative = negative function

instance
  Multiplication
    (Quantity units)
    (DirectionSurfaceFunction2D space)
    (VectorSurfaceFunction2D units space)
  where
  value .*. DirectionSurfaceFunction2D vectorSurfaceFunction = value .*. vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction2D space)
    (Quantity units)
    (VectorSurfaceFunction2D units space)
  where
  DirectionSurfaceFunction2D vectorSurfaceFunction .*. value = vectorSurfaceFunction .*. value

instance
  Multiplication
    (SurfaceFunction1D units)
    (DirectionSurfaceFunction2D space)
    (VectorSurfaceFunction2D units space)
  where
  scalarSurfaceFunction .*. DirectionSurfaceFunction2D vectorSurfaceFunction =
    scalarSurfaceFunction .*. vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction2D space)
    (SurfaceFunction1D units)
    (VectorSurfaceFunction2D units space)
  where
  DirectionSurfaceFunction2D vectorSurfaceFunction .*. scalarSurfaceFunction =
    vectorSurfaceFunction .*. scalarSurfaceFunction

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2D space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2D space1)
    (VectorSurfaceFunction2D units space2)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction2D units space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2D space1)
    (Direction2D space2)
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction2D space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D Unitless)
  where
  lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction2D space1)
    (Vector2D units space2)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Vector2D units space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2D space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2D space1)
    (VectorSurfaceFunction2D units space2)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction2D units space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D units)
  where
  lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2D space1)
    (Direction2D space2)
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction2D space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D Unitless)
  where
  lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction2D space1)
    (Vector2D units space2)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector2D units space1)
    (DirectionSurfaceFunction2D space2)
    (SurfaceFunction1D units)
  where
  lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance HasField "xComponent" (DirectionSurfaceFunction2D space) (SurfaceFunction1D Unitless) where
  getField (DirectionSurfaceFunction2D function) = function.xComponent

instance HasField "yComponent" (DirectionSurfaceFunction2D space) (SurfaceFunction1D Unitless) where
  getField (DirectionSurfaceFunction2D function) = function.yComponent

placeIn ::
  Frame2D frameUnits global local ->
  DirectionSurfaceFunction2D local ->
  DirectionSurfaceFunction2D global
placeIn frame (DirectionSurfaceFunction2D function) =
  DirectionSurfaceFunction2D (VectorSurfaceFunction2D.placeIn frame function)

relativeTo ::
  Frame2D frameUnits global local ->
  DirectionSurfaceFunction2D global ->
  DirectionSurfaceFunction2D local
relativeTo frame = placeIn (Frame2D.inverse frame)
