module OpenSolid.DirectionSurfaceFunction3D
  ( DirectionSurfaceFunction3D
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

import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.DirectionBounds3D (DirectionBounds3D)
import OpenSolid.DirectionBounds3D qualified as DirectionBounds3D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorSurfaceFunction3D (VectorSurfaceFunction3D)
import OpenSolid.VectorSurfaceFunction3D qualified as VectorSurfaceFunction3D

newtype DirectionSurfaceFunction3D space
  = DirectionSurfaceFunction3D (VectorSurfaceFunction3D Unitless space)

unsafe :: VectorSurfaceFunction3D Unitless space -> DirectionSurfaceFunction3D space
unsafe = DirectionSurfaceFunction3D

unwrap :: DirectionSurfaceFunction3D space -> VectorSurfaceFunction3D Unitless space
unwrap (DirectionSurfaceFunction3D vectorSurfaceFunction) = vectorSurfaceFunction

evaluate :: DirectionSurfaceFunction3D space -> UvPoint -> Direction3D space
evaluate (DirectionSurfaceFunction3D vectorSurfaceFunction) uvPoint =
  Direction3D.unsafe (VectorSurfaceFunction3D.evaluate vectorSurfaceFunction uvPoint)

evaluateBounds :: DirectionSurfaceFunction3D space -> UvBounds -> DirectionBounds3D space
evaluateBounds (DirectionSurfaceFunction3D vectorSurfaceFunction) uvBounds =
  DirectionBounds3D.unsafe (VectorSurfaceFunction3D.evaluateBounds vectorSurfaceFunction uvBounds)

derivative ::
  SurfaceParameter ->
  DirectionSurfaceFunction3D space ->
  VectorSurfaceFunction3D Unitless space
derivative parameter (DirectionSurfaceFunction3D vectorSurfaceFunction) =
  VectorSurfaceFunction3D.derivative parameter vectorSurfaceFunction

constant :: Direction3D space -> DirectionSurfaceFunction3D space
constant direction =
  DirectionSurfaceFunction3D (VectorSurfaceFunction3D.constant (Vector3D.unit direction))

instance Negation (DirectionSurfaceFunction3D space) where
  negate (DirectionSurfaceFunction3D vectorSurfaceFunction) =
    DirectionSurfaceFunction3D (negate vectorSurfaceFunction)

instance
  Multiplication
    Sign
    (DirectionSurfaceFunction3D space)
    (DirectionSurfaceFunction3D space)
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication_
    Sign
    (DirectionSurfaceFunction3D space)
    (DirectionSurfaceFunction3D space)
  where
  Positive ?*? function = function
  Negative ?*? function = -function

instance
  Multiplication
    (DirectionSurfaceFunction3D space)
    Sign
    (DirectionSurfaceFunction3D space)
  where
  function * Positive = function
  function * Negative = -function

instance
  Multiplication_
    (DirectionSurfaceFunction3D space)
    Sign
    (DirectionSurfaceFunction3D space)
  where
  function ?*? Positive = function
  function ?*? Negative = -function

instance
  Multiplication
    (Quantity units)
    (DirectionSurfaceFunction3D space)
    (VectorSurfaceFunction3D units space)
  where
  value * DirectionSurfaceFunction3D vectorSurfaceFunction = value * vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction3D space)
    (Quantity units)
    (VectorSurfaceFunction3D units space)
  where
  DirectionSurfaceFunction3D vectorSurfaceFunction * value = vectorSurfaceFunction * value

instance
  Multiplication
    (SurfaceFunction1D units)
    (DirectionSurfaceFunction3D space)
    (VectorSurfaceFunction3D units space)
  where
  scalarSurfaceFunction * DirectionSurfaceFunction3D vectorSurfaceFunction =
    scalarSurfaceFunction * vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction3D space)
    (SurfaceFunction1D units)
    (VectorSurfaceFunction3D units space)
  where
  DirectionSurfaceFunction3D vectorSurfaceFunction * scalarSurfaceFunction =
    vectorSurfaceFunction * scalarSurfaceFunction

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3D space1)
    (DirectionSurfaceFunction3D space2)
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction3D lhs `dot` DirectionSurfaceFunction3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3D space1)
    (VectorSurfaceFunction3D units space2)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction3D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction3D units space1)
    (DirectionSurfaceFunction3D space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` DirectionSurfaceFunction3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3D space1)
    (Direction3D space2)
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction3D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction3D space1)
    (DirectionSurfaceFunction3D space2)
    (SurfaceFunction1D Unitless)
  where
  lhs `dot` DirectionSurfaceFunction3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3D space1)
    (Vector3D units space2)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction3D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Vector3D units space1)
    (DirectionSurfaceFunction3D space2)
    (SurfaceFunction1D units)
  where
  lhs `dot` DirectionSurfaceFunction3D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3D space1)
    (DirectionSurfaceFunction3D space2)
    (VectorSurfaceFunction3D Unitless space1)
  where
  DirectionSurfaceFunction3D lhs `cross` DirectionSurfaceFunction3D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3D space1)
    (VectorSurfaceFunction3D units space2)
    (VectorSurfaceFunction3D units space1)
  where
  DirectionSurfaceFunction3D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction3D units space1)
    (DirectionSurfaceFunction3D space2)
    (VectorSurfaceFunction3D units space1)
  where
  lhs `cross` DirectionSurfaceFunction3D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3D space1)
    (Direction3D space2)
    (VectorSurfaceFunction3D Unitless space1)
  where
  DirectionSurfaceFunction3D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3D space1)
    (DirectionSurfaceFunction3D space2)
    (VectorSurfaceFunction3D Unitless space1)
  where
  lhs `cross` DirectionSurfaceFunction3D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3D space1)
    (Vector3D units space2)
    (VectorSurfaceFunction3D units space1)
  where
  DirectionSurfaceFunction3D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3D units space1)
    (DirectionSurfaceFunction3D space2)
    (VectorSurfaceFunction3D units space1)
  where
  lhs `cross` DirectionSurfaceFunction3D rhs = lhs `cross` rhs

placeIn ::
  Frame3D global local ->
  DirectionSurfaceFunction3D local ->
  DirectionSurfaceFunction3D global
placeIn frame (DirectionSurfaceFunction3D function) =
  DirectionSurfaceFunction3D (VectorSurfaceFunction3D.placeIn frame function)

relativeTo ::
  Frame3D global local ->
  DirectionSurfaceFunction3D global ->
  DirectionSurfaceFunction3D local
relativeTo frame = placeIn (Frame3D.inverse frame)
