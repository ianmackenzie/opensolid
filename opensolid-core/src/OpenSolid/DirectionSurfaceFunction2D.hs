module OpenSolid.DirectionSurfaceFunction2D
  ( DirectionSurfaceFunction2D
  , unsafe
  , unwrap
  , value
  , bounds
  , derivative
  , constant
  , placeIn
  , relativeTo
  )
where

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

newtype DirectionSurfaceFunction2D
  = DirectionSurfaceFunction2D (VectorSurfaceFunction2D Unitless)

unsafe :: VectorSurfaceFunction2D Unitless -> DirectionSurfaceFunction2D
unsafe = DirectionSurfaceFunction2D

unwrap :: DirectionSurfaceFunction2D -> VectorSurfaceFunction2D Unitless
unwrap (DirectionSurfaceFunction2D vectorSurfaceFunction) = vectorSurfaceFunction

value :: DirectionSurfaceFunction2D -> UvPoint -> Direction2D
value (DirectionSurfaceFunction2D vectorSurfaceFunction) uvPoint =
  Direction2D.unsafe (VectorSurfaceFunction2D.value vectorSurfaceFunction uvPoint)

bounds :: DirectionSurfaceFunction2D -> UvBounds -> DirectionBounds2D
bounds (DirectionSurfaceFunction2D vectorSurfaceFunction) uvBounds =
  DirectionBounds2D.unsafe (VectorSurfaceFunction2D.bounds vectorSurfaceFunction uvBounds)

derivative :: SurfaceParameter -> DirectionSurfaceFunction2D -> VectorSurfaceFunction2D Unitless
derivative parameter (DirectionSurfaceFunction2D vectorSurfaceFunction) =
  VectorSurfaceFunction2D.derivative parameter vectorSurfaceFunction

constant :: Direction2D -> DirectionSurfaceFunction2D
constant direction =
  DirectionSurfaceFunction2D (VectorSurfaceFunction2D.constant (Vector2D.unit direction))

instance Negation DirectionSurfaceFunction2D where
  negate (DirectionSurfaceFunction2D vectorSurfaceFunction) =
    DirectionSurfaceFunction2D (negate vectorSurfaceFunction)

instance
  Multiplication
    Sign
    DirectionSurfaceFunction2D
    DirectionSurfaceFunction2D
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication_
    Sign
    DirectionSurfaceFunction2D
    DirectionSurfaceFunction2D
  where
  Positive ?*? function = function
  Negative ?*? function = -function

instance
  Multiplication
    DirectionSurfaceFunction2D
    Sign
    DirectionSurfaceFunction2D
  where
  function * Positive = function
  function * Negative = -function

instance
  Multiplication_
    DirectionSurfaceFunction2D
    Sign
    DirectionSurfaceFunction2D
  where
  function ?*? Positive = function
  function ?*? Negative = -function

instance
  Multiplication
    (Quantity units)
    DirectionSurfaceFunction2D
    (VectorSurfaceFunction2D units)
  where
  quantity * DirectionSurfaceFunction2D vectorSurfaceFunction = quantity * vectorSurfaceFunction

instance
  Multiplication
    DirectionSurfaceFunction2D
    (Quantity units)
    (VectorSurfaceFunction2D units)
  where
  DirectionSurfaceFunction2D vectorSurfaceFunction * quantity = vectorSurfaceFunction * quantity

instance
  Multiplication
    (SurfaceFunction1D units)
    DirectionSurfaceFunction2D
    (VectorSurfaceFunction2D units)
  where
  scalarSurfaceFunction * DirectionSurfaceFunction2D vectorSurfaceFunction =
    scalarSurfaceFunction * vectorSurfaceFunction

instance
  Multiplication
    DirectionSurfaceFunction2D
    (SurfaceFunction1D units)
    (VectorSurfaceFunction2D units)
  where
  DirectionSurfaceFunction2D vectorSurfaceFunction * scalarSurfaceFunction =
    vectorSurfaceFunction * scalarSurfaceFunction

instance
  DotMultiplication
    DirectionSurfaceFunction2D
    DirectionSurfaceFunction2D
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  DotMultiplication
    DirectionSurfaceFunction2D
    (VectorSurfaceFunction2D units)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `dot` rhs = lhs `dot` rhs

instance
  DotMultiplication
    (VectorSurfaceFunction2D units)
    DirectionSurfaceFunction2D
    (SurfaceFunction1D units)
  where
  lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  DotMultiplication
    DirectionSurfaceFunction2D
    Direction2D
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `dot` rhs = lhs `dot` rhs

instance
  DotMultiplication
    Direction2D
    DirectionSurfaceFunction2D
    (SurfaceFunction1D Unitless)
  where
  lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  DotMultiplication
    DirectionSurfaceFunction2D
    (Vector2D units)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `dot` rhs = lhs `dot` rhs

instance
  DotMultiplication
    (Vector2D units)
    DirectionSurfaceFunction2D
    (SurfaceFunction1D units)
  where
  lhs `dot` DirectionSurfaceFunction2D rhs = lhs `dot` rhs

instance
  CrossMultiplication
    DirectionSurfaceFunction2D
    DirectionSurfaceFunction2D
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance
  CrossMultiplication
    DirectionSurfaceFunction2D
    (VectorSurfaceFunction2D units)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `cross` rhs = lhs `cross` rhs

instance
  CrossMultiplication
    (VectorSurfaceFunction2D units)
    DirectionSurfaceFunction2D
    (SurfaceFunction1D units)
  where
  lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance
  CrossMultiplication
    DirectionSurfaceFunction2D
    Direction2D
    (SurfaceFunction1D Unitless)
  where
  DirectionSurfaceFunction2D lhs `cross` rhs = lhs `cross` rhs

instance
  CrossMultiplication
    Direction2D
    DirectionSurfaceFunction2D
    (SurfaceFunction1D Unitless)
  where
  lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

instance
  CrossMultiplication
    DirectionSurfaceFunction2D
    (Vector2D units)
    (SurfaceFunction1D units)
  where
  DirectionSurfaceFunction2D lhs `cross` rhs = lhs `cross` rhs

instance
  CrossMultiplication
    (Vector2D units)
    DirectionSurfaceFunction2D
    (SurfaceFunction1D units)
  where
  lhs `cross` DirectionSurfaceFunction2D rhs = lhs `cross` rhs

placeIn :: Frame2D frameUnits -> DirectionSurfaceFunction2D -> DirectionSurfaceFunction2D
placeIn frame (DirectionSurfaceFunction2D function) =
  DirectionSurfaceFunction2D (VectorSurfaceFunction2D.placeIn frame function)

relativeTo :: Frame2D frameUnits -> DirectionSurfaceFunction2D -> DirectionSurfaceFunction2D
relativeTo frame = placeIn (Frame2D.inverse frame)
