module OpenSolid.DirectionSurfaceFunction3d
  ( DirectionSurfaceFunction3d
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

import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.DirectionBounds3d (DirectionBounds3d)
import OpenSolid.DirectionBounds3d qualified as DirectionBounds3d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Prelude
import OpenSolid.SurfaceFunction (SurfaceFunction)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.VectorSurfaceFunction3d (VectorSurfaceFunction3d)
import OpenSolid.VectorSurfaceFunction3d qualified as VectorSurfaceFunction3d

newtype DirectionSurfaceFunction3d space
  = DirectionSurfaceFunction3d (VectorSurfaceFunction3d (space @ Unitless))

unsafe :: VectorSurfaceFunction3d (space @ Unitless) -> DirectionSurfaceFunction3d space
unsafe = DirectionSurfaceFunction3d

unwrap :: DirectionSurfaceFunction3d space -> VectorSurfaceFunction3d (space @ Unitless)
unwrap (DirectionSurfaceFunction3d vectorSurfaceFunction) = vectorSurfaceFunction

evaluate :: DirectionSurfaceFunction3d space -> UvPoint -> Direction3d space
evaluate (DirectionSurfaceFunction3d vectorSurfaceFunction) uvPoint =
  Direction3d.unsafe (VectorSurfaceFunction3d.evaluate vectorSurfaceFunction uvPoint)

evaluateBounds :: DirectionSurfaceFunction3d space -> UvBounds -> DirectionBounds3d space
evaluateBounds (DirectionSurfaceFunction3d vectorSurfaceFunction) uvBounds =
  DirectionBounds3d.unsafe (VectorSurfaceFunction3d.evaluateBounds vectorSurfaceFunction uvBounds)

derivative ::
  SurfaceParameter ->
  DirectionSurfaceFunction3d space ->
  VectorSurfaceFunction3d (space @ Unitless)
derivative parameter (DirectionSurfaceFunction3d vectorSurfaceFunction) =
  VectorSurfaceFunction3d.derivative parameter vectorSurfaceFunction

constant :: Direction3d space -> DirectionSurfaceFunction3d space
constant direction =
  DirectionSurfaceFunction3d (VectorSurfaceFunction3d.constant (Vector3d.unit direction))

instance Negation (DirectionSurfaceFunction3d space) where
  negate (DirectionSurfaceFunction3d vectorSurfaceFunction) =
    DirectionSurfaceFunction3d (negate vectorSurfaceFunction)

instance
  Multiplication
    Sign
    (DirectionSurfaceFunction3d space)
    (DirectionSurfaceFunction3d space)
  where
  Positive * function = function
  Negative * function = -function

instance
  Multiplication'
    Sign
    (DirectionSurfaceFunction3d space)
    (DirectionSurfaceFunction3d space)
  where
  Positive .*. function = function
  Negative .*. function = -function

instance
  Multiplication
    (DirectionSurfaceFunction3d space)
    Sign
    (DirectionSurfaceFunction3d space)
  where
  function * Positive = function
  function * Negative = -function

instance
  Multiplication'
    (DirectionSurfaceFunction3d space)
    Sign
    (DirectionSurfaceFunction3d space)
  where
  function .*. Positive = function
  function .*. Negative = -function

instance
  Multiplication
    (Quantity units)
    (DirectionSurfaceFunction3d space)
    (VectorSurfaceFunction3d (space @ units))
  where
  value * DirectionSurfaceFunction3d vectorSurfaceFunction = value * vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction3d space)
    (Quantity units)
    (VectorSurfaceFunction3d (space @ units))
  where
  DirectionSurfaceFunction3d vectorSurfaceFunction * value = vectorSurfaceFunction * value

instance
  Multiplication
    (SurfaceFunction units)
    (DirectionSurfaceFunction3d space)
    (VectorSurfaceFunction3d (space @ units))
  where
  scalarSurfaceFunction * DirectionSurfaceFunction3d vectorSurfaceFunction =
    scalarSurfaceFunction * vectorSurfaceFunction

instance
  Multiplication
    (DirectionSurfaceFunction3d space)
    (SurfaceFunction units)
    (VectorSurfaceFunction3d (space @ units))
  where
  DirectionSurfaceFunction3d vectorSurfaceFunction * scalarSurfaceFunction =
    vectorSurfaceFunction * scalarSurfaceFunction

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3d space1)
    (DirectionSurfaceFunction3d space2)
    (SurfaceFunction Unitless)
  where
  DirectionSurfaceFunction3d lhs `dot` DirectionSurfaceFunction3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3d space1)
    (VectorSurfaceFunction3d (space2 @ units))
    (SurfaceFunction units)
  where
  DirectionSurfaceFunction3d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (VectorSurfaceFunction3d (space1 @ units))
    (DirectionSurfaceFunction3d space2)
    (SurfaceFunction units)
  where
  lhs `dot` DirectionSurfaceFunction3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3d space1)
    (Direction3d space2)
    (SurfaceFunction Unitless)
  where
  DirectionSurfaceFunction3d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Direction3d space1)
    (DirectionSurfaceFunction3d space2)
    (SurfaceFunction Unitless)
  where
  lhs `dot` DirectionSurfaceFunction3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (DirectionSurfaceFunction3d space1)
    (Vector3d (space2 @ units))
    (SurfaceFunction units)
  where
  DirectionSurfaceFunction3d lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication
    (Vector3d (space1 @ units))
    (DirectionSurfaceFunction3d space2)
    (SurfaceFunction units)
  where
  lhs `dot` DirectionSurfaceFunction3d rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3d space1)
    (DirectionSurfaceFunction3d space2)
    (VectorSurfaceFunction3d (space1 @ Unitless))
  where
  DirectionSurfaceFunction3d lhs `cross` DirectionSurfaceFunction3d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3d space1)
    (VectorSurfaceFunction3d (space2 @ units))
    (VectorSurfaceFunction3d (space1 @ units))
  where
  DirectionSurfaceFunction3d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (VectorSurfaceFunction3d (space1 @ units))
    (DirectionSurfaceFunction3d space2)
    (VectorSurfaceFunction3d (space1 @ units))
  where
  lhs `cross` DirectionSurfaceFunction3d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3d space1)
    (Direction3d space2)
    (VectorSurfaceFunction3d (space1 @ Unitless))
  where
  DirectionSurfaceFunction3d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Direction3d space1)
    (DirectionSurfaceFunction3d space2)
    (VectorSurfaceFunction3d (space1 @ Unitless))
  where
  lhs `cross` DirectionSurfaceFunction3d rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (DirectionSurfaceFunction3d space1)
    (Vector3d (space2 @ units))
    (VectorSurfaceFunction3d (space1 @ units))
  where
  DirectionSurfaceFunction3d lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication
    (Vector3d (space1 @ units))
    (DirectionSurfaceFunction3d space2)
    (VectorSurfaceFunction3d (space1 @ units))
  where
  lhs `cross` DirectionSurfaceFunction3d rhs = lhs `cross` rhs

placeIn ::
  Frame3d (global @ frameUnits) (Defines local) ->
  DirectionSurfaceFunction3d local ->
  DirectionSurfaceFunction3d global
placeIn frame (DirectionSurfaceFunction3d function) =
  DirectionSurfaceFunction3d (VectorSurfaceFunction3d.placeIn frame function)

relativeTo ::
  Frame3d (global @ frameUnits) (Defines local) ->
  DirectionSurfaceFunction3d global ->
  DirectionSurfaceFunction3d local
relativeTo frame = placeIn (Frame3d.inverse frame)
