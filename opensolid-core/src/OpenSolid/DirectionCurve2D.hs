module OpenSolid.DirectionCurve2D
  ( DirectionCurve2D
  , unsafe
  , unwrap
  , startValue
  , endValue
  , value
  , bounds
  , derivative
  , constant
  , arc
  , reverse
  , placeIn
  , relativeTo
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Curve1D (Curve1D)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.DirectionBounds2D (DirectionBounds2D)
import OpenSolid.DirectionBounds2D qualified as DirectionBounds2D
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2D (DirectionSurfaceFunction2D)
import {-# SOURCE #-} OpenSolid.DirectionSurfaceFunction2D qualified as DirectionSurfaceFunction2D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Interval (Interval)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.SurfaceFunction1D (SurfaceFunction1D)
import OpenSolid.SurfaceParameter (SurfaceParameter)
import OpenSolid.Vector2D (Vector2D (Vector2D))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.VectorCurve2D (VectorCurve2D)
import OpenSolid.VectorCurve2D qualified as VectorCurve2D

newtype DirectionCurve2D = DirectionCurve2D (VectorCurve2D Unitless)

unsafe :: VectorCurve2D Unitless -> DirectionCurve2D
unsafe = DirectionCurve2D

unwrap :: DirectionCurve2D -> VectorCurve2D Unitless
unwrap (DirectionCurve2D vectorCurve) = vectorCurve

startValue :: DirectionCurve2D -> Direction2D
startValue curve = value curve 0.0

endValue :: DirectionCurve2D -> Direction2D
endValue curve = value curve 1.0

value :: DirectionCurve2D -> Number -> Direction2D
value (DirectionCurve2D vectorCurve) tValue =
  Direction2D.unsafe (VectorCurve2D.value vectorCurve tValue)

bounds :: DirectionCurve2D -> Interval Unitless -> DirectionBounds2D
bounds (DirectionCurve2D vectorCurve) tBounds =
  DirectionBounds2D.unsafe (VectorCurve2D.bounds vectorCurve tBounds)

derivative :: DirectionCurve2D -> VectorCurve2D Unitless
derivative (DirectionCurve2D vectorCurve) = VectorCurve2D.derivative vectorCurve

constant :: Direction2D -> DirectionCurve2D
constant direction = DirectionCurve2D (VectorCurve2D.constant (Vector2D.unit direction))

arc :: Angle -> Angle -> DirectionCurve2D
arc a b = DirectionCurve2D (VectorCurve2D.arc (Vector2D 1.0 0.0) (Vector2D 0.0 1.0) a b)

reverse :: DirectionCurve2D -> DirectionCurve2D
reverse (DirectionCurve2D vectorCurve) = DirectionCurve2D (VectorCurve2D.reverse vectorCurve)

instance Negation DirectionCurve2D where
  negate (DirectionCurve2D vectorCurve) = DirectionCurve2D (negate vectorCurve)

instance Multiplication Sign DirectionCurve2D DirectionCurve2D where
  Positive * curve = curve
  Negative * curve = -curve

instance Multiplication_ Sign DirectionCurve2D DirectionCurve2D where
  Positive ?*? curve = curve
  Negative ?*? curve = -curve

instance Multiplication DirectionCurve2D Sign DirectionCurve2D where
  curve * Positive = curve
  curve * Negative = -curve

instance Multiplication_ DirectionCurve2D Sign DirectionCurve2D where
  curve ?*? Positive = curve
  curve ?*? Negative = -curve

instance Multiplication (Quantity units) DirectionCurve2D (VectorCurve2D units) where
  quantity * DirectionCurve2D vectorCurve = quantity * vectorCurve

instance Multiplication DirectionCurve2D (Quantity units) (VectorCurve2D units) where
  DirectionCurve2D vectorCurve * quantity = vectorCurve * quantity

instance Multiplication (Curve1D units) DirectionCurve2D (VectorCurve2D units) where
  scalarCurve * DirectionCurve2D vectorCurve = scalarCurve * vectorCurve

instance Multiplication DirectionCurve2D (Curve1D units) (VectorCurve2D units) where
  DirectionCurve2D vectorCurve * scalarCurve = vectorCurve * scalarCurve

instance DotMultiplication DirectionCurve2D DirectionCurve2D (Curve1D Unitless) where
  DirectionCurve2D lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance DotMultiplication DirectionCurve2D (VectorCurve2D units) (Curve1D units) where
  DirectionCurve2D lhs `dot` rhs = lhs `dot` rhs

instance DotMultiplication (VectorCurve2D units) DirectionCurve2D (Curve1D units) where
  lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance DotMultiplication DirectionCurve2D Direction2D (Curve1D Unitless) where
  DirectionCurve2D lhs `dot` rhs = lhs `dot` rhs

instance DotMultiplication Direction2D DirectionCurve2D (Curve1D Unitless) where
  lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance DotMultiplication DirectionCurve2D (Vector2D units) (Curve1D units) where
  DirectionCurve2D lhs `dot` rhs = lhs `dot` rhs

instance DotMultiplication (Vector2D units) DirectionCurve2D (Curve1D units) where
  lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance CrossMultiplication DirectionCurve2D DirectionCurve2D (Curve1D Unitless) where
  DirectionCurve2D lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance CrossMultiplication DirectionCurve2D (VectorCurve2D units) (Curve1D units) where
  DirectionCurve2D lhs `cross` rhs = lhs `cross` rhs

instance CrossMultiplication (VectorCurve2D units) DirectionCurve2D (Curve1D units) where
  lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance CrossMultiplication DirectionCurve2D Direction2D (Curve1D Unitless) where
  DirectionCurve2D lhs `cross` rhs = lhs `cross` rhs

instance CrossMultiplication Direction2D DirectionCurve2D (Curve1D Unitless) where
  lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance CrossMultiplication DirectionCurve2D (Vector2D units) (Curve1D units) where
  DirectionCurve2D lhs `cross` rhs = lhs `cross` rhs

instance CrossMultiplication (Vector2D units) DirectionCurve2D (Curve1D units) where
  lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance Composition DirectionCurve2D (Curve1D Unitless) DirectionCurve2D where
  DirectionCurve2D curve . curve1D = DirectionCurve2D (curve . curve1D)

instance
  Composition
    DirectionCurve2D
    (SurfaceFunction1D Unitless)
    DirectionSurfaceFunction2D
  where
  DirectionCurve2D curve . surfaceFunction =
    DirectionSurfaceFunction2D.unsafe (curve . surfaceFunction)

instance
  Composition
    DirectionCurve2D
    SurfaceParameter
    DirectionSurfaceFunction2D
  where
  DirectionCurve2D curve . surfaceParameter =
    DirectionSurfaceFunction2D.unsafe (curve . surfaceParameter)

placeIn :: Frame2D frameUnits -> DirectionCurve2D -> DirectionCurve2D
placeIn frame (DirectionCurve2D curve) = DirectionCurve2D (VectorCurve2D.placeIn frame curve)

relativeTo :: Frame2D frameUnits -> DirectionCurve2D -> DirectionCurve2D
relativeTo frame = placeIn (Frame2D.inverse frame)
