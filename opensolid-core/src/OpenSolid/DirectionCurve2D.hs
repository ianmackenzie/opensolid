module OpenSolid.DirectionCurve2D
  ( DirectionCurve2D
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
  , placeIn
  , relativeTo
  )
where

import GHC.Records (HasField (getField))
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

newtype DirectionCurve2D space = DirectionCurve2D (VectorCurve2D Unitless space)

instance HasField "derivative" (DirectionCurve2D space) (VectorCurve2D Unitless space) where
  getField = derivative

unsafe :: VectorCurve2D Unitless space -> DirectionCurve2D space
unsafe = DirectionCurve2D

unwrap :: DirectionCurve2D space -> VectorCurve2D Unitless space
unwrap (DirectionCurve2D vectorCurve) = vectorCurve

startValue :: DirectionCurve2D space -> Direction2D space
startValue curve = evaluate curve 0

endValue :: DirectionCurve2D space -> Direction2D space
endValue curve = evaluate curve 1

evaluate :: DirectionCurve2D space -> Number -> Direction2D space
evaluate (DirectionCurve2D vectorCurve) tValue =
  Direction2D.unsafe (VectorCurve2D.evaluate vectorCurve tValue)

evaluateBounds :: DirectionCurve2D space -> Interval Unitless -> DirectionBounds2D space
evaluateBounds (DirectionCurve2D vectorCurve) tBounds =
  DirectionBounds2D.unsafe (VectorCurve2D.evaluateBounds vectorCurve tBounds)

derivative :: DirectionCurve2D space -> VectorCurve2D Unitless space
derivative (DirectionCurve2D vectorCurve) = VectorCurve2D.derivative vectorCurve

constant :: Direction2D space -> DirectionCurve2D space
constant direction = DirectionCurve2D (VectorCurve2D.constant (Vector2D.unit direction))

arc :: Angle -> Angle -> DirectionCurve2D space
arc a b = DirectionCurve2D (VectorCurve2D.arc (Vector2D 1 0) (Vector2D 0 1) a b)

reverse :: DirectionCurve2D space -> DirectionCurve2D space
reverse (DirectionCurve2D vectorCurve) = DirectionCurve2D (VectorCurve2D.reverse vectorCurve)

instance Negation (DirectionCurve2D space) where
  negative (DirectionCurve2D vectorCurve) = DirectionCurve2D (negative vectorCurve)

instance Multiplication Sign (DirectionCurve2D space) (DirectionCurve2D space) where
  Positive .*. curve = curve
  Negative .*. curve = negative curve

instance Multiplication_ Sign (DirectionCurve2D space) (DirectionCurve2D space) where
  Positive ?*? curve = curve
  Negative ?*? curve = negative curve

instance Multiplication (DirectionCurve2D space) Sign (DirectionCurve2D space) where
  curve .*. Positive = curve
  curve .*. Negative = negative curve

instance Multiplication_ (DirectionCurve2D space) Sign (DirectionCurve2D space) where
  curve ?*? Positive = curve
  curve ?*? Negative = negative curve

instance
  Multiplication
    (Quantity units)
    (DirectionCurve2D space)
    (VectorCurve2D units space)
  where
  value .*. DirectionCurve2D vectorCurve = value .*. vectorCurve

instance
  Multiplication
    (DirectionCurve2D space)
    (Quantity units)
    (VectorCurve2D units space)
  where
  DirectionCurve2D vectorCurve .*. value = vectorCurve .*. value

instance
  Multiplication
    (Curve1D units)
    (DirectionCurve2D space)
    (VectorCurve2D units space)
  where
  scalarCurve .*. DirectionCurve2D vectorCurve = scalarCurve .*. vectorCurve

instance
  Multiplication
    (DirectionCurve2D space)
    (Curve1D units)
    (VectorCurve2D units space)
  where
  DirectionCurve2D vectorCurve .*. scalarCurve = vectorCurve .*. scalarCurve

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2D space1) (DirectionCurve2D space2) (Curve1D Unitless)
  where
  DirectionCurve2D lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2D space1) (VectorCurve2D units space2) (Curve1D units)
  where
  DirectionCurve2D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (VectorCurve2D units space1) (DirectionCurve2D space2) (Curve1D units)
  where
  lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2D space1) (Direction2D space2) (Curve1D Unitless)
  where
  DirectionCurve2D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Direction2D space1) (DirectionCurve2D space2) (Curve1D Unitless)
  where
  lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (DirectionCurve2D space1) (Vector2D units space2) (Curve1D units)
  where
  DirectionCurve2D lhs `dot` rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  DotMultiplication (Vector2D units space1) (DirectionCurve2D space2) (Curve1D units)
  where
  lhs `dot` DirectionCurve2D rhs = lhs `dot` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2D space1) (DirectionCurve2D space2) (Curve1D Unitless)
  where
  DirectionCurve2D lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2D space1) (VectorCurve2D units space2) (Curve1D units)
  where
  DirectionCurve2D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (VectorCurve2D units space1) (DirectionCurve2D space2) (Curve1D units)
  where
  lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2D space1) (Direction2D space2) (Curve1D Unitless)
  where
  DirectionCurve2D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Direction2D space1) (DirectionCurve2D space2) (Curve1D Unitless)
  where
  lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (DirectionCurve2D space1) (Vector2D units space2) (Curve1D units)
  where
  DirectionCurve2D lhs `cross` rhs = lhs `cross` rhs

instance
  space1 ~ space2 =>
  CrossMultiplication (Vector2D units space1) (DirectionCurve2D space2) (Curve1D units)
  where
  lhs `cross` DirectionCurve2D rhs = lhs `cross` rhs

instance Composition (Curve1D Unitless) (DirectionCurve2D space) (DirectionCurve2D space) where
  DirectionCurve2D curve `compose` curve1D = DirectionCurve2D (curve `compose` curve1D)

instance
  Composition
    (SurfaceFunction1D Unitless)
    (DirectionCurve2D space)
    (DirectionSurfaceFunction2D space)
  where
  DirectionCurve2D curve `compose` surfaceFunction =
    DirectionSurfaceFunction2D.unsafe (curve `compose` surfaceFunction)

instance
  Composition
    SurfaceParameter
    (DirectionCurve2D space)
    (DirectionSurfaceFunction2D space)
  where
  DirectionCurve2D curve `compose` surfaceParameter =
    DirectionSurfaceFunction2D.unsafe (curve `compose` surfaceParameter)

instance HasField "xComponent" (DirectionCurve2D space) (Curve1D Unitless) where
  getField (DirectionCurve2D curve) = curve.xComponent

instance HasField "yComponent" (DirectionCurve2D space) (Curve1D Unitless) where
  getField (DirectionCurve2D curve) = curve.yComponent

placeIn ::
  Frame2D frameUnits global local ->
  DirectionCurve2D local ->
  DirectionCurve2D global
placeIn frame (DirectionCurve2D curve) = DirectionCurve2D (VectorCurve2D.placeIn frame curve)

relativeTo ::
  Frame2D frameUnits global local ->
  DirectionCurve2D global ->
  DirectionCurve2D local
relativeTo frame = placeIn (Frame2D.inverse frame)
