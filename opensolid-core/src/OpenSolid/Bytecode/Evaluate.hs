{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Bytecode.Evaluate
  ( Compiled (Constant, Bytecode)
  , curveValue1D
  , curveRange1D
  , curveValue2D
  , curveRange2D
  , vectorCurveValue2D
  , vectorCurveRange2D
  , curveValue3D
  , curveRange3D
  , vectorCurveValue3D
  , vectorCurveRange3D
  , surfaceValue1D
  , surfaceRange1D
  , surfaceValue2D
  , surfaceRange2D
  , vectorSurfaceValue2D
  , vectorSurfaceRange2D
  , surfaceValue3D
  , surfaceRange3D
  , vectorSurfaceValue3D
  , vectorSurfaceRange3D
  , solveMonotonicSurfaceU
  , solveMonotonicSurfaceV
  )
where

import Data.ByteString.Unsafe qualified
import GHC.Exts (Addr#, Ptr (Ptr))
import GHC.Foreign (CString)
import OpenSolid.Binary (ByteString)
import OpenSolid.Bounds2D qualified as Bounds2D
import OpenSolid.Bounds3D qualified as Bounds3D
import OpenSolid.IO qualified as IO
import OpenSolid.Interval (Interval (I#, Interval))
import OpenSolid.Interval qualified as Interval
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D qualified as Point2D
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds2D (Bounds2D, PositionBounds2D)
  , Bounds3D (PositionBounds3D)
  , Point2D (Point2D, Position2D)
  , Point3D (Position3D)
  , Vector3D (V3D#)
  , VectorBounds2D (VectorBounds2D)
  , VectorBounds3D (VB3D#)
  )
import OpenSolid.Quantity (Quantity (Q#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2D (Vector2D (V2D#))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import System.IO.Unsafe qualified

data Compiled input output where
  Constant :: output -> Compiled input output
  Bytecode :: ByteString -> Compiled input output

{-# INLINE callFunction #-}
callFunction :: ByteString -> (Addr# -> a) -> a
callFunction bytecode callback =
  System.IO.Unsafe.unsafeDupablePerformIO do
    Data.ByteString.Unsafe.unsafeUseAsCString bytecode do
      \(Ptr f#) -> IO.succeed (callback f#)

----- Curve1D -----

curveValue1D :: Compiled Number Number -> Number -> Quantity units
curveValue1D (Constant value) _ = Quantity.unerase value
curveValue1D (Bytecode bytecode) (Q# t#) = callFunction bytecode \f# ->
  Q# (opensolid_cmm_curve1d_value f# t#)

curveRange1D :: Compiled Number Number -> Interval Unitless -> Interval units
curveRange1D (Constant value) _ = Interval.constant (Quantity.unerase value)
curveRange1D (Bytecode bytecode) (I# tLow# tHigh#) = callFunction bytecode \f# -> do
  let !(# xLow#, xHigh# #) = opensolid_cmm_curve1d_bounds f# tLow# tHigh#
  I# xLow# xHigh#

----- Curve2D -----

curveValue2D :: Compiled Number (Point2D Unitless) -> Number -> Point2D units
curveValue2D (Constant value) _ = Point2D.unerase value
curveValue2D (Bytecode bytecode) tValue = curveBytecodeValue2D bytecode tValue & Point2D.unerase

curveRange2D :: Compiled Number (Point2D Unitless) -> Interval Unitless -> Bounds2D units
curveRange2D (Constant value) _ = Bounds2D.constant (Point2D.unerase value)
curveRange2D (Bytecode bytecode) tRange = curveBytecodeRange2D bytecode tRange & Bounds2D.unerase

curveBytecodeValue2D :: ByteString -> Number -> Point2D Unitless
curveBytecodeValue2D bytecode tValue = Position2D (vectorCurveBytecodeValue2D bytecode tValue)

curveBytecodeRange2D :: ByteString -> Interval Unitless -> Bounds2D Unitless
curveBytecodeRange2D bytecode tRange = PositionBounds2D (vectorCurveBytecodeRange2D bytecode tRange)

----- VectorCurve2D -----

vectorCurveValue2D :: Compiled Number (Vector2D Unitless) -> Number -> Vector2D units
vectorCurveValue2D (Constant value) _ = Vector2D.unerase value
vectorCurveValue2D (Bytecode bytecode) tValue =
  vectorCurveBytecodeValue2D bytecode tValue & Vector2D.unerase

vectorCurveRange2D :: Compiled Number (Vector2D Unitless) -> Interval Unitless -> VectorBounds2D units
vectorCurveRange2D (Constant value) _ = VectorBounds2D.constant (Vector2D.unerase value)
vectorCurveRange2D (Bytecode bytecode) tRange =
  vectorCurveBytecodeRange2D bytecode tRange & VectorBounds2D.unerase

vectorCurveBytecodeValue2D :: ByteString -> Number -> Vector2D Unitless
vectorCurveBytecodeValue2D bytecode (Q# t#) = callFunction bytecode \f# -> do
  let !(# x#, y# #) = opensolid_cmm_curve2d_value f# t#
  V2D# x# y#

vectorCurveBytecodeRange2D :: ByteString -> Interval Unitless -> VectorBounds2D Unitless
vectorCurveBytecodeRange2D bytecode (I# tLow# tHigh#) = callFunction bytecode \f# -> do
  let !(# xLow#, xHigh#, yLow#, yHigh# #) = opensolid_cmm_curve2d_bounds f# tLow# tHigh#
  VectorBounds2D (I# xLow# xHigh#) (I# yLow# yHigh#)

----- Curve3D -----

curveValue3D :: Compiled Number (Point3D Void) -> Number -> Point3D space
curveValue3D (Constant value) _ = Point3D.unerase value
curveValue3D (Bytecode bytecode) tValue = curveBytecodeValue3D bytecode tValue & Point3D.unerase

curveRange3D :: Compiled Number (Point3D Void) -> Interval Unitless -> Bounds3D space
curveRange3D (Constant value) _ = Bounds3D.constant (Point3D.unerase value)
curveRange3D (Bytecode bytecode) tRange = curveBytecodeRange3D bytecode tRange & Bounds3D.unerase

curveBytecodeValue3D :: ByteString -> Number -> Point3D Void
curveBytecodeValue3D bytecode tValue =
  Position3D (Vector3D.coerce (vectorCurveBytecodeValue3D bytecode tValue))

curveBytecodeRange3D :: ByteString -> Interval Unitless -> Bounds3D Void
curveBytecodeRange3D bytecode tRange =
  PositionBounds3D (VectorBounds3D.coerce (vectorCurveBytecodeRange3D bytecode tRange))

----- VectorCurve3D -----

vectorCurveValue3D :: Compiled Number (Vector3D Unitless Void) -> Number -> Vector3D units space
vectorCurveValue3D (Constant value) _ = Vector3D.unerase value
vectorCurveValue3D (Bytecode bytecode) tValue =
  vectorCurveBytecodeValue3D bytecode tValue & Vector3D.unerase

vectorCurveRange3D ::
  Compiled Number (Vector3D Unitless Void) ->
  Interval Unitless ->
  VectorBounds3D units space
vectorCurveRange3D (Constant value) _ = VectorBounds3D.constant (Vector3D.unerase value)
vectorCurveRange3D (Bytecode bytecode) tRange =
  vectorCurveBytecodeRange3D bytecode tRange & VectorBounds3D.unerase

vectorCurveBytecodeValue3D :: ByteString -> Number -> Vector3D Unitless Void
vectorCurveBytecodeValue3D bytecode (Q# t#) = callFunction bytecode \f# -> do
  let !(# x#, y#, z# #) = opensolid_cmm_curve3d_value f# t#
  V3D# x# y# z#

vectorCurveBytecodeRange3D :: ByteString -> Interval Unitless -> VectorBounds3D Unitless Void
vectorCurveBytecodeRange3D bytecode (I# tLow# tHigh#) = callFunction bytecode \f# -> do
  let !(# xLow#, xHigh#, yLow#, yHigh#, zLow#, zHigh# #) =
        opensolid_cmm_curve3d_bounds f# tLow# tHigh#
  VB3D# xLow# xHigh# yLow# yHigh# zLow# zHigh#

----- Surface1D -----

surfaceValue1D :: Compiled UvPoint Number -> UvPoint -> Quantity units
surfaceValue1D (Constant value) _ = Quantity.unerase value
surfaceValue1D (Bytecode bytecode) (Point2D (Q# u#) (Q# v#)) =
  callFunction bytecode \f# ->
    Q# (opensolid_cmm_surface1d_value f# u# v#)

surfaceRange1D :: Compiled UvPoint Number -> UvBounds -> Interval units
surfaceRange1D (Constant value) _ = Interval.constant (Quantity.unerase value)
surfaceRange1D (Bytecode bytecode) (Bounds2D (I# uLow# uHigh#) (I# vLow# vHigh#)) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh# #) = opensolid_cmm_surface1d_bounds f# uLow# uHigh# vLow# vHigh#
    I# xLow# xHigh#

----- Surface2D -----

surfaceValue2D :: Compiled UvPoint (Point2D Unitless) -> UvPoint -> Point2D units
surfaceValue2D (Constant value) _ = Point2D.unerase value
surfaceValue2D (Bytecode bytecode) uvPoint =
  surfaceBytecodeValue2D bytecode uvPoint & Point2D.unerase

surfaceRange2D :: Compiled UvPoint (Point2D Unitless) -> UvBounds -> Bounds2D units
surfaceRange2D (Constant value) _ = Bounds2D.constant (Point2D.unerase value)
surfaceRange2D (Bytecode bytecode) uvBounds =
  surfaceBytecodeRange2D bytecode uvBounds & Bounds2D.unerase

surfaceBytecodeValue2D :: ByteString -> UvPoint -> Point2D Unitless
surfaceBytecodeValue2D bytecode uvPoint =
  Position2D (vectorSurfaceBytecodeValue2D bytecode uvPoint)

surfaceBytecodeRange2D :: ByteString -> UvBounds -> Bounds2D Unitless
surfaceBytecodeRange2D bytecode uvBounds =
  PositionBounds2D (vectorSurfaceBytecodeRange2D bytecode uvBounds)

----- VectorSurface2D -----

vectorSurfaceValue2D :: Compiled UvPoint (Vector2D Unitless) -> UvPoint -> Vector2D units
vectorSurfaceValue2D (Constant value) _ = Vector2D.unerase value
vectorSurfaceValue2D (Bytecode bytecode) uvPoint =
  vectorSurfaceBytecodeValue2D bytecode uvPoint & Vector2D.unerase

vectorSurfaceRange2D :: Compiled UvPoint (Vector2D Unitless) -> UvBounds -> VectorBounds2D units
vectorSurfaceRange2D (Constant value) _ = VectorBounds2D.constant (Vector2D.unerase value)
vectorSurfaceRange2D (Bytecode bytecode) uvBounds =
  vectorSurfaceBytecodeRange2D bytecode uvBounds & VectorBounds2D.unerase

vectorSurfaceBytecodeValue2D :: ByteString -> UvPoint -> Vector2D Unitless
vectorSurfaceBytecodeValue2D bytecode (Point2D (Q# u#) (Q# v#)) =
  callFunction bytecode \f# -> do
    let !(# x#, y# #) = opensolid_cmm_surface2d_value f# u# v#
    V2D# x# y#

vectorSurfaceBytecodeRange2D :: ByteString -> UvBounds -> VectorBounds2D Unitless
vectorSurfaceBytecodeRange2D bytecode (Bounds2D (I# uLow# uHigh#) (I# vLow# vHigh#)) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh#, yLow#, yHigh# #) =
          opensolid_cmm_surface2d_bounds f# uLow# uHigh# vLow# vHigh#
    VectorBounds2D (I# xLow# xHigh#) (I# yLow# yHigh#)

----- Surface3D -----

surfaceValue3D :: Compiled UvPoint (Point3D Void) -> UvPoint -> Point3D space
surfaceValue3D (Constant value) _ = Point3D.unerase value
surfaceValue3D (Bytecode bytecode) uvPoint =
  surfaceBytecodeValue3D bytecode uvPoint & Point3D.unerase

surfaceRange3D :: Compiled UvPoint (Point3D Void) -> UvBounds -> Bounds3D units
surfaceRange3D (Constant value) _ = Bounds3D.constant (Point3D.unerase value)
surfaceRange3D (Bytecode bytecode) uvBounds =
  surfaceBytecodeRange3D bytecode uvBounds & Bounds3D.unerase

surfaceBytecodeValue3D :: ByteString -> UvPoint -> Point3D Void
surfaceBytecodeValue3D bytecode uvPoint =
  Position3D (Vector3D.coerce (vectorSurfaceBytecodeValue3D bytecode uvPoint))

surfaceBytecodeRange3D :: ByteString -> UvBounds -> Bounds3D Void
surfaceBytecodeRange3D bytecode uvBounds =
  PositionBounds3D (VectorBounds3D.coerce (vectorSurfaceBytecodeRange3D bytecode uvBounds))

----- VectorSurface3D -----

vectorSurfaceValue3D ::
  Compiled UvPoint (Vector3D Unitless Void) ->
  UvPoint ->
  Vector3D units space
vectorSurfaceValue3D (Constant value) _ = Vector3D.unerase value
vectorSurfaceValue3D (Bytecode bytecode) uvPoint =
  vectorSurfaceBytecodeValue3D bytecode uvPoint & Vector3D.unerase

vectorSurfaceRange3D ::
  Compiled UvPoint (Vector3D Unitless Void) ->
  UvBounds ->
  VectorBounds3D units space
vectorSurfaceRange3D (Constant value) _ = VectorBounds3D.constant (Vector3D.unerase value)
vectorSurfaceRange3D (Bytecode bytecode) uvBounds =
  vectorSurfaceBytecodeRange3D bytecode uvBounds & VectorBounds3D.unerase

vectorSurfaceBytecodeValue3D :: ByteString -> UvPoint -> Vector3D Unitless Void
vectorSurfaceBytecodeValue3D bytecode (Point2D (Q# u#) (Q# v#)) =
  callFunction bytecode \f# -> do
    let !(# x#, y#, z# #) = opensolid_cmm_surface3d_value f# u# v#
    V3D# x# y# z#

vectorSurfaceBytecodeRange3D :: ByteString -> UvBounds -> VectorBounds3D Unitless Void
vectorSurfaceBytecodeRange3D bytecode (Bounds2D (I# uLow# uHigh#) (I# vLow# vHigh#)) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh#, yLow#, yHigh#, zLow#, zHigh# #) =
          opensolid_cmm_surface3d_bounds f# uLow# uHigh# vLow# vHigh#
    VB3D# xLow# xHigh# yLow# yHigh# zLow# zHigh#

----- Newton-Raphson solving -----

callSolver :: ByteString -> ByteString -> (CString -> CString -> IO a) -> a
callSolver functionBytes derivativeBytes callback =
  System.IO.Unsafe.unsafeDupablePerformIO $
    Data.ByteString.Unsafe.unsafeUseAsCString functionBytes \functionPointer ->
      Data.ByteString.Unsafe.unsafeUseAsCString derivativeBytes \derivativePointer ->
        callback functionPointer derivativePointer

solveMonotonicSurfaceU ::
  Tolerance units =>
  Compiled UvPoint Number ->
  Compiled UvPoint Number ->
  Interval Unitless ->
  Number ->
  Number
solveMonotonicSurfaceU (Constant _) _ (Interval u1 _) _ = u1
solveMonotonicSurfaceU linearFunction (Constant slope) (Interval u1 _) vValue =
  if slope == 0.0 then u1 else u1 - surfaceValue1D linearFunction (Point2D u1 vValue) / slope
solveMonotonicSurfaceU (Bytecode functionBytecode) (Bytecode derivativeBytecode) uBounds vValue =
  callSolver functionBytecode derivativeBytecode $
    \functionPointer derivativePointer ->
      IO.succeed $
        Number.fromDouble $
          opensolid_solve_monotonic_surface_u
            (Number.toDouble (Quantity.erase ?tolerance))
            functionPointer
            derivativePointer
            (Number.toDouble (Interval.lower uBounds))
            (Number.toDouble (Interval.upper uBounds))
            (Number.toDouble vValue)

solveMonotonicSurfaceV ::
  Tolerance units =>
  Compiled UvPoint Number ->
  Compiled UvPoint Number ->
  Number ->
  Interval Unitless ->
  Number
solveMonotonicSurfaceV (Constant _) _ _ (Interval v1 _) = v1
solveMonotonicSurfaceV linearFunction (Constant slope) uValue (Interval v1 _) =
  if slope == 0.0 then v1 else v1 - surfaceValue1D linearFunction (Point2D uValue v1) / slope
solveMonotonicSurfaceV (Bytecode functionBytecode) (Bytecode derivativeBytecode) uValue vBounds =
  callSolver functionBytecode derivativeBytecode $
    \functionPointer derivativePointer ->
      IO.succeed $
        Number.fromDouble $
          opensolid_solve_monotonic_surface_v
            (Number.toDouble (Quantity.erase ?tolerance))
            functionPointer
            derivativePointer
            (Number.toDouble uValue)
            (Number.toDouble (Interval.lower vBounds))
            (Number.toDouble (Interval.upper vBounds))

----- Foreign functions -----

foreign import prim "opensolid_cmm_curve1d_value"
  opensolid_cmm_curve1d_value :: Addr# -> Double# -> Double#

foreign import prim "opensolid_cmm_curve1d_bounds"
  opensolid_cmm_curve1d_bounds :: Addr# -> Double# -> Double# -> (# Double#, Double# #)

foreign import prim "opensolid_cmm_curve2d_value"
  opensolid_cmm_curve2d_value :: Addr# -> Double# -> (# Double#, Double# #)

foreign import prim "opensolid_cmm_curve2d_bounds"
  opensolid_cmm_curve2d_bounds ::
    Addr# ->
    Double# ->
    Double# ->
    (# Double#, Double#, Double#, Double# #)

foreign import prim "opensolid_cmm_curve3d_value"
  opensolid_cmm_curve3d_value :: Addr# -> Double# -> (# Double#, Double#, Double# #)

foreign import prim "opensolid_cmm_curve3d_bounds"
  opensolid_cmm_curve3d_bounds ::
    Addr# ->
    Double# ->
    Double# ->
    (# Double#, Double#, Double#, Double#, Double#, Double# #)

foreign import prim "opensolid_cmm_surface1d_value"
  opensolid_cmm_surface1d_value :: Addr# -> Double# -> Double# -> Double#

foreign import prim "opensolid_cmm_surface1d_bounds"
  opensolid_cmm_surface1d_bounds ::
    Addr# ->
    Double# ->
    Double# ->
    Double# ->
    Double# ->
    (# Double#, Double# #)

foreign import prim "opensolid_cmm_surface2d_value"
  opensolid_cmm_surface2d_value :: Addr# -> Double# -> Double# -> (# Double#, Double# #)

foreign import prim "opensolid_cmm_surface2d_bounds"
  opensolid_cmm_surface2d_bounds ::
    Addr# ->
    Double# ->
    Double# ->
    Double# ->
    Double# ->
    (# Double#, Double#, Double#, Double# #)

foreign import prim "opensolid_cmm_surface3d_value"
  opensolid_cmm_surface3d_value :: Addr# -> Double# -> Double# -> (# Double#, Double#, Double# #)

foreign import prim "opensolid_cmm_surface3d_bounds"
  opensolid_cmm_surface3d_bounds ::
    Addr# ->
    Double# ->
    Double# ->
    Double# ->
    Double# ->
    (# Double#, Double#, Double#, Double#, Double#, Double# #)

foreign import ccall unsafe "bytecode.h opensolid_solve_monotonic_surface_u"
  opensolid_solve_monotonic_surface_u ::
    Double -> CString -> CString -> Double -> Double -> Double -> Double

foreign import ccall unsafe "bytecode.h opensolid_solve_monotonic_surface_v"
  opensolid_solve_monotonic_surface_v ::
    Double -> CString -> CString -> Double -> Double -> Double -> Double
