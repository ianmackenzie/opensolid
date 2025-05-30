module OpenSolid.Bytecode.Evaluate
  ( Compiled (Constant, Bytecode)
  , curve1dValue
  , curve1dBounds
  , curve2dValue
  , curve2dBounds
  , curve3dValue
  , curve3dBounds
  , surface1dValue
  , surface1dBounds
  , surface2dValue
  , surface2dBounds
  , surface3dValue
  , surface3dBounds
  , solveMonotonicSurfaceU
  , solveMonotonicSurfaceV
  )
where

import Data.ByteString.Unsafe qualified
import Foreign (Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified
import GHC.Foreign (CString)
import OpenSolid.Binary (ByteString)
import OpenSolid.Bounds (Bounds (Bounds))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Float qualified as Float
import OpenSolid.IO qualified as IO
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Prelude
import OpenSolid.Primitives (Vector3d (Vector3d), VectorBounds3d (VectorBounds3d))
import OpenSolid.Qty qualified as Qty
import OpenSolid.SurfaceParameter (UvBounds, UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds2d qualified as VectorBounds2d
import OpenSolid.VectorBounds3d qualified as VectorBounds3d
import System.IO.Unsafe qualified
import Prelude (Double)

data Compiled input output
  = Constant output
  | Bytecode ByteString

callFunction :: ByteString -> Int -> (CString -> Ptr Double -> IO a) -> a
callFunction functionBytes numReturnValues callback =
  System.IO.Unsafe.unsafeDupablePerformIO $
    Data.ByteString.Unsafe.unsafeUseAsCString functionBytes \functionPointer ->
      Foreign.Marshal.Alloc.allocaBytes (8 * numReturnValues) \returnValuesPointer ->
        callback functionPointer returnValuesPointer

getReturnValue :: Int -> Ptr Double -> IO Float
getReturnValue index returnValuesPointer =
  IO.map Float.fromDouble (Foreign.peekElemOff returnValuesPointer index)

curve1dValue :: Compiled Float Float -> Float -> Float
curve1dValue (Constant value) _ = value
curve1dValue (Bytecode bytecode) tValue =
  callFunction bytecode 1 $
    \functionPointer returnValuePointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuePointer
      getReturnValue 0 returnValuePointer

curve1dBounds :: Compiled Float Float -> Bounds Unitless -> Bounds Unitless
curve1dBounds (Constant value) _ = Bounds.constant value
curve1dBounds (Bytecode bytecode) (Bounds tLower tUpper) =
  callFunction bytecode 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_bounds
        functionPointer
        (Float.toDouble tLower)
        (Float.toDouble tUpper)
        returnValuesPointer
      lower <- getReturnValue 0 returnValuesPointer
      upper <- getReturnValue 1 returnValuesPointer
      IO.succeed (Bounds lower upper)

curve2dValue ::
  Compiled Float (Vector2d (space @ Unitless)) ->
  Float ->
  Vector2d (space @ Unitless)
curve2dValue (Constant value) _ = value
curve2dValue (Bytecode bytecode) tValue =
  callFunction bytecode 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      IO.succeed (Vector2d x y)

curve2dBounds ::
  Compiled Float (Vector2d (space @ Unitless)) ->
  Bounds Unitless ->
  VectorBounds2d (space @ Unitless)
curve2dBounds (Constant value) _ = VectorBounds2d.constant value
curve2dBounds (Bytecode bytecode) (Bounds tLower tUpper) =
  callFunction bytecode 4 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_bounds
        functionPointer
        (Float.toDouble tLower)
        (Float.toDouble tUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      IO.succeed (VectorBounds2d (Bounds xLower xUpper) (Bounds yLower yUpper))

curve3dValue ::
  Compiled Float (Vector3d (space @ Unitless)) ->
  Float ->
  Vector3d (space @ Unitless)
curve3dValue (Constant value) _ = value
curve3dValue (Bytecode bytecode) tValue =
  callFunction bytecode 3 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      z <- getReturnValue 2 returnValuesPointer
      IO.succeed (Vector3d x y z)

curve3dBounds ::
  Compiled Float (Vector3d (space @ Unitless)) ->
  Bounds Unitless ->
  VectorBounds3d (space @ Unitless)
curve3dBounds (Constant value) _ = VectorBounds3d.constant value
curve3dBounds (Bytecode bytecode) (Bounds tLower tUpper) =
  callFunction bytecode 6 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_curve_bounds
        functionPointer
        (Float.toDouble tLower)
        (Float.toDouble tUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      zLower <- getReturnValue 4 returnValuesPointer
      zUpper <- getReturnValue 5 returnValuesPointer
      IO.succeed (VectorBounds3d (Bounds xLower xUpper) (Bounds yLower yUpper) (Bounds zLower zUpper))

surface1dValue :: Compiled UvPoint Float -> UvPoint -> Float
surface1dValue (Constant value) _ = value
surface1dValue (Bytecode bytecode) (Point2d uValue vValue) =
  callFunction bytecode 1 $
    \functionPointer returnValuePointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuePointer
      getReturnValue 0 returnValuePointer

surface1dBounds :: Compiled UvPoint Float -> UvBounds -> Bounds Unitless
surface1dBounds (Constant value) _ = Bounds.constant value
surface1dBounds (Bytecode bytecode) (Bounds2d (Bounds uLower uUpper) (Bounds vLower vUpper)) =
  callFunction bytecode 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_bounds
        functionPointer
        (Float.toDouble uLower)
        (Float.toDouble uUpper)
        (Float.toDouble vLower)
        (Float.toDouble vUpper)
        returnValuesPointer
      lower <- getReturnValue 0 returnValuesPointer
      upper <- getReturnValue 1 returnValuesPointer
      IO.succeed (Bounds lower upper)

surface2dValue ::
  Compiled UvPoint (Vector2d (space @ Unitless)) ->
  UvPoint ->
  Vector2d (space @ Unitless)
surface2dValue (Constant value) _ = value
surface2dValue (Bytecode bytecode) (Point2d uValue vValue) =
  callFunction bytecode 2 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      IO.succeed (Vector2d x y)

surface2dBounds ::
  Compiled UvPoint (Vector2d (space @ Unitless)) ->
  UvBounds ->
  VectorBounds2d (space @ Unitless)
surface2dBounds (Constant value) _ = VectorBounds2d.constant value
surface2dBounds (Bytecode bytecode) (Bounds2d (Bounds uLower uUpper) (Bounds vLower vUpper)) =
  callFunction bytecode 4 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_bounds
        functionPointer
        (Float.toDouble uLower)
        (Float.toDouble uUpper)
        (Float.toDouble vLower)
        (Float.toDouble vUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      IO.succeed (VectorBounds2d (Bounds xLower xUpper) (Bounds yLower yUpper))

surface3dValue ::
  Compiled UvPoint (Vector3d (space @ Unitless)) ->
  UvPoint ->
  Vector3d (space @ Unitless)
surface3dValue (Constant value) _ = value
surface3dValue (Bytecode bytecode) (Point2d uValue vValue) =
  callFunction bytecode 3 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuesPointer
      x <- getReturnValue 0 returnValuesPointer
      y <- getReturnValue 1 returnValuesPointer
      z <- getReturnValue 2 returnValuesPointer
      IO.succeed (Vector3d x y z)

surface3dBounds ::
  Compiled UvPoint (Vector3d (space @ Unitless)) ->
  UvBounds ->
  VectorBounds3d (space @ Unitless)
surface3dBounds (Constant value) _ = VectorBounds3d.constant value
surface3dBounds (Bytecode bytecode) (Bounds2d (Bounds uLower uUpper) (Bounds vLower vUpper)) =
  callFunction bytecode 6 $
    \functionPointer returnValuesPointer -> IO.do
      opensolid_surface_bounds
        functionPointer
        (Float.toDouble uLower)
        (Float.toDouble uUpper)
        (Float.toDouble vLower)
        (Float.toDouble vUpper)
        returnValuesPointer
      xLower <- getReturnValue 0 returnValuesPointer
      xUpper <- getReturnValue 1 returnValuesPointer
      yLower <- getReturnValue 2 returnValuesPointer
      yUpper <- getReturnValue 3 returnValuesPointer
      zLower <- getReturnValue 4 returnValuesPointer
      zUpper <- getReturnValue 5 returnValuesPointer
      IO.succeed (VectorBounds3d (Bounds xLower xUpper) (Bounds yLower yUpper) (Bounds zLower zUpper))

callSolver :: ByteString -> ByteString -> (CString -> CString -> IO a) -> a
callSolver functionBytes derivativeBytes callback =
  System.IO.Unsafe.unsafeDupablePerformIO $
    Data.ByteString.Unsafe.unsafeUseAsCString functionBytes \functionPointer ->
      Data.ByteString.Unsafe.unsafeUseAsCString derivativeBytes \derivativePointer ->
        callback functionPointer derivativePointer

solveMonotonicSurfaceU ::
  Tolerance units =>
  Compiled UvPoint Float ->
  Compiled UvPoint Float ->
  Bounds Unitless ->
  Float ->
  Float
solveMonotonicSurfaceU (Constant _) _ (Bounds u1 _) _ = u1
solveMonotonicSurfaceU linearFunction (Constant slope) (Bounds u1 _) vValue =
  if slope == 0.0 then u1 else u1 - surface1dValue linearFunction (Point2d u1 vValue) / slope
solveMonotonicSurfaceU (Bytecode functionBytecode) (Bytecode derivativeBytecode) uBounds vValue =
  callSolver functionBytecode derivativeBytecode $
    \functionPointer derivativePointer ->
      IO.succeed $
        Float.fromDouble $
          opensolid_solve_monotonic_surface_u
            (Float.toDouble (Qty.coerce ?tolerance))
            functionPointer
            derivativePointer
            (Float.toDouble (Bounds.lower uBounds))
            (Float.toDouble (Bounds.upper uBounds))
            (Float.toDouble vValue)

solveMonotonicSurfaceV ::
  Tolerance units =>
  Compiled UvPoint Float ->
  Compiled UvPoint Float ->
  Float ->
  Bounds Unitless ->
  Float
solveMonotonicSurfaceV (Constant _) _ _ (Bounds v1 _) = v1
solveMonotonicSurfaceV linearFunction (Constant slope) uValue (Bounds v1 _) =
  if slope == 0.0 then v1 else v1 - surface1dValue linearFunction (Point2d uValue v1) / slope
solveMonotonicSurfaceV (Bytecode functionBytecode) (Bytecode derivativeBytecode) uValue vBounds =
  callSolver functionBytecode derivativeBytecode $
    \functionPointer derivativePointer ->
      IO.succeed $
        Float.fromDouble $
          opensolid_solve_monotonic_surface_v
            (Float.toDouble (Qty.coerce ?tolerance))
            functionPointer
            derivativePointer
            (Float.toDouble uValue)
            (Float.toDouble (Bounds.lower vBounds))
            (Float.toDouble (Bounds.upper vBounds))

foreign import ccall unsafe "bytecode.h opensolid_curve_value"
  opensolid_curve_value ::
    CString -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "bytecode.h opensolid_curve_bounds"
  opensolid_curve_bounds ::
    CString -> Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "bytecode.h opensolid_surface_value"
  opensolid_surface_value ::
    CString -> Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "bytecode.h opensolid_surface_bounds"
  opensolid_surface_bounds ::
    CString -> Double -> Double -> Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "bytecode.h opensolid_solve_monotonic_surface_u"
  opensolid_solve_monotonic_surface_u ::
    Double -> CString -> CString -> Double -> Double -> Double -> Double

foreign import ccall unsafe "bytecode.h opensolid_solve_monotonic_surface_v"
  opensolid_solve_monotonic_surface_v ::
    Double -> CString -> CString -> Double -> Double -> Double -> Double
