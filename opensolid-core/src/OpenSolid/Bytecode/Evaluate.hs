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
import Data.Coerce qualified
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
import OpenSolid.Qty (Qty (Qty))
import OpenSolid.Qty qualified as Qty
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.Vector3d qualified as Vector3d
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

getReturnValue :: Int -> Ptr Double -> IO (Qty units)
getReturnValue index returnValuesPointer =
  IO.map Data.Coerce.coerce (Foreign.peekElemOff @Double returnValuesPointer index)

curve1dValue :: Compiled Float (Qty units1) -> Float -> Qty units2
curve1dValue (Constant value) _ = Qty.coerce value
curve1dValue (Bytecode bytecode) tValue =
  callFunction bytecode 1 $
    \functionPointer returnValuePointer -> IO.do
      opensolid_curve_value
        functionPointer
        (Float.toDouble tValue)
        returnValuePointer
      getReturnValue 0 returnValuePointer

curve1dBounds :: Compiled Float (Qty units1) -> Bounds Unitless -> Bounds units2
curve1dBounds (Constant value) _ = Bounds.constant (Qty.coerce value)
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

curve2dValue :: Compiled Float (Vector2d (space1 @ units1)) -> Float -> Vector2d (space2 @ units2)
curve2dValue (Constant value) _ = Vector2d.coerce value
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
  Compiled Float (Vector2d (space1 @ units1)) ->
  Bounds Unitless ->
  VectorBounds2d (space2 @ units2)
curve2dBounds (Constant value) _ = VectorBounds2d.constant (Vector2d.coerce value)
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

curve3dValue :: Compiled Float (Vector3d (space1 @ units1)) -> Float -> Vector3d (space2 @ units2)
curve3dValue (Constant value) _ = Vector3d.coerce value
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
  Compiled Float (Vector3d (space1 @ units1)) ->
  Bounds Unitless ->
  VectorBounds3d (space2 @ units2)
curve3dBounds (Constant value) _ = VectorBounds3d.constant (Vector3d.coerce value)
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

surface1dValue :: Compiled UvPoint (Qty units1) -> UvPoint -> Qty units2
surface1dValue (Constant value) _ = Qty.coerce value
surface1dValue (Bytecode bytecode) (Point2d uValue vValue) =
  callFunction bytecode 1 $
    \functionPointer returnValuePointer -> IO.do
      opensolid_surface_value
        functionPointer
        (Float.toDouble uValue)
        (Float.toDouble vValue)
        returnValuePointer
      getReturnValue 0 returnValuePointer

surface1dBounds :: Compiled UvPoint (Qty units1) -> UvBounds -> Bounds units2
surface1dBounds (Constant value) _ = Bounds.constant (Qty.coerce value)
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
  Compiled UvPoint (Vector2d (space1 @ units1)) ->
  UvPoint ->
  Vector2d (space2 @ units2)
surface2dValue (Constant value) _ = Vector2d.coerce value
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
  Compiled UvPoint (Vector2d (space1 @ units1)) ->
  UvBounds ->
  VectorBounds2d (space2 @ units2)
surface2dBounds (Constant value) _ = VectorBounds2d.constant (Vector2d.coerce value)
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
  Compiled UvPoint (Vector3d (space1 @ units1)) ->
  UvPoint ->
  Vector3d (space2 @ units2)
surface3dValue (Constant value) _ = Vector3d.coerce value
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
  Compiled UvPoint (Vector3d (space1 @ units1)) ->
  UvBounds ->
  VectorBounds3d (space2 @ units2)
surface3dBounds (Constant value) _ = VectorBounds3d.constant (Vector3d.coerce value)
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
      let x = Bounds xLower xUpper
      let y = Bounds yLower yUpper
      let z = Bounds zLower zUpper
      IO.succeed (VectorBounds3d x y z)

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
