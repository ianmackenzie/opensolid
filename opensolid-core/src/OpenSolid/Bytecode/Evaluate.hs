{-# LANGUAGE UnboxedTuples #-}

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
import GHC.Exts (Addr#, Ptr (Ptr))
import GHC.Foreign (CString)
import OpenSolid.Binary (ByteString)
import OpenSolid.Bounds (Bounds (Bounds, Bounds#))
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.IO qualified as IO
import OpenSolid.Number qualified as Number
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Vector2D (Vector2D (Vector2D#))
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Prelude
import OpenSolid.Primitives (Vector3D (Vector3D#), VectorBounds3D (VectorBounds3D#))
import OpenSolid.Quantity (Quantity (Quantity#))
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Unboxed.Math
import OpenSolid.UvBounds (UvBounds)
import OpenSolid.UvPoint (UvPoint)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))
import OpenSolid.VectorBounds2D qualified as VectorBounds2D
import OpenSolid.VectorBounds3D qualified as VectorBounds3D
import System.IO.Unsafe qualified

data Compiled input output
  = Constant output
  | Bytecode ByteString

{-# INLINE callFunction #-}
callFunction :: ByteString -> (Addr# -> IO a) -> a
callFunction bytecode callback =
  System.IO.Unsafe.unsafeDupablePerformIO $
    Data.ByteString.Unsafe.unsafeUseAsCString bytecode \(Ptr f#) -> callback f#

curve1dValue :: Compiled Number (Quantity units1) -> Number -> Quantity units2
curve1dValue (Constant value) _ = Quantity.coerce value
curve1dValue (Bytecode bytecode) (Quantity# t#) =
  callFunction bytecode \f# -> do
    let x# = opensolid_cmm_curve1d_value f# t#
    IO.succeed (Quantity# x#)

curve1dBounds :: Compiled Number (Quantity units1) -> Bounds Unitless -> Bounds units2
curve1dBounds (Constant value) _ = Bounds.constant (Quantity.coerce value)
curve1dBounds (Bytecode bytecode) (Bounds# tLow# tHigh#) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh# #) = opensolid_cmm_curve1d_bounds f# tLow# tHigh#
    IO.succeed (Bounds# xLow# xHigh#)

curve2dValue :: Compiled Number (Vector2D units1 space1) -> Number -> Vector2D units2 space2
curve2dValue (Constant value) _ = Vector2D.coerce value
curve2dValue (Bytecode bytecode) (Quantity# t#) =
  callFunction bytecode \f# -> do
    let !(# x#, y# #) = opensolid_cmm_curve2d_value f# t#
    IO.succeed (Vector2D# x# y#)

curve2dBounds ::
  Compiled Number (Vector2D units1 space1) ->
  Bounds Unitless ->
  VectorBounds2D units2 space2
curve2dBounds (Constant value) _ = VectorBounds2D.constant (Vector2D.coerce value)
curve2dBounds (Bytecode bytecode) (Bounds# tLow# tHigh#) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh#, yLow#, yHigh# #) = opensolid_cmm_curve2d_bounds f# tLow# tHigh#
    IO.succeed (VectorBounds2D (Bounds# xLow# xHigh#) (Bounds# yLow# yHigh#))

curve3dValue :: Compiled Number (Vector3D units1 space1) -> Number -> Vector3D units2 space2
curve3dValue (Constant value) _ = Vector3D.coerce value
curve3dValue (Bytecode bytecode) (Quantity# t#) =
  callFunction bytecode \f# -> do
    let !(# x#, y#, z# #) = opensolid_cmm_curve3d_value f# t#
    IO.succeed (Vector3D# x# y# z#)

curve3dBounds ::
  Compiled Number (Vector3D units1 space1) ->
  Bounds Unitless ->
  VectorBounds3D units2 space2
curve3dBounds (Constant value) _ = VectorBounds3D.constant (Vector3D.coerce value)
curve3dBounds (Bytecode bytecode) (Bounds# tLow# tHigh#) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh#, yLow#, yHigh#, zLow#, zHigh# #) =
          opensolid_cmm_curve3d_bounds f# tLow# tHigh#
    IO.succeed (VectorBounds3D# xLow# xHigh# yLow# yHigh# zLow# zHigh#)

surface1dValue :: Compiled UvPoint (Quantity units1) -> UvPoint -> Quantity units2
surface1dValue (Constant value) _ = Quantity.coerce value
surface1dValue (Bytecode bytecode) (Point2D (Quantity# u#) (Quantity# v#)) =
  callFunction bytecode \f# -> do
    let x# = opensolid_cmm_surface1d_value f# u# v#
    IO.succeed (Quantity# x#)

surface1dBounds :: Compiled UvPoint (Quantity units1) -> UvBounds -> Bounds units2
surface1dBounds (Constant value) _ = Bounds.constant (Quantity.coerce value)
surface1dBounds (Bytecode bytecode) (Bounds2D (Bounds# uLow# uHigh#) (Bounds# vLow# vHigh#)) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh# #) = opensolid_cmm_surface1d_bounds f# uLow# uHigh# vLow# vHigh#
    IO.succeed (Bounds# xLow# xHigh#)

surface2dValue ::
  Compiled UvPoint (Vector2D units1 space1) ->
  UvPoint ->
  Vector2D units2 space2
surface2dValue (Constant value) _ = Vector2D.coerce value
surface2dValue (Bytecode bytecode) (Point2D (Quantity# u#) (Quantity# v#)) =
  callFunction bytecode \f# -> do
    let !(# x#, y# #) = opensolid_cmm_surface2d_value f# u# v#
    IO.succeed (Vector2D# x# y#)

surface2dBounds ::
  Compiled UvPoint (Vector2D units1 space1) ->
  UvBounds ->
  VectorBounds2D units2 space2
surface2dBounds (Constant value) _ = VectorBounds2D.constant (Vector2D.coerce value)
surface2dBounds (Bytecode bytecode) (Bounds2D (Bounds# uLow# uHigh#) (Bounds# vLow# vHigh#)) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh#, yLow#, yHigh# #) =
          opensolid_cmm_surface2d_bounds f# uLow# uHigh# vLow# vHigh#
    IO.succeed (VectorBounds2D (Bounds# xLow# xHigh#) (Bounds# yLow# yHigh#))

surface3dValue ::
  Compiled UvPoint (Vector3D units1 space1) ->
  UvPoint ->
  Vector3D units2 space2
surface3dValue (Constant value) _ = Vector3D.coerce value
surface3dValue (Bytecode bytecode) (Point2D (Quantity# u#) (Quantity# v#)) =
  callFunction bytecode \f# -> do
    let !(# x#, y#, z# #) = opensolid_cmm_surface3d_value f# u# v#
    IO.succeed (Vector3D# x# y# z#)

surface3dBounds ::
  Compiled UvPoint (Vector3D units1 space1) ->
  UvBounds ->
  VectorBounds3D units2 space2
surface3dBounds (Constant value) _ = VectorBounds3D.constant (Vector3D.coerce value)
surface3dBounds (Bytecode bytecode) (Bounds2D (Bounds# uLow# uHigh#) (Bounds# vLow# vHigh#)) =
  callFunction bytecode \f# -> do
    let !(# xLow#, xHigh#, yLow#, yHigh#, zLow#, zHigh# #) =
          opensolid_cmm_surface3d_bounds f# uLow# uHigh# vLow# vHigh#
    IO.succeed (VectorBounds3D# xLow# xHigh# yLow# yHigh# zLow# zHigh#)

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
  Bounds Unitless ->
  Number ->
  Number
solveMonotonicSurfaceU (Constant _) _ (Bounds u1 _) _ = u1
solveMonotonicSurfaceU linearFunction (Constant slope) (Bounds u1 _) vValue =
  if slope == 0 then u1 else u1 .-. surface1dValue linearFunction (Point2D u1 vValue) ./. slope
solveMonotonicSurfaceU (Bytecode functionBytecode) (Bytecode derivativeBytecode) uBounds vValue =
  callSolver functionBytecode derivativeBytecode $
    \functionPointer derivativePointer ->
      IO.succeed $
        Number.fromDouble $
          opensolid_solve_monotonic_surface_u
            (Number.toDouble (Quantity.coerce ?tolerance))
            functionPointer
            derivativePointer
            (Number.toDouble (Bounds.lower uBounds))
            (Number.toDouble (Bounds.upper uBounds))
            (Number.toDouble vValue)

solveMonotonicSurfaceV ::
  Tolerance units =>
  Compiled UvPoint Number ->
  Compiled UvPoint Number ->
  Number ->
  Bounds Unitless ->
  Number
solveMonotonicSurfaceV (Constant _) _ _ (Bounds v1 _) = v1
solveMonotonicSurfaceV linearFunction (Constant slope) uValue (Bounds v1 _) =
  if slope == 0 then v1 else v1 .-. surface1dValue linearFunction (Point2D uValue v1) ./. slope
solveMonotonicSurfaceV (Bytecode functionBytecode) (Bytecode derivativeBytecode) uValue vBounds =
  callSolver functionBytecode derivativeBytecode $
    \functionPointer derivativePointer ->
      IO.succeed $
        Number.fromDouble $
          opensolid_solve_monotonic_surface_v
            (Number.toDouble (Quantity.coerce ?tolerance))
            functionPointer
            derivativePointer
            (Number.toDouble uValue)
            (Number.toDouble (Bounds.lower vBounds))
            (Number.toDouble (Bounds.upper vBounds))

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
