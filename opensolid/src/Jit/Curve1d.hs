module Jit.Curve1d (valueFunction, boundsFunction) where

import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import IO qualified
import Jit.Expression (Expression)
import Jit.Expression qualified as Expression
import OpenSolid
import Qty (Qty (Qty))
import Range (Range (Range))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Prelude (Double)

type ValueFunction = Double -> Double

type BoundsFunction units = Double -> Double -> Ptr Double -> IO ()

foreign import ccall unsafe "opensolid_jit_compile_curve1d_value_function"
  opensolid_jit_compile_curve1d_value_function :: Expression.Ptr -> FunPtr ValueFunction

foreign import ccall unsafe "dynamic"
  curve1d_value_function :: FunPtr ValueFunction -> ValueFunction

foreign import ccall unsafe "opensolid_jit_compile_curve1d_bounds_function"
  opensolid_jit_compile_curve1d_bounds_function :: Expression.Ptr -> FunPtr (BoundsFunction units)

foreign import ccall unsafe "dynamic"
  curve1d_bounds_function :: FunPtr (BoundsFunction units) -> BoundsFunction units

valueFunction :: Expression Expression.Curve -> (Float -> Qty units)
valueFunction expression = do
  -- TODO perform garbage collection on JIT-compiled functions:
  -- use GHC.Weak.mkWeak on f# to associate a finalizer with it
  -- that calls a Rust function to delete the underlying JIT-compiled function/module
  let expressionPtr = Expression.toPtr expression
  let f = curve1d_value_function (opensolid_jit_compile_curve1d_value_function expressionPtr)
  \(Qty x) -> Qty (f x)

boundsFunction :: Expression Expression.Curve -> (Range Unitless -> Range units)
boundsFunction expression = do
  let expressionPtr = Expression.toPtr expression
  let f = curve1d_bounds_function (opensolid_jit_compile_curve1d_bounds_function expressionPtr)
  \(Range (Qty xLower) (Qty xUpper)) -> unsafeDupablePerformIO IO.do
    outputs <- Alloc.mallocBytes 16
    f xLower xUpper outputs
    yLower <- Foreign.peekElemOff outputs 0
    yUpper <- Foreign.peekElemOff outputs 1
    Alloc.free outputs
    IO.succeed (Range (Qty yLower) (Qty yUpper))
