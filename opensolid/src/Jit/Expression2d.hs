module Jit.Expression2d
  ( Expression2d (Expression2d)
  , Curve
  , Surface
  , constant
  , xy
  , interpolateFrom
  , placeIn
  , curve
  , surface
  , transformBy
  )
where

import Arithmetic.Unboxed (Double#)
import Direction2d (Direction2d (Direction2d))
import Foreign (FunPtr, Ptr)
import Foreign qualified
import Foreign.Marshal.Alloc qualified as Alloc
import Frame2d (Frame2d)
import Frame2d qualified
import IO qualified
import Jit.Expression (Expression)
import Jit.Expression qualified as Expression
import Jit.VectorExpression2d (VectorExpression2d (VectorExpression2d))
import Jit.VectorExpression2d qualified as VectorExpression2d
import OpenSolid
import Point2d (Point2d (Point2d, Point2d#))
import Point2d qualified
import Qty (Qty (Qty#))
import System.IO.Unsafe (unsafeDupablePerformIO)
import Transform2d (Transform2d (Transform2d))
import Units qualified
import Uv qualified
import Vector2d (Vector2d)

data Expression2d parameterization
  = Expression2d (Expression parameterization) (Expression parameterization)

type Curve = Expression2d Expression.Curve

type Surface = Expression2d Expression.Surface

instance HasUnits (Expression2d parameterization) where
  type UnitsOf (Expression2d parameterization) = Unitless

instance
  parameterization1 ~ parameterization2 =>
  Units.Coercion
    (Expression2d parameterization1)
    (Expression2d parameterization2)
  where
  coerce = identity

instance
  parameterization1 ~ parameterization2 =>
  Addition
    (Expression2d parameterization1)
    (VectorExpression2d parameterization2)
    (Expression2d parameterization1)
  where
  Expression2d x1 y1 + VectorExpression2d x2 y2 = Expression2d (x1 + x2) (y1 + y2)

instance
  Addition
    (Expression2d parameterization)
    (Vector2d (space @ units))
    (Expression2d parameterization)
  where
  expression + vector = expression + VectorExpression2d.constant vector

instance
  parameterization1 ~ parameterization2 =>
  Subtraction
    (Expression2d parameterization1)
    (Expression2d parameterization2)
    (VectorExpression2d parameterization1)
  where
  Expression2d x1 y1 - Expression2d x2 y2 = VectorExpression2d (x1 - x2) (y1 - y2)

instance
  Subtraction
    (Expression2d parameterization)
    (Point2d (space @ units))
    (VectorExpression2d parameterization)
  where
  expression - point = expression - constant point

instance
  Subtraction
    (Point2d (space @ units))
    (Expression2d parameterization)
    (VectorExpression2d parameterization)
  where
  point - expression = constant point - expression

instance
  parameterization1 ~ parameterization2 =>
  Subtraction
    (Expression2d parameterization1)
    (VectorExpression2d parameterization2)
    (Expression2d parameterization1)
  where
  Expression2d x1 y1 - VectorExpression2d x2 y2 = Expression2d (x1 - x2) (y1 - y2)

instance
  Subtraction
    (Expression2d parameterization)
    (Vector2d (space @ units))
    (Expression2d parameterization)
  where
  expression - vector = expression - VectorExpression2d.constant vector

instance
  Composition
    (Expression parameterization)
    (Expression2d Expression.Curve)
    (Expression2d parameterization)
  where
  Expression2d px py . expression = Expression2d (px . expression) (py . expression)

constant :: Point2d (space @ units) -> Expression2d parameterization
constant (Point2d px py) = Expression2d (Expression.constant px) (Expression.constant py)

xy :: Expression parameterization -> Expression parameterization -> Expression2d parameterization
xy = Expression2d

interpolateFrom ::
  Expression2d parameterization ->
  Expression2d parameterization ->
  Expression parameterization ->
  Expression2d parameterization
interpolateFrom start end t = start + t * (end - start)

placeIn :: Frame2d (space @ units) defines -> Expression2d parameterization -> Expression2d parameterization
placeIn frame (Expression2d x y) = do
  let p0 = Frame2d.originPoint frame
  let i = Vector2d.unit (Frame2d.xDirection frame)
  let j = Vector2d.unit (Frame2d.yDirection frame)
  p0 + x * i + y * j

transformBy ::
  Transform2d a (space @ units) ->
  Expression2d parameterization ->
  Expression2d parameterization
transformBy (Transform2d p0 i j) (Expression2d x y) = p0 + x * i + y * j

foreign import ccall unsafe "opensolid_jit_compile_curve2d"
  opensolid_jit_compile_curve2d ::
    Expression.Ptr ->
    Expression.Ptr ->
    FunPtr (Double# -> Ptr (Qty units) -> IO ())

foreign import ccall unsafe "dynamic"
  curve2d_function ::
    FunPtr (Double# -> Ptr (Qty units) -> IO ()) ->
    (Double# -> Ptr (Qty units) -> IO ())

curve :: Expression2d Expression.Curve -> (Float -> Point2d (space @ units))
curve (Expression2d x y) = do
  let f# = curve2d_function (opensolid_jit_compile_curve2d (Expression.toPtr x) (Expression.toPtr y))
  \(Qty# t#) -> unsafeDupablePerformIO IO.do
    outputs <- Alloc.mallocBytes 16
    f# t# outputs
    px <- Foreign.peekElemOff outputs 0
    py <- Foreign.peekElemOff outputs 1
    Alloc.free outputs
    IO.succeed (Point2d.xy px py)

foreign import ccall unsafe "opensolid_jit_compile_surface2d"
  opensolid_jit_compile_surface2d ::
    Expression.Ptr ->
    Expression.Ptr ->
    FunPtr (Double# -> Double# -> Ptr (Qty units) -> IO ())

foreign import ccall unsafe "dynamic"
  surface2d_function ::
    FunPtr (Double# -> Double# -> Ptr (Qty units) -> IO ()) ->
    (Double# -> Double# -> Ptr (Qty units) -> IO ())

surface :: Expression2d Expression.Surface -> (Uv.Point -> Point2d (space @ units))
surface (Expression2d x y) = do
  let f# = surface2d_function (opensolid_jit_compile_surface2d (Expression.toPtr x) (Expression.toPtr y))
  \(Point2d# u# v#) -> unsafeDupablePerformIO IO.do
    outputs <- Alloc.mallocBytes 16
    f# u# v# outputs
    px <- Foreign.peekElemOff outputs 0
    py <- Foreign.peekElemOff outputs 1
    Alloc.free outputs
    IO.succeed (Point2d.xy px py)
