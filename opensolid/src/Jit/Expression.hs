module Jit.Expression
  ( Expression
  , Curve
  , Surface
  , constant
  , parameter
  , u
  , v
  , Ptr
  , toPtr
  , squared
  , sqrt
  , sin
  , cos
  , interpolateFrom
  , curve
  , surface
  )
where

import Arithmetic.Unboxed (Double#)
import Data.Int (Int64)
import Float qualified
import Foreign (FunPtr)
import Foreign qualified
import {-# SOURCE #-} Jit.Expression2d (Expression2d (Expression2d))
import OpenSolid
import Point2d (Point2d (Point2d#))
import Qty (Qty (Qty#))
import Units qualified
import Uv qualified
import Prelude (Double)

data Curve

data Surface

data Expression parameterization where
  Parameter ::
    Expression Curve
  U ::
    Expression Surface
  V ::
    Expression Surface
  Constant ::
    Float -> Expression parameterization
  Negated ::
    Expression parameterization ->
    Expression parameterization
  Sum ::
    Expression parameterization ->
    Expression parameterization ->
    Expression parameterization
  Difference ::
    Expression parameterization ->
    Expression parameterization ->
    Expression parameterization
  Product ::
    Expression parameterization ->
    Expression parameterization ->
    Expression parameterization
  Quotient ::
    Expression parameterization ->
    Expression parameterization ->
    Expression parameterization
  SquareRoot ::
    Expression parameterization ->
    Expression parameterization
  Sine ::
    Expression parameterization ->
    Expression parameterization
  Cosine ::
    Expression parameterization ->
    Expression parameterization

deriving instance Eq (Expression parameterization)

deriving instance Ord (Expression parameterization)

constant :: Qty units -> Expression parameterization
constant = Constant . Units.coerce

parameter :: Expression Curve
parameter = Parameter

u :: Expression Surface
u = U

v :: Expression Surface
v = V

instance HasUnits (Expression parameterization) where
  type UnitsOf (Expression parameterization) = Unitless

instance Units.Coercion (Expression parameterization) (Expression parameterization) where
  coerce = identity

instance Negation (Expression parameterization) where
  negate (Constant value) = Constant (negate value)
  negate (Negated expression) = expression
  negate (Difference lhs rhs) = Difference rhs lhs
  negate expression = Negated expression

instance Multiplication' Sign (Expression parameterization) where
  type Sign .*. Expression parameterization = Expression parameterization
  Positive .*. expression = expression
  Negative .*. expression = -expression

instance Multiplication' (Expression parameterization) Sign where
  type Expression parameterization .*. Sign = Expression parameterization
  expression .*. Positive = expression
  expression .*. Negative = -expression

instance Multiplication Sign (Expression parameterization) (Expression parameterization)

instance Multiplication (Expression parameterization) Sign (Expression parameterization)

instance
  parameterization1 ~ parameterization2 =>
  Addition
    (Expression parameterization1)
    (Expression parameterization2)
    (Expression parameterization1)
  where
  Constant lhs + Constant rhs = Constant (lhs + rhs)
  lhs + Constant 0.0 = lhs
  Constant 0.0 + rhs = rhs
  lhs + rhs = if lhs <= rhs then Sum lhs rhs else Sum rhs lhs

instance Addition (Expression parameterization) (Qty units) (Expression parameterization) where
  expression + value = expression + constant value

instance Addition (Qty units) (Expression parameterization) (Expression parameterization) where
  value + expression = constant value + expression

instance
  parameterization1 ~ parameterization2 =>
  Subtraction
    (Expression parameterization1)
    (Expression parameterization2)
    (Expression parameterization1)
  where
  Constant lhs - Constant rhs = Constant (lhs - rhs)
  lhs - Constant 0.0 = lhs
  Constant 0.0 - rhs = negate rhs
  lhs - rhs = Difference lhs rhs

instance Subtraction (Expression parameterization) (Qty units) (Expression parameterization) where
  expression - value = expression - constant value

instance Subtraction (Qty units) (Expression parameterization) (Expression parameterization) where
  value - expression = constant value - expression

instance
  parameterization1 ~ parameterization2 =>
  Multiplication' (Expression parameterization1) (Expression parameterization2)
  where
  type Expression parameterization1 .*. Expression parameterization2 = Expression parameterization1
  Constant lhs .*. Constant rhs = Constant (lhs * rhs)
  _ .*. Constant 0.0 = Constant 0.0
  Constant 0.0 .*. _ = Constant 0.0
  lhs .*. Constant 1.0 = lhs
  Constant 1.0 .*. rhs = rhs
  lhs .*. rhs = if lhs <= rhs then Product lhs rhs else Product rhs lhs

instance
  parameterization1 ~ parameterization2 =>
  Multiplication
    (Expression parameterization1)
    (Expression parameterization2)
    (Expression parameterization1)

instance Multiplication' (Qty units) (Expression parameterization) where
  type Qty units .*. Expression parameterization = Expression parameterization
  value .*. expression = constant value .*. expression

instance Multiplication (Qty units) (Expression parameterization) (Expression parameterization)

instance Multiplication' (Expression parameterization) (Qty units) where
  type Expression parameterization .*. Qty units = Expression parameterization
  expression .*. value = expression .*. constant value

instance Multiplication (Expression parameterization) (Qty units) (Expression parameterization)

instance
  parameterization1 ~ parameterization2 =>
  Division' (Expression parameterization1) (Expression parameterization2)
  where
  type Expression parameterization1 ./. Expression parameterization2 = Expression parameterization1
  Constant lhs ./. Constant rhs = Constant (lhs / rhs)
  Constant 0.0 ./. _ = Constant 0.0
  lhs ./. Constant 1.0 = lhs
  lhs ./. Constant value = lhs * (1.0 / value)
  lhs ./. rhs = Quotient lhs rhs

instance
  parameterization1 ~ parameterization2 =>
  Division
    (Expression parameterization1)
    (Expression parameterization2)
    (Expression parameterization1)

instance Division' (Expression parameterization) (Qty units) where
  type Expression parameterization ./. Qty units = Expression parameterization
  expression ./. value = expression ./. constant value

instance Division (Expression parameterization) (Qty units) (Expression parameterization)

instance Division' (Qty units) (Expression parameterization) where
  type Qty units ./. Expression parameterization = Expression parameterization
  value ./. expression = constant value ./. expression

instance Division (Qty units) (Expression parameterization) (Expression parameterization)

instance
  Composition
    (Expression parameterization)
    (Expression Curve)
    (Expression parameterization)
  where
  Parameter . expression = expression
  Constant value . _ = Constant value
  Negated arg . inner = negate (arg . inner)
  Sum lhs rhs . inner = (lhs . inner) + (rhs . inner)
  Difference lhs rhs . inner = (lhs . inner) - (rhs . inner)
  Product lhs rhs . inner = (lhs . inner) * (rhs . inner)
  Quotient lhs rhs . inner = (lhs . inner) / (rhs . inner)
  SquareRoot arg . inner = sqrt (arg . inner)
  Sine arg . inner = sin (arg . inner)
  Cosine arg . inner = cos (arg . inner)

instance
  Composition
    (Expression2d parameterization)
    (Expression Surface)
    (Expression parameterization)
  where
  U . Expression2d inner _ = inner
  V . Expression2d _ inner = inner
  Constant value . _ = Constant value
  Negated arg . inner = negate (arg . inner)
  Sum lhs rhs . inner = (lhs . inner) + (rhs . inner)
  Difference lhs rhs . inner = (lhs . inner) - (rhs . inner)
  Product lhs rhs . inner = (lhs . inner) * (rhs . inner)
  Quotient lhs rhs . inner = (lhs . inner) / (rhs . inner)
  SquareRoot arg . inner = sqrt (arg . inner)
  Sine arg . inner = sin (arg . inner)
  Cosine arg . inner = cos (arg . inner)

squared :: Expression parameterization -> Expression parameterization
squared (Constant expression) = Constant (Float.squared expression)
squared (Negated expression) = squared expression
squared (SquareRoot expression) = expression
squared expression = expression * expression

sqrt :: Expression parameterization -> Expression parameterization
sqrt (Constant value) = Constant (Float.sqrt value)
sqrt expression = SquareRoot expression

sin :: Expression parameterization -> Expression parameterization
sin (Constant value) = Constant (Float.sin value)
sin expression = Sine expression

cos :: Expression parameterization -> Expression parameterization
cos (Constant value) = Constant (Float.cos value)
cos (Negated expression) = cos expression
cos expression = Cosine expression

interpolateFrom ::
  Expression parameterization ->
  Expression parameterization ->
  Expression parameterization ->
  Expression parameterization
interpolateFrom start end t = start + t * (end - start)

data Expression#

type Ptr = Foreign.Ptr Expression#

foreign import ccall unsafe "opensolid_jit_constant"
  opensolid_jit_constant :: Double -> Ptr

foreign import ccall unsafe "opensolid_jit_argument"
  opensolid_jit_argument :: Int64 -> Ptr

foreign import ccall unsafe "opensolid_jit_negate"
  opensolid_jit_negate :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_sum"
  opensolid_jit_sum :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_difference"
  opensolid_jit_difference :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_product"
  opensolid_jit_product :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_quotient"
  opensolid_jit_quotient :: Ptr -> Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_sqrt"
  opensolid_jit_sqrt :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_sin"
  opensolid_jit_sin :: Ptr -> Ptr

foreign import ccall unsafe "opensolid_jit_cos"
  opensolid_jit_cos :: Ptr -> Ptr

toPtr :: Expression parameterization -> Ptr
toPtr expression = case expression of
  Parameter -> opensolid_jit_argument (fromIntegral 0)
  U -> opensolid_jit_argument (fromIntegral 0)
  V -> opensolid_jit_argument (fromIntegral 1)
  Constant value -> opensolid_jit_constant (Float.toDouble value)
  Negated arg -> opensolid_jit_negate (toPtr arg)
  Sum lhs rhs -> opensolid_jit_sum (toPtr lhs) (toPtr rhs)
  Difference lhs rhs -> opensolid_jit_difference (toPtr lhs) (toPtr rhs)
  Product lhs rhs -> opensolid_jit_product (toPtr lhs) (toPtr rhs)
  Quotient lhs rhs -> opensolid_jit_quotient (toPtr lhs) (toPtr rhs)
  SquareRoot arg -> opensolid_jit_sqrt (toPtr arg)
  Sine arg -> opensolid_jit_sin (toPtr arg)
  Cosine arg -> opensolid_jit_cos (toPtr arg)

foreign import ccall unsafe "opensolid_jit_compile_curve1d"
  opensolid_jit_compile_curve1d :: Ptr -> FunPtr (Double# -> Double#)

foreign import ccall unsafe "dynamic"
  curve1d_function :: FunPtr (Double# -> Double#) -> (Double# -> Double#)

curve :: Expression Curve -> (Float -> Qty units)
curve expression = do
  -- TODO perform garbage collection on JIT-compiled functions:
  -- use GHC.Weak.mkWeak on f# to associate a finalizer with it
  -- that calls a Rust function to delete the underlying JIT-compiled function/module
  let f# = curve1d_function (opensolid_jit_compile_curve1d (toPtr expression))
  \(Qty# x#) -> Qty# (f# x#)

foreign import ccall unsafe "opensolid_jit_compile_surface1d"
  opensolid_jit_compile_surface1d :: Ptr -> FunPtr (Double# -> Double# -> Double#)

foreign import ccall unsafe "dynamic"
  surface1d_function :: FunPtr (Double# -> Double# -> Double#) -> (Double# -> Double# -> Double#)

surface :: Expression Surface -> (Uv.Point -> Qty units)
surface expression = do
  let f# = surface1d_function (opensolid_jit_compile_surface1d (toPtr expression))
  \(Point2d# u# v#) -> Qty# (f# u# v#)
