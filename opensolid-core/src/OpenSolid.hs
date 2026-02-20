module OpenSolid
  ( number
  , Negation
  , negate
  , Addition
  , sum
  , Subtraction
  , difference
  , subtract
  , Multiplication
  , product
  , Multiplication_
  , product_
  , twice
  , half
  , Division
  , quotient
  , Division_
  , quotient_
  , DotMultiplication
  , dotProduct
  , DotMultiplication_
  , dotProduct_
  , CrossMultiplication
  , crossProduct
  , CrossMultiplication_
  , crossProduct_
  , Composition
  , compose
  , Intersects
  , intersects
  , ApproximateEquality
  , approximatelyEquals
  , Argument
  , argument
  , Quantity
  , Number
  , Sign (Sign, Positive, Negative)
  , Tolerance
  , Result (Ok, Error)
  )
where

import OpenSolid.Prelude

number :: Number -> Number
number = id

sum :: Addition a b c => a -> b -> c
sum = (+)

difference :: Subtraction a b c => a -> b -> c
difference = (-)

product :: Multiplication a b c => a -> b -> c
product = (*)

product_ :: Multiplication_ a b c => a -> b -> c
product_ = (?*?)

twice :: Multiplication Number a a => a -> a
twice = (2.0 *)

half :: Multiplication Number a a => a -> a
half = (0.5 *)

quotient :: Division a b c => a -> b -> c
quotient = (/)

quotient_ :: Division_ a b c => a -> b -> c
quotient_ = (?/?)

dotProduct :: DotMultiplication a b c => a -> b -> c
dotProduct = dot

dotProduct_ :: DotMultiplication_ a b c => a -> b -> c
dotProduct_ = dot_

crossProduct :: CrossMultiplication a b c => a -> b -> c
crossProduct = cross

crossProduct_ :: CrossMultiplication_ a b c => a -> b -> c
crossProduct_ = cross_

compose :: Composition f g h => f -> g -> h
compose = (.)

approximatelyEquals :: (ApproximateEquality a constraint, constraint) => a -> a -> Bool
approximatelyEquals = (~=)

type Argument name value = name ::: value

argument :: forall name -> value -> Argument name value
argument name value = name ::: value
