module Units
  ( UnitsOf
  , Unitless
  , type (:*:)
  , type (:/:)
  , Product
  , Quotient
  , Squared
  )
where

import Basics

type UnitsOf :: k -> Type
type family UnitsOf a

data Unitless

type role (:*:) phantom phantom

data units1 :*: units2

type role (:/:) phantom phantom

data units1 :/: units2

infixl 7 :*:, :/:

type (.*.) :: Type -> Type -> Type
type family a .*. b

type (./.) :: Type -> Type -> Type
type family a ./. b

type Sqr :: Type -> Type
type family Sqr units = squaredUnits | squaredUnits -> units

type Product units1 units2 units3 =
  ( units1 .*. units2 ~ units3
  , units2 .*. units1 ~ units3
  , units3 ./. units1 ~ units2
  , units3 ./. units2 ~ units1
  )

type Quotient units1 units2 units3 =
  ( units1 ./. units2 ~ units3
  , units2 .*. units3 ~ units1
  , units3 .*. units2 ~ units1
  , units1 ./. units3 ~ units2
  )

type Squared units1 units2 =
  ( Sqr units1 ~ units2
  , Product units1 units1 units2
  )
