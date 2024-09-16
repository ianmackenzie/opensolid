module Units
  ( Units (..)
  , UnitsOf
  , Product
  , Quotient
  , Squared
  )
where

import Basics
import Data.Kind (Constraint)
import GHC.TypeLits (Symbol)

data Units
  = Unitless
  | Units Symbol
  | Units :*: Units
  | Units :/: Units

infixl 7 :*:, :/:

type UnitsOf :: k -> Units
type family UnitsOf a

type (.*.) :: Units -> Units -> Units
type family a .*. b

type (./.) :: Units -> Units -> Units
type family a ./. b

type Sqr :: Units -> Units
type family Sqr units = squaredUnits | squaredUnits -> units

type Product :: Units -> Units -> Units -> Constraint
type Product units1 units2 units3 =
  ( units1 .*. units2 ~ units3
  , units2 .*. units1 ~ units3
  , units3 ./. units1 ~ units2
  , units3 ./. units2 ~ units1
  )

type Quotient :: Units -> Units -> Units -> Constraint
type Quotient units1 units2 units3 =
  ( units1 ./. units2 ~ units3
  , units2 .*. units3 ~ units1
  , units3 .*. units2 ~ units1
  , units1 ./. units3 ~ units2
  )

type Squared :: Units -> Units -> Constraint
type Squared units1 units2 =
  ( Sqr units1 ~ units2
  , Product units1 units1 units2
  )
