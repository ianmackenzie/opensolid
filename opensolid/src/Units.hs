module Units
  ( Units
  , Conversion
  , conversion
  , convert
  , unconvert
  , Coercion (coerce)
  , type (:*:)
  , type (:/:)
  , Specialize
  , specialize
  , unspecialize
  , commute
  , leftAssociate
  , rightAssociate
  , Product
  , Squared
  , Quotient
  , Unitless
  , Radians
  , Meters
  , Seconds
  , MetersPerSecond
  , MetersPerSecondSquared
  , SquareMeters
  , CubicMeters
  )
where

import Basics
import Data.Coerce qualified
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty)
import {-# SOURCE #-} Qty (Qty (Qty_))
import {-# SOURCE #-} Result (Result (Error, Ok))
import {-# SOURCE #-} Sign (Sign)
import Prelude qualified

type Units :: k -> Type
type family Units a

type instance Units Int = Unitless

type instance Units (Qty units) = units

type instance Units Sign = Unitless

type instance Units (Maybe a) = Units a

type instance Units (Result x a) = Units a

type instance Units (List a) = Units a

type instance Units (NonEmpty a) = Units a

newtype Conversion units1 units2 = Conversion Prelude.Double

conversion :: Qty units1 -> Qty units2 -> Conversion units1 units2
conversion (Qty_ a) (Qty_ b) = Conversion (b Prelude./ a)

convert :: Conversion units1 units2 -> Qty units1 -> Qty units2
convert (Conversion factor) (Qty_ value) = Qty_ (value Prelude.* factor)

unconvert :: Conversion units1 units2 -> Qty units2 -> Qty units1
unconvert (Conversion factor) (Qty_ value) = Qty_ (value Prelude./ factor)

type Coercion :: Type -> Type -> Constraint
class Coercion b a => Coercion a b where
  coerce :: a -> b

instance Coercion Int Int where
  coerce = identity

instance Coercion (Qty units1) (Qty units2) where
  coerce = Data.Coerce.coerce

instance Coercion Sign Sign where
  coerce = identity

instance Coercion a b => Coercion (Maybe a) (Maybe b) where
  coerce (Just value) = Just (coerce value)
  coerce Nothing = Nothing

instance Coercion a b => Coercion (Result x a) (Result x b) where
  coerce (Ok value) = Ok (coerce value)
  coerce (Error error) = Error error

instance (Coercion a b, Data.Coerce.Coercible a b) => Coercion (List a) (List b) where
  coerce = Data.Coerce.coerce

instance (Coercion a b, Data.Coerce.Coercible a b) => Coercion (NonEmpty a) (NonEmpty b) where
  coerce = Data.Coerce.coerce

type role (:*:) phantom phantom

data units1 :*: units2

type role (:/:) phantom phantom

data units1 :/: units2

infixl 7 :*:, :/:

{-# INLINE specialize #-}
specialize :: (Coercion a b, Specialize (Units a) (Units b)) => a -> b
specialize = coerce

{-# INLINE unspecialize #-}
unspecialize :: (Coercion a b, Specialize (Units a) (Units b)) => b -> a
unspecialize = coerce

commute ::
  ( Coercion a b
  , Units a ~ units1 :*: units2
  , Units b ~ units2 :*: units1
  ) =>
  a ->
  b
commute = coerce

leftAssociate ::
  ( Coercion a b
  , Units a ~ units1 :*: (units2 :*: units3)
  , Units b ~ (units1 :*: units2) :*: units3
  ) =>
  a ->
  b
leftAssociate = coerce

rightAssociate ::
  ( Coercion a b
  , Units a ~ (units1 :*: units2) :*: units3
  , Units b ~ units1 :*: (units2 :*: units3)
  ) =>
  a ->
  b
rightAssociate = coerce

data Unitless

data Radians

data Meters

data Seconds

data MetersPerSecond

data MetersPerSecondSquared

data SquareMeters

data CubicMeters

type family a .*. b where
  Unitless .*. Unitless = Unitless
  Unitless .*. units = units
  units .*. Unitless = units
  Meters .*. Meters = SquareMeters
  Meters .*. SquareMeters = CubicMeters
  SquareMeters .*. Meters = CubicMeters
  Seconds .*. MetersPerSecond = Meters
  MetersPerSecond .*. Seconds = Meters
  Seconds .*. MetersPerSecondSquared = MetersPerSecond
  MetersPerSecondSquared .*. Seconds = MetersPerSecond

infixl 7 .*.

type family a ./. b where
  Unitless ./. Unitless = Unitless
  units ./. Unitless = units
  units ./. units = Unitless
  SquareMeters ./. Meters = Meters
  CubicMeters ./. Meters = SquareMeters
  CubicMeters ./. SquareMeters = Meters
  Meters ./. Seconds = MetersPerSecond
  Meters ./. MetersPerSecond = Seconds
  MetersPerSecond ./. Seconds = MetersPerSecondSquared
  MetersPerSecond ./. MetersPerSecondSquared = Seconds

infixl 7 ./.

type family Sqr units = squaredUnits | squaredUnits -> units where
  Sqr Unitless = Unitless
  Sqr Meters = SquareMeters

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

class Specialize genericUnits specificUnits | genericUnits -> specificUnits

instance Product units1 units2 units3 => Specialize (units1 :*: units2) units3

instance Quotient units1 units2 units3 => Specialize (units1 :/: units2) units3

instance Specialize Unitless Unitless
