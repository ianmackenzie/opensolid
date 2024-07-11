module Units
  ( HasUnits (..)
  , Coercion (coerce)
  , erase
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
import {-# SOURCE #-} Qty (Qty)
import {-# SOURCE #-} Result (Result (Failure, Success))
import {-# SOURCE #-} Sign (Sign)

class Units (Erase a) ~ Unitless => HasUnits (a :: k) where
  type Units a
  type Erase a :: k

instance HasUnits Int where
  type Units Int = Unitless
  type Erase Int = Int

instance HasUnits (Qty units) where
  type Units (Qty units) = units
  type Erase (Qty units) = Qty Unitless

instance HasUnits Sign where
  type Units Sign = Unitless
  type Erase Sign = Sign

instance HasUnits a => HasUnits (Maybe a) where
  type Units (Maybe a) = Units a
  type Erase (Maybe a) = Maybe (Erase a)

instance HasUnits a => HasUnits (Result x a) where
  type Units (Result x a) = Units a
  type Erase (Result x a) = Result x (Erase a)

instance HasUnits a => HasUnits (List a) where
  type Units (List a) = Units a
  type Erase (List a) = List (Erase a)

instance HasUnits a => HasUnits (NonEmpty a) where
  type Units (NonEmpty a) = Units a
  type Erase (NonEmpty a) = NonEmpty (Erase a)

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
  coerce (Success value) = Success (coerce value)
  coerce (Failure error) = Failure error

instance (Coercion a b, Data.Coerce.Coercible a b) => Coercion (List a) (List b) where
  coerce = Data.Coerce.coerce

instance (Coercion a b, Data.Coerce.Coercible a b) => Coercion (NonEmpty a) (NonEmpty b) where
  coerce = Data.Coerce.coerce

type role (:*:) phantom phantom

data units1 :*: units2

type role (:/:) phantom phantom

data units1 :/: units2

infixl 7 :*:, :/:

{-# INLINE erase #-}
erase :: Coercion a (Erase a) => a -> Erase a
erase = coerce

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
