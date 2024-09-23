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
  , Inverse
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

class HasUnits (a :: k) where
  type UnitsOf a

instance HasUnits Int where
  type UnitsOf Int = Unitless

instance HasUnits (Qty units) where
  type UnitsOf (Qty units) = units

instance HasUnits Sign where
  type UnitsOf Sign = Unitless

instance HasUnits a => HasUnits (Maybe a) where
  type UnitsOf (Maybe a) = UnitsOf a

instance HasUnits a => HasUnits (Result x a) where
  type UnitsOf (Result x a) = UnitsOf a

instance HasUnits a => HasUnits (List a) where
  type UnitsOf (List a) = UnitsOf a

instance HasUnits a => HasUnits (NonEmpty a) where
  type UnitsOf (NonEmpty a) = UnitsOf a

type Coercion :: Type -> Type -> Constraint
class Coercion b a => Coercion a b where
  coerce :: a -> b

instance Coercion Int Int where
  {-# INLINE coerce #-}
  coerce = identity

instance Coercion (Qty units1) (Qty units2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

instance Coercion Sign Sign where
  {-# INLINE coerce #-}
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

data units1 :*: units2 deriving (Eq, Show)

infixl 7 :*:

type role (:/:) phantom phantom

data units1 :/: units2 deriving (Eq, Show)

infixl 7 :/:

{-# INLINE erase #-}
erase :: (Coercion a b, UnitsOf b ~ Unitless) => a -> b
erase = coerce

{-# INLINE specialize #-}
specialize :: (Coercion a b, Specialize (UnitsOf a) (UnitsOf b)) => a -> b
specialize = coerce

{-# INLINE unspecialize #-}
unspecialize :: (Coercion a b, Specialize (UnitsOf a) (UnitsOf b)) => b -> a
unspecialize = coerce

commute ::
  ( Coercion a b
  , UnitsOf a ~ units1 :*: units2
  , UnitsOf b ~ units2 :*: units1
  ) =>
  a ->
  b
commute = coerce

leftAssociate ::
  ( Coercion a b
  , UnitsOf a ~ units1 :*: (units2 :*: units3)
  , UnitsOf b ~ (units1 :*: units2) :*: units3
  ) =>
  a ->
  b
leftAssociate = coerce

rightAssociate ::
  ( Coercion a b
  , UnitsOf a ~ (units1 :*: units2) :*: units3
  , UnitsOf b ~ units1 :*: (units2 :*: units3)
  ) =>
  a ->
  b
rightAssociate = coerce

data Unitless deriving (Eq, Show)

data Radians deriving (Eq, Show)

data Meters deriving (Eq, Show)

data Seconds deriving (Eq, Show)

data MetersPerSecond deriving (Eq, Show)

data MetersPerSecondSquared deriving (Eq, Show)

data SquareMeters deriving (Eq, Show)

data CubicMeters deriving (Eq, Show)

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

type Inverse units1 units2 =
  ( Quotient Unitless units1 units2
  , Quotient Unitless units2 units1
  )

class Specialize genericUnits specificUnits | genericUnits -> specificUnits

instance Product units1 units2 units3 => Specialize (units1 :*: units2) units3

instance Quotient units1 units2 units3 => Specialize (units1 :/: units2) units3

instance Specialize Unitless Unitless
