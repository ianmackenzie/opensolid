module OpenSolid.Units
  ( HasUnits
  , Coercion (coerce)
  , erase
  , (:*:)
  , (:/:)
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
  , MetersToTheFourthPower
  )
where

import Data.Coerce qualified
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty)
import OpenSolid.Bootstrap
import {-# SOURCE #-} OpenSolid.Qty (Qty)
import {-# SOURCE #-} OpenSolid.Result (Result (Failure, Success))
import {-# SOURCE #-} OpenSolid.Sign (Sign)
import OpenSolid.Unitless (Unitless)

class HasUnits erased Unitless erased => HasUnits (a :: k) units erased | a -> units, a -> erased

instance HasUnits Int Unitless Int

instance HasUnits (Qty units) units (Qty Unitless)

instance HasUnits Sign Unitless Sign

instance HasUnits a units erased => HasUnits (Maybe a) units (Maybe erased)

instance HasUnits a units erased => HasUnits (Result x a) units (Result x erased)

instance HasUnits a units erased => HasUnits (List a) units (List erased)

instance HasUnits a units erased => HasUnits (NonEmpty a) units (NonEmpty erased)

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

{-# INLINE erase #-}
erase :: (Coercion a erased, HasUnits a units erased) => a -> erased
erase = coerce

data a :*: b

data a :/: b

{-# INLINE specialize #-}
specialize ::
  ( Coercion a b
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , Specialize unitsA unitsB
  ) =>
  a ->
  b
specialize = coerce

{-# INLINE unspecialize #-}
unspecialize ::
  ( Coercion a b
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , Specialize unitsA unitsB
  ) =>
  b ->
  a
unspecialize = coerce

commute ::
  ( Coercion a b
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , unitsA ~ units1 :*: units2
  , unitsB ~ units2 :*: units1
  ) =>
  a ->
  b
commute = coerce

leftAssociate ::
  ( Coercion a b
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , unitsA ~ units1 :*: (units2 :*: units3)
  , unitsB ~ (units1 :*: units2) :*: units3
  ) =>
  a ->
  b
leftAssociate = coerce

rightAssociate ::
  ( Coercion a b
  , HasUnits a unitsA erasedA
  , HasUnits b unitsB erasedB
  , unitsA ~ (units1 :*: units2) :*: units3
  , unitsB ~ units1 :*: (units2 :*: units3)
  ) =>
  a ->
  b
rightAssociate = coerce

data Radians deriving (Eq, Show)

data Meters deriving (Eq, Show)

data Seconds deriving (Eq, Show)

data MetersPerSecond deriving (Eq, Show)

data MetersPerSecondSquared deriving (Eq, Show)

data SquareMeters deriving (Eq, Show)

data CubicMeters deriving (Eq, Show)

data MetersToTheFourthPower deriving (Eq, Show)

type family a .*. b where
  Unitless .*. Unitless = Unitless
  Unitless .*. units = units
  units .*. Unitless = units
  Meters .*. Meters = SquareMeters
  Meters .*. SquareMeters = CubicMeters
  SquareMeters .*. Meters = CubicMeters
  Meters .*. CubicMeters = MetersToTheFourthPower
  CubicMeters .*. Meters = MetersToTheFourthPower
  SquareMeters .*. SquareMeters = MetersToTheFourthPower
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
  MetersToTheFourthPower ./. Meters = CubicMeters
  MetersToTheFourthPower ./. SquareMeters = SquareMeters
  MetersToTheFourthPower ./. CubicMeters = Meters
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
