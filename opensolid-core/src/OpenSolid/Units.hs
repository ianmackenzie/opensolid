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
  , simplify
  , Simplification
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
import {-# SOURCE #-} OpenSolid.Quantity (Quantity)
import {-# SOURCE #-} OpenSolid.Result (Result (Failure, Success))
import {-# SOURCE #-} OpenSolid.Sign (Sign)
import OpenSolid.Unitless (Unitless)

class HasUnits (a :: k) units | a -> units

instance HasUnits Int Unitless

instance HasUnits (Quantity units) units

instance HasUnits Sign Unitless

instance HasUnits a units => HasUnits (Maybe a) units

instance HasUnits a units => HasUnits (Result x a) units

instance HasUnits a units => HasUnits (List a) units

instance HasUnits a units => HasUnits (NonEmpty a) units

type Coercion :: Type -> Type -> Constraint
class Coercion b a => Coercion a b where
  coerce :: a -> b

instance Coercion Int Int where
  {-# INLINE coerce #-}
  coerce = id

instance Coercion (Quantity units1) (Quantity units2) where
  {-# INLINE coerce #-}
  coerce = Data.Coerce.coerce

instance Coercion Sign Sign where
  {-# INLINE coerce #-}
  coerce = id

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
erase :: forall b a units. (Coercion a b, HasUnits a units, HasUnits b Unitless) => a -> b
erase = coerce

data a :*: b

data a :/: b

{-# INLINE specialize #-}
specialize ::
  ( Coercion a b
  , HasUnits a unitsA
  , HasUnits b unitsB
  , Specialize unitsA unitsB
  ) =>
  a ->
  b
specialize = coerce

{-# INLINE unspecialize #-}
unspecialize ::
  ( Coercion a b
  , HasUnits a unitsA
  , HasUnits b unitsB
  , Specialize unitsA unitsB
  ) =>
  b ->
  a
unspecialize = coerce

{-# INLINE commute #-}
commute ::
  ( Coercion a b
  , HasUnits a unitsA
  , HasUnits b unitsB
  , unitsA ~ units1 :*: units2
  , unitsB ~ units2 :*: units1
  ) =>
  a ->
  b
commute = coerce

{-# INLINE leftAssociate #-}
leftAssociate ::
  ( Coercion a b
  , HasUnits a unitsA
  , HasUnits b unitsB
  , unitsA ~ units1 :*: (units2 :*: units3)
  , unitsB ~ (units1 :*: units2) :*: units3
  ) =>
  a ->
  b
leftAssociate = coerce

{-# INLINE rightAssociate #-}
rightAssociate ::
  ( Coercion a b
  , HasUnits a unitsA
  , HasUnits b unitsB
  , unitsA ~ (units1 :*: units2) :*: units3
  , unitsB ~ units1 :*: (units2 :*: units3)
  ) =>
  a ->
  b
rightAssociate = coerce

{-# INLINE simplify #-}
simplify ::
  ( Coercion a b
  , HasUnits a unitsA
  , HasUnits b unitsB
  , Simplification unitsA unitsB
  ) =>
  a ->
  b
simplify = coerce

class Simplification units1 units2

instance
  Simplification
    (Unitless :*: units)
    units

instance
  Simplification
    (units :*: Unitless)
    units

instance
  Simplification
    (units :/: Unitless)
    units

instance
  Simplification
    ((units1 :/: units2) :*: units2)
    units1

instance
  Simplification
    (units2 :*: (units1 :/: units2))
    units1

instance
  Simplification
    ((units1 :*: units3) :/: (units2 :*: units3))
    (units1 :/: units2)

instance
  Simplification
    (units1 :*: (Unitless :/: units2))
    (units1 :/: units2)

instance
  Simplification
    ((Unitless :/: units2) :*: units1)
    (units1 :/: units2)

instance
  Simplification
    (Unitless :/: (units1 :/: units2))
    (units2 :/: units1)

instance
  Simplification
    ((units1 :*: units2) :/: units2)
    units1

instance
  Simplification
    (units1 :/: (units1 :/: units2))
    units2

instance
  Simplification
    (units :/: (units :*: units))
    (Unitless :/: units)

instance
  Simplification
    ((units :*: units) :/: (units :*: units :*: units))
    (Unitless :/: units)

instance
  Simplification
    (((units :*: units) :/: (units :*: units :*: units)) :*: units)
    Unitless

instance
  Simplification
    (((units :*: units) :/: (units :*: units :*: units)) :*: (units :*: units))
    units

data Radians deriving (Eq, Show)

data Meters deriving (Eq, Show)

data Seconds deriving (Eq, Show)

data MetersPerSecond deriving (Eq, Show)

data MetersPerSecondSquared deriving (Eq, Show)

data SquareMeters deriving (Eq, Show)

data CubicMeters deriving (Eq, Show)

data MetersToTheFourthPower deriving (Eq, Show)

data InverseMeters deriving (Eq, Show)

type family Prod a b where
  Prod Unitless Unitless = Unitless
  Prod Unitless units = units
  Prod units Unitless = units
  Prod Meters Meters = SquareMeters
  Prod Meters SquareMeters = CubicMeters
  Prod SquareMeters Meters = CubicMeters
  Prod Meters CubicMeters = MetersToTheFourthPower
  Prod CubicMeters Meters = MetersToTheFourthPower
  Prod SquareMeters SquareMeters = MetersToTheFourthPower
  Prod Seconds MetersPerSecond = Meters
  Prod MetersPerSecond Seconds = Meters
  Prod Seconds MetersPerSecondSquared = MetersPerSecond
  Prod MetersPerSecondSquared Seconds = MetersPerSecond
  Prod Meters InverseMeters = Unitless
  Prod InverseMeters Meters = Unitless
  Prod SquareMeters InverseMeters = Meters
  Prod InverseMeters SquareMeters = Meters
  Prod CubicMeters InverseMeters = SquareMeters
  Prod InverseMeters CubicMeters = SquareMeters
  Prod MetersToTheFourthPower InverseMeters = CubicMeters
  Prod InverseMeters MetersToTheFourthPower = CubicMeters

type family Quot a b where
  Quot Unitless Unitless = Unitless
  Quot units Unitless = units
  Quot units units = Unitless
  Quot SquareMeters Meters = Meters
  Quot CubicMeters Meters = SquareMeters
  Quot CubicMeters SquareMeters = Meters
  Quot MetersToTheFourthPower Meters = CubicMeters
  Quot MetersToTheFourthPower SquareMeters = SquareMeters
  Quot MetersToTheFourthPower CubicMeters = Meters
  Quot Meters Seconds = MetersPerSecond
  Quot Meters MetersPerSecond = Seconds
  Quot MetersPerSecond Seconds = MetersPerSecondSquared
  Quot MetersPerSecond MetersPerSecondSquared = Seconds
  Quot Unitless InverseMeters = Meters
  Quot Unitless Meters = InverseMeters
  Quot Meters InverseMeters = SquareMeters
  Quot SquareMeters InverseMeters = CubicMeters
  Quot CubicMeters InverseMeters = MetersToTheFourthPower

type family Square units = squaredUnits | squaredUnits -> units where
  Square Unitless = Unitless
  Square Meters = SquareMeters
  Square SquareMeters = MetersToTheFourthPower

type Product units1 units2 units3 =
  ( Prod units1 units2 ~ units3
  , Prod units2 units1 ~ units3
  , Quot units3 units1 ~ units2
  , Quot units3 units2 ~ units1
  )

type Quotient units1 units2 units3 =
  ( Quot units1 units2 ~ units3
  , Prod units2 units3 ~ units1
  , Prod units3 units2 ~ units1
  , Quot units1 units3 ~ units2
  )

type Squared units1 units2 =
  ( Square units1 ~ units2
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
