module Units
  ( Conversion
  , conversion
  , convert
  , unconvert
  , Coercion
  , Generic
  , GenericProduct
  , GenericQuotient
  , (:/)
  , (:*)
  , drop
  , add
  , generalize
  , specialize
  , (.*)
  , (./)
  , (.<>)
  , (.><)
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

import Arithmetic
import Basics
import Data.Kind (Constraint)
import Data.List.NonEmpty (NonEmpty)
import {-# SOURCE #-} Qty (Qty (Qty))
import Unsafe.Coerce (unsafeCoerce)
import Prelude qualified

newtype Conversion units1 units2 = Conversion Prelude.Double

conversion :: Qty units1 -> Qty units2 -> Conversion units1 units2
conversion (Qty a) (Qty b) = Conversion (b Prelude./ a)

convert :: Conversion units1 units2 -> Qty units1 -> Qty units2
convert (Conversion factor) (Qty value) = Qty (value Prelude.* factor)

unconvert :: Conversion units1 units2 -> Qty units2 -> Qty units1
unconvert (Conversion factor) (Qty value) = Qty (value Prelude./ factor)

type Coercion :: Type -> Type -> Type -> Type -> Constraint
class Coercion u1 u2 a b | a -> u1, b -> u2, a u2 -> b

instance Coercion u1 u2 a b => Coercion u1 u2 (List a) (List b)

instance Coercion u1 u2 a b => Coercion u1 u2 (NonEmpty a) (NonEmpty b)

data Generic units

data GenericProduct units1 units2

data GenericQuotient units1 units2

{-# INLINE drop #-}
drop :: Coercion units Unitless a b => a -> b
drop = unsafeCoerce

{-# INLINE add #-}
add :: Coercion Unitless units a b => a -> b
add = unsafeCoerce

{-# INLINE generalize #-}
generalize :: Coercion units (Generic units) a b => a -> b
generalize = unsafeCoerce

{-# INLINE specialize #-}
specialize :: (Coercion genericUnits specificUnits a b, Specialize genericUnits specificUnits) => a -> b
specialize = unsafeCoerce

(.*) ::
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless unitsC cUnitless c
  , unitsA :* unitsB ~ unitsC
  , Multiplication aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.*) lhs rhs = add (drop lhs * drop rhs)

(./) ::
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless unitsC cUnitless c
  , unitsA :/ unitsB ~ unitsC
  , Division aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(./) lhs rhs = add (drop lhs / drop rhs)

(.<>) ::
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless unitsC cUnitless c
  , unitsA :* unitsB ~ unitsC
  , DotProduct aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.<>) lhs rhs = add (drop lhs <> drop rhs)

(.><) ::
  ( Coercion unitsA Unitless a aUnitless
  , Coercion unitsB Unitless b bUnitless
  , Coercion Unitless unitsC cUnitless c
  , unitsA :* unitsB ~ unitsC
  , CrossProduct aUnitless bUnitless cUnitless
  ) =>
  a ->
  b ->
  c
(.><) lhs rhs = add (drop lhs >< drop rhs)

infixl 7 .*, ./, .<>, .><

class
  ( Product units2 units1 units3
  , Quotient units3 units1 units2
  , Quotient units3 units2 units1
  ) =>
  Product units1 units2 units3
    | units1 units2 -> units3

class
  ( Product units3 units2 units1
  , Product units2 units3 units1
  , Quotient units1 units3 units2
  ) =>
  Quotient units1 units2 units3
    | units1 units2 -> units3

class
  Product units units squaredUnits =>
  Squared units squaredUnits
    | units -> squaredUnits
    , squaredUnits -> units

data Unitless

data Radians

data Meters

data Seconds

data MetersPerSecond

data MetersPerSecondSquared

data SquareMeters

data CubicMeters

instance {-# INCOHERENT #-} Product Unitless Unitless Unitless

instance {-# INCOHERENT #-} Product Unitless units units

instance {-# INCOHERENT #-} Product units Unitless units

instance {-# INCOHERENT #-} Quotient Unitless Unitless Unitless

instance {-# INCOHERENT #-} Quotient units Unitless units

instance {-# INCOHERENT #-} Quotient units units Unitless

instance Product Meters Meters SquareMeters

instance Product Meters SquareMeters CubicMeters

instance Product SquareMeters Meters CubicMeters

instance Product Seconds MetersPerSecond Meters

instance Product MetersPerSecond Seconds Meters

instance Product Seconds MetersPerSecondSquared MetersPerSecond

instance Product MetersPerSecondSquared Seconds MetersPerSecond

instance Quotient SquareMeters Meters Meters

instance Quotient CubicMeters Meters SquareMeters

instance Quotient CubicMeters SquareMeters Meters

instance Quotient Meters Seconds MetersPerSecond

instance Quotient Meters MetersPerSecond Seconds

instance Quotient MetersPerSecond Seconds MetersPerSecondSquared

instance Quotient MetersPerSecond MetersPerSecondSquared Seconds

instance Squared Unitless Unitless

instance Squared Meters SquareMeters

instance units ~ units' => Product (Generic units) (Generic units') (GenericProduct units units)

instance (units ~ units', units ~ units'') => Quotient (GenericProduct units units') (Generic units'') (Generic units)

type family units1 :* units2 where
  Generic units1 :* Generic units2 = GenericProduct units1 units2
  GenericQuotient Unitless units2 :* Generic units1 = GenericQuotient units1 units2
  Generic units1 :* GenericQuotient Unitless units2 = GenericQuotient units1 units2
  GenericQuotient units1 units2 :* Generic units2 = Generic units1
  GenericQuotient units1 units2 :* GenericQuotient units2 units1 = Unitless
  GenericProduct units1 units2 :* Generic units3 = GenericProduct (GenericProduct units1 units2) units3
  Generic units1 :* (GenericProduct units2 units3) = GenericProduct units1 (GenericProduct units2 units3)

type family units1 :/ units2 where
  Generic units1 :/ Generic units2 = GenericQuotient units1 units2
  GenericProduct units units :/ Generic units = Generic units
  GenericProduct units1 units2 :/ Generic units1 = Generic units2
  GenericProduct units1 units2 :/ Generic units2 = Generic units1
  Unitless :/ GenericQuotient Unitless units2 = Generic units2
  Generic units1 :/ GenericQuotient Unitless units2 = GenericProduct units1 units2
  Generic units1 :/ GenericQuotient units1 units2 = Generic units2
  GenericProduct units1 units2 :/ GenericProduct units2 units2 = GenericQuotient units1 units2
  GenericProduct units2 units1 :/ GenericProduct units2 units2 = GenericQuotient units1 units2
  GenericProduct units1 units1 :/ GenericProduct units1 units2 = GenericQuotient units1 units2
  GenericProduct units1 units1 :/ GenericProduct units2 units1 = GenericQuotient units1 units2
  GenericProduct units1 units3 :/ GenericProduct units2 units3 = GenericQuotient units1 units2
  GenericProduct units3 units1 :/ GenericProduct units3 units2 = GenericQuotient units1 units2
  GenericProduct units3 units1 :/ GenericProduct units2 units3 = GenericQuotient units1 units2
  GenericProduct units1 units3 :/ GenericProduct units3 units2 = GenericQuotient units1 units2

instance Squared (Generic units) (GenericProduct units units)

class Specialize genericUnits specificUnits | genericUnits -> specificUnits

instance Specialize (Generic units) units

instance Product units1 units2 units3 => Specialize (GenericProduct units1 units2) units3

instance Quotient units1 units2 units3 => Specialize (GenericQuotient units1 units2) units3
