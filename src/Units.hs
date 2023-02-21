module Units
  ( Coercion
  , Generic
  , GenericProduct
  , GenericQuotient
  , drop
  , add
  , generalize
  , specialize
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

import Data.Kind (Type)
import Unsafe.Coerce (unsafeCoerce)

class Coercion (a :: Type -> Type)

data Generic units

data GenericProductOf units1 units2

data GenericQuotientOf units1 units2

{-# INLINE drop #-}
drop :: Coercion a => a units -> a Unitless
drop = unsafeCoerce

{-# INLINE add #-}
add :: Coercion a => a Unitless -> a units
add = unsafeCoerce

{-# INLINE generalize #-}
generalize :: Coercion a => a units -> a (Generic units)
generalize = unsafeCoerce

{-# INLINE specialize #-}
specialize :: (Coercion a, Specialize genericUnits units) => a genericUnits -> a units
specialize = unsafeCoerce

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

instance Product (Generic units) (Generic units) (GenericProductOf units units)

instance Quotient (GenericProductOf units units) (Generic units) (Generic units)

instance Quotient Unitless (Generic units) (GenericQuotientOf Unitless units)

instance Product (Generic units) (GenericQuotientOf Unitless units) Unitless

instance Product (GenericQuotientOf Unitless units) (Generic units) Unitless

instance Quotient Unitless (GenericQuotientOf Unitless units) (Generic units)

type family GenericProduct units1 units2 where
  GenericProduct (Generic units1) (Generic units2) = GenericProductOf units1 units2
  GenericProduct (GenericQuotientOf Unitless units2) (Generic units1) = GenericQuotientOf units1 units2
  GenericProduct (Generic units1) (GenericQuotientOf Unitless units2) = GenericQuotientOf units1 units2
  GenericProduct (GenericQuotientOf units1 units2) (Generic units2) = Generic units1
  GenericProduct (GenericQuotientOf units1 units2) (GenericQuotientOf units2 units1) = Unitless

type family GenericQuotient units1 units2 where
  GenericQuotient (Generic units1) (Generic units2) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units units) (Generic units) = Generic units
  GenericQuotient (GenericProductOf units1 units2) (Generic units1) = Generic units2
  GenericQuotient (GenericProductOf units1 units2) (Generic units2) = Generic units1
  GenericQuotient (Generic units1) (GenericQuotientOf units1 units2) = Generic units2
  GenericQuotient (GenericProductOf units1 units2) (GenericProductOf units2 units2) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units2 units1) (GenericProductOf units2 units2) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units1 units1) (GenericProductOf units1 units2) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units1 units1) (GenericProductOf units2 units1) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units1 units3) (GenericProductOf units2 units3) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units3 units1) (GenericProductOf units3 units2) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units3 units1) (GenericProductOf units2 units3) = GenericQuotientOf units1 units2
  GenericQuotient (GenericProductOf units1 units3) (GenericProductOf units3 units2) = GenericQuotientOf units1 units2

instance Squared (Generic units) (GenericProductOf units units)

class Specialize genericUnits specificUnits | genericUnits -> specificUnits

instance Specialize (Generic units) units

instance Product units1 units2 units3 => Specialize (GenericProductOf units1 units2) units3

instance Quotient units1 units2 units3 => Specialize (GenericQuotientOf units1 units2) units3
