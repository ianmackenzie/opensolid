module Units
  ( Coercion
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

data GenericSquared units

drop :: Coercion a => a units -> a Unitless
drop = unsafeCoerce

add :: Coercion a => a Unitless -> a units
add = unsafeCoerce

generalize :: Coercion a => a units -> a (Generic units)
generalize = unsafeCoerce

specialize :: Coercion a => a (Generic units) -> a units
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

instance Product (Generic units) (Generic units) (GenericSquared units)

instance Quotient (GenericSquared units) (Generic units) (Generic units)

instance Squared (Generic units) (GenericSquared units)
