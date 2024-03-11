module Units (Unitless, Product, Quotient, Squared) where

data Unitless

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

instance {-# INCOHERENT #-} Product Unitless Unitless Unitless

instance {-# INCOHERENT #-} Product Unitless units units

instance {-# INCOHERENT #-} Product units Unitless units

instance {-# INCOHERENT #-} Quotient Unitless Unitless Unitless

instance {-# INCOHERENT #-} Quotient units Unitless units

instance {-# INCOHERENT #-} Quotient units units Unitless

instance Squared Unitless Unitless
