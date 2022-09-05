module Units where

-- Units arithmetic type classes

class Multiplication units1 units2 where
    type Product units1 units2

class Division units1 units2 where
    type Quotient units1 units2

class Sqrt units where
    type SquareRoot units

-- Unitless 'units' type and related generic arithmetic rules

data Unitless

instance {-# OVERLAPPING #-} Multiplication Unitless Unitless where
    type Product Unitless Unitless = Unitless

instance {-# OVERLAPPING #-} Multiplication Unitless units where
    type Product Unitless units = units

instance {-# OVERLAPPING #-} Multiplication units Unitless where
    type Product units Unitless = units

instance {-# OVERLAPPING #-} Division Unitless Unitless where
    type Quotient Unitless Unitless = Unitless

instance {-# OVERLAPPING #-} Division units Unitless where
    type Quotient units Unitless = units

instance {-# OVERLAPPING #-} Division units units where
    type Quotient units units = Unitless

instance Sqrt Unitless where
    type SquareRoot Unitless = Unitless

-- Units

data Meters

data SquareMeters

data CubicMeters

-- Products

instance Multiplication Meters Meters where
    type Product Meters Meters = SquareMeters

instance Multiplication Meters SquareMeters where
    type Product Meters SquareMeters = CubicMeters

instance Multiplication SquareMeters Meters where
    type Product SquareMeters Meters = CubicMeters

-- Quotients

instance Division SquareMeters Meters where
    type Quotient SquareMeters Meters = Meters

instance Division CubicMeters SquareMeters where
    type Quotient CubicMeters SquareMeters = Meters

instance Division CubicMeters Meters where
    type Quotient CubicMeters Meters = SquareMeters

-- Square roots

instance Sqrt SquareMeters where
    type SquareRoot SquareMeters = Meters
