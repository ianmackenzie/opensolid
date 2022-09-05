module Units where

class Multiplication units1 units2 where
    type Product units1 units2

class Division units1 units2 where
    type Quotient units1 units2

class Sqrt units where
    type SquareRoot units

-- Unitless

data Unitless

instance Multiplication Unitless Unitless where
    type Product Unitless Unitless = Unitless

instance Division Unitless Unitless where
    type Quotient Unitless Unitless = Unitless

instance Sqrt Unitless where
    type SquareRoot Unitless = Unitless

-- Meters

data Meters

instance Multiplication Unitless Meters where
    type Product Unitless Meters = Meters

instance Multiplication Meters Unitless where
    type Product Meters Unitless = Meters

instance Division Meters Meters where
    type Quotient Meters Meters = Unitless

instance Division Meters Unitless where
    type Quotient Meters Unitless = Meters

-- Square meters

data SquareMeters

instance Multiplication Unitless SquareMeters where
    type Product Unitless SquareMeters = SquareMeters

instance Multiplication SquareMeters Unitless where
    type Product SquareMeters Unitless = SquareMeters

instance Division SquareMeters SquareMeters where
    type Quotient SquareMeters SquareMeters = Unitless

instance Division SquareMeters Unitless where
    type Quotient SquareMeters Unitless = SquareMeters

instance Sqrt SquareMeters where
    type SquareRoot SquareMeters = Meters

-- Cubic meters

data CubicMeters

instance Multiplication Unitless CubicMeters where
    type Product Unitless CubicMeters = CubicMeters

instance Multiplication CubicMeters Unitless where
    type Product CubicMeters Unitless = CubicMeters

instance Division CubicMeters CubicMeters where
    type Quotient CubicMeters CubicMeters = Unitless

instance Division CubicMeters Unitless where
    type Quotient CubicMeters Unitless = CubicMeters

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
