module Units (
    Meters,
    SquareMeters,
    CubicMeters,
) where

import OpenSolid

-- Meters

data Meters = Meters

instance Multiplication Unitless Meters where
    type Product Unitless Meters = Meters
    Unitless * Meters = Meters

instance Multiplication Meters Unitless where
    type Product Meters Unitless = Meters
    Meters * Unitless = Meters

instance Division Meters Meters where
    type Quotient Meters Meters = Unitless
    Meters / Meters = Unitless

instance Division Meters Unitless where
    type Quotient Meters Unitless = Meters
    Meters / Unitless = Meters

-- Square meters

data SquareMeters = SquareMeters

instance Multiplication Unitless SquareMeters where
    type Product Unitless SquareMeters = SquareMeters
    Unitless * SquareMeters = SquareMeters

instance Multiplication SquareMeters Unitless where
    type Product SquareMeters Unitless = SquareMeters
    SquareMeters * Unitless = SquareMeters

instance Division SquareMeters SquareMeters where
    type Quotient SquareMeters SquareMeters = Unitless
    SquareMeters / SquareMeters = Unitless

instance Division SquareMeters Unitless where
    type Quotient SquareMeters Unitless = SquareMeters
    SquareMeters / Unitless = SquareMeters

-- Cubic meters

data CubicMeters = CubicMeters

instance Multiplication Unitless CubicMeters where
    type Product Unitless CubicMeters = CubicMeters
    Unitless * CubicMeters = CubicMeters

instance Multiplication CubicMeters Unitless where
    type Product CubicMeters Unitless = CubicMeters
    CubicMeters * Unitless = CubicMeters

instance Division CubicMeters CubicMeters where
    type Quotient CubicMeters CubicMeters = Unitless
    CubicMeters / CubicMeters = Unitless

instance Division CubicMeters Unitless where
    type Quotient CubicMeters Unitless = CubicMeters
    CubicMeters / Unitless = CubicMeters

-- Products

instance Multiplication Meters Meters where
    type Product Meters Meters = SquareMeters
    Meters * Meters = SquareMeters

instance Multiplication Meters SquareMeters where
    type Product Meters SquareMeters = CubicMeters
    Meters * SquareMeters = CubicMeters

instance Multiplication SquareMeters Meters where
    type Product SquareMeters Meters = CubicMeters
    SquareMeters * Meters = CubicMeters

-- Quotients

instance Division SquareMeters Meters where
    type Quotient SquareMeters Meters = Meters
    SquareMeters / Meters = Meters

instance Division CubicMeters SquareMeters where
    type Quotient CubicMeters SquareMeters = Meters
    CubicMeters / SquareMeters = Meters

instance Division CubicMeters Meters where
    type Quotient CubicMeters Meters = SquareMeters
    CubicMeters / Meters = SquareMeters

-- Square roots

instance Sqrt SquareMeters where
    type SquareRoot SquareMeters = Meters
    sqrt SquareMeters = Meters
