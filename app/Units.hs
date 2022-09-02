module Units (
    Meters,
    SquareMeters,
    CubicMeters,
) where

import OpenSolid

-- Meters

data Meters = Meters

instance Multiplication Unitless Meters Meters where
    Unitless * Meters = Meters

instance Multiplication Meters Unitless Meters where
    Meters * Unitless = Meters

instance Division Meters Meters Unitless where
    Meters / Meters = Unitless

instance Division Meters Unitless Meters where
    Meters / Unitless = Meters

-- Square meters

data SquareMeters = SquareMeters

instance Multiplication Unitless SquareMeters SquareMeters where
    Unitless * SquareMeters = SquareMeters

instance Multiplication SquareMeters Unitless SquareMeters where
    SquareMeters * Unitless = SquareMeters

instance Division SquareMeters SquareMeters Unitless where
    SquareMeters / SquareMeters = Unitless

instance Division SquareMeters Unitless SquareMeters where
    SquareMeters / Unitless = SquareMeters

-- Cubic meters

data CubicMeters = CubicMeters

instance Multiplication Unitless CubicMeters CubicMeters where
    Unitless * CubicMeters = CubicMeters

instance Multiplication CubicMeters Unitless CubicMeters where
    CubicMeters * Unitless = CubicMeters

instance Division CubicMeters CubicMeters Unitless where
    CubicMeters / CubicMeters = Unitless

instance Division CubicMeters Unitless CubicMeters where
    CubicMeters / Unitless = CubicMeters

-- Products

instance Multiplication Meters Meters SquareMeters where
    Meters * Meters = SquareMeters

instance Multiplication Meters SquareMeters CubicMeters where
    Meters * SquareMeters = CubicMeters

instance Multiplication SquareMeters Meters CubicMeters where
    SquareMeters * Meters = CubicMeters

-- Quotients

instance Division SquareMeters Meters Meters where
    SquareMeters / Meters = Meters

instance Division CubicMeters SquareMeters Meters where
    CubicMeters / SquareMeters = Meters

instance Division CubicMeters Meters SquareMeters where
    CubicMeters / Meters = SquareMeters

-- Square roots

instance Sqrt SquareMeters Meters where
    sqrt SquareMeters = Meters
