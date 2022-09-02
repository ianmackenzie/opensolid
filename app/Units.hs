module Units (
    Meters,
    SquareMeters,
    CubicMeters,
) where

import OpenSolid

instance Multiplication Unitless units units where
    Unitless * units = units

instance Multiplication units Unitless units where
    units * Unitless = units

instance Division units Unitless units where
    units / Unitless = units

data Meters = Meters

instance Division Meters Meters Unitless where
    Meters / Meters = Unitless

data SquareMeters = SquareMeters

instance Division SquareMeters SquareMeters Unitless where
    SquareMeters / SquareMeters = Unitless

data CubicMeters = CubicMeters

instance Division CubicMeters CubicMeters Unitless where
    CubicMeters / CubicMeters = Unitless

instance Multiplication Meters Meters SquareMeters where
    Meters * Meters = SquareMeters

instance Division SquareMeters Meters Meters where
    SquareMeters / Meters = Meters

instance Sqrt SquareMeters Meters where
    sqrt SquareMeters = Meters

instance Multiplication Meters SquareMeters CubicMeters where
    Meters * SquareMeters = CubicMeters

instance Division CubicMeters SquareMeters Meters where
    CubicMeters / SquareMeters = Meters

instance Multiplication SquareMeters Meters CubicMeters where
    SquareMeters * Meters = CubicMeters

instance Division CubicMeters Meters SquareMeters where
    CubicMeters / Meters = SquareMeters
