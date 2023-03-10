module Curve1d.Region
  ( Region (Region, domain, nonZeroDerivativeOrder, nonZeroDerivativeSign)
  , merge
  )
where

import OpenSolid
import Range (Range (Range))
import Range qualified
import Units (Unitless)

data Region = Region
  { domain :: Range Unitless
  , nonZeroDerivativeOrder :: Int
  , nonZeroDerivativeSign :: Sign
  }
  deriving (Eq, Show)

merge :: Region -> Region -> Maybe Region
merge (Region (Range low1 high1) order1 sign1) (Region (Range low2 high2) order2 sign2)
  | order1 == order2 && sign1 == sign2 && high1 == low2 =
      Just (Region (Range.unsafe low1 high2) order1 sign1)
  | otherwise = Nothing
