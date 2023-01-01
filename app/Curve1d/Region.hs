module Curve1d.Region (
    Region (Region),
    domain,
    nonZeroDerivativeOrder,
    nonZeroDerivativeSign,
    merge,
) where

import OpenSolid
import Range (Range)
import Range qualified

data Region = Region
    { domain :: Range Unitless
    , nonZeroDerivativeOrder :: Int
    , nonZeroDerivativeSign :: Sign
    }
    deriving (Eq, Show)

merge :: Region -> Region -> Maybe Region
merge (Region leftDomain leftOrder leftSign) (Region rightDomain rightOrder rightSign) =
    if leftOrder == rightOrder && leftSign == rightSign && Range.maxValue leftDomain == Range.minValue rightDomain
        then Just (Region (Range.aggregate leftDomain rightDomain) leftOrder leftSign)
        else Nothing
