module Interval (
    Interval,
    constant,
    from,
    minValue,
    maxValue,
    midpoint,
    endpoints,
    squared,
    contains,
    bisect,
    unit,
    isAtomic,
) where

import OpenSolid
import Range (Range)
import qualified Range
import Range.Unsafe

type Interval = Range Unitless

constant :: Float -> Interval
constant =
    Range.constant

from :: Float -> Float -> Interval
from =
    Range.from

minValue :: Interval -> Float
minValue =
    Range.minValue

maxValue :: Interval -> Float
maxValue =
    Range.maxValue

midpoint :: Interval -> Float
midpoint =
    Range.midpoint

endpoints :: Interval -> (Float, Float)
endpoints =
    Range.endpoints

squared :: Interval -> Interval
squared =
    Range.squared

contains :: Float -> Interval -> Bool
contains =
    Range.contains

bisect :: Interval -> (Interval, Interval)
bisect =
    Range.bisect

unit :: Interval
unit =
    Range 0.0 1.0

isAtomic :: Interval -> Bool
isAtomic interval =
    let (Range low high) = interval
        mid = midpoint interval
     in mid == low || mid == high
