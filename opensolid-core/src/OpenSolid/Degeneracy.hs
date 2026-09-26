module OpenSolid.Degeneracy
  ( windowSize
  , startValue
  , endValue
  , startRange
  , endRange
  )
where

import OpenSolid.Interval (Interval (Interval))
import OpenSolid.Prelude

windowSize :: Number
windowSize = 1 / 16

startValue :: Number
startValue = windowSize

endValue :: Number
endValue = 1.0 - windowSize

startRange :: Interval Unitless
startRange = Interval 0.0 startValue

endRange :: Interval Unitless
endRange = Interval endValue 1.0
