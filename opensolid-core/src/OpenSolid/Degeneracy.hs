module OpenSolid.Degeneracy (windowSize, tStart, tEnd) where

import OpenSolid.Prelude

windowSize :: Number
windowSize = 1 / 16

tStart :: Number
tStart = windowSize

tEnd :: Number
tEnd = 1.0 - windowSize
