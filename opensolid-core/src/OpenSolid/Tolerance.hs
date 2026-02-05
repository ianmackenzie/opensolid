module OpenSolid.Tolerance
  ( Tolerance
  , using
  )
where

import OpenSolid.Prelude

using :: Quantity units -> (Tolerance units => a) -> a
using tolerance expression = let ?tolerance = tolerance in expression
