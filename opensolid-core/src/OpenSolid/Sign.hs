module OpenSolid.Sign
  ( random
  , value
  )
where

import OpenSolid.Prelude
import OpenSolid.Random.Internal qualified as Random
import System.Random qualified

random :: Random.Generator Sign
random = Random.Generator System.Random.uniform

{-# INLINE value #-}
value :: Sign -> Number
value (Sign x) = x
