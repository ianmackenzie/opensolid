module OpenSolid.Uniform
  ( Uniform (Uniform)
  , unwrap
  , steps
  , leading
  , trailing
  , inBetween
  , midpoints
  , samples
  , reverse
  , random
  )
where

import Data.Coerce qualified
import OpenSolid.Parameter qualified as Parameter
import OpenSolid.Prelude
import OpenSolid.Random qualified as Random

newtype Uniform = Uniform Number

instance Multiplication Uniform (Quantity units) (Quantity units) where
  {-# INLINE (*) #-}
  Uniform r * quantity = r * quantity

instance Multiplication (Quantity units) Uniform (Quantity units) where
  {-# INLINE (*) #-}
  quantity * Uniform r = quantity * r

{-# INLINE unwrap #-}
unwrap :: Uniform -> Number
unwrap (Uniform r) = r

steps :: Int -> List Uniform
steps = Data.Coerce.coerce Parameter.steps

leading :: Int -> List Uniform
leading = Data.Coerce.coerce Parameter.leading

trailing :: Int -> List Uniform
trailing = Data.Coerce.coerce Parameter.trailing

inBetween :: Int -> List Uniform
inBetween = Data.Coerce.coerce Parameter.inBetween

midpoints :: Int -> List Uniform
midpoints = Data.Coerce.coerce Parameter.midpoints

samples :: NonEmpty Uniform
samples = Data.Coerce.coerce Parameter.samples

{-# INLINE reverse #-}
reverse :: Uniform -> Uniform
reverse (Uniform r) = Uniform (1.0 - r)

random :: Random.Generator Uniform
random = Random.map Uniform Parameter.random
