module OpenSolid.Nondegenerate
  ( Nondegenerate (Nondegenerate)
  , IsDegenerate (IsDegenerate)
  , unwrap
  , interior
  , exterior
  )
where

import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Prelude

newtype Nondegenerate a = Nondegenerate a

data IsDegenerate = IsDegenerate deriving (Eq, Show)

{-# INLINE unwrap #-}
unwrap :: Nondegenerate a -> a
unwrap (Nondegenerate value) = value

{-| If a value (curve, surface etc.) is nondegenerate,
that means it can only be zero at its boundaries.
Therefore, the interior of a nondegenerate value is a nonzero value.
This function simply extracts the underlying value
and returns it wrapped as a nonzero value,
for cases where you're only working with the interior portion of a nondegenerate value.
-}
interior :: Nondegenerate a -> Nonzero a
interior (Nondegenerate value) = Nonzero value

exterior :: Nonzero a -> Nondegenerate a
exterior (Nonzero value) = Nondegenerate value
