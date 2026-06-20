module OpenSolid.Nondegenerate
  ( Nondegenerate (Nondegenerate)
  , unwrap
  , interior
  , exterior
  , Field
  , field
  , get
  , map
  )
where

import OpenSolid.Nonzero (Nonzero (Nonzero))
import OpenSolid.Units qualified as Units

newtype Nondegenerate a = Nondegenerate a

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

data Field value = Field ~value

instance Units.Coercion a b => Units.Coercion (Field a) (Field b) where
  coerce (Field value) = Field (Units.coerce value)

field :: (Nondegenerate a -> b) -> a -> Field b
field function object = Field (function (Nondegenerate object))

get :: (a -> Field b) -> Nondegenerate a -> b
get accessor (Nondegenerate object) = let Field value = accessor object in value

map :: (a -> b) -> Field a -> Field b
map function (Field value) = Field (function value)
