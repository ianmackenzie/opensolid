module OpenSolid.Sign (value) where

import OpenSolid.Prelude

{-# INLINE value #-}
value :: Sign -> Number
value (Sign x) = x
