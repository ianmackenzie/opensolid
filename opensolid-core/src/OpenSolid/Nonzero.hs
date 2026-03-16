module OpenSolid.Nonzero (Nonzero (Nonzero), unwrap) where

newtype Nonzero a = Nonzero a

{-# INLINE unwrap #-}
unwrap :: Nonzero a -> a
unwrap (Nonzero value) = value
