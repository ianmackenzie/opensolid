module OpenSolid.Nondegenerate (Nondegenerate (Nondegenerate), unwrap) where

newtype Nondegenerate a = Nondegenerate a

{-# INLINE unwrap #-}
unwrap :: Nondegenerate a -> a
unwrap (Nondegenerate value) = value
