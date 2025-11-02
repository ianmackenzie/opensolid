module OpenSolid.Syntax
  ( int
  , float
  , negative
  , (.+)
  , (.-)
  , (.*)
  , (.*.)
  , (./)
  , (./.)
  , half
  , twice
  , dot
  , dot'
  , cross
  , cross'
  , compose
  , (%)
  , (//)
  , (|>)
  , (@)
  , type (@)
  )
where

import OpenSolid.Prelude

{-# INLINE int #-}
int :: Int -> Int
int = id

{-# INLINE float #-}
float :: Float -> Float
float = id

{-# INLINE negative #-}
negative :: Negation a => a -> a
negative = negate

{-# INLINE half #-}
half :: Multiplication Float a b => a -> b
half value = 0.5 * value

{-# INLINE twice #-}
twice :: Multiplication Float a b => a -> b
twice value = 2.0 * value

{-# INLINE (.+) #-}
(.+) :: Addition a b c => a -> b -> c
(.+) = (+)

infixl 6 .+

{-# INLINE (.-) #-}
(.-) :: Subtraction a b c => a -> b -> c
(.-) = (-)

infixl 6 .-

{-# INLINE (.*) #-}
(.*) :: Multiplication a b c => a -> b -> c
(.*) = (*)

infixl 7 .*

{-# INLINE (./) #-}
(./) :: Division a b c => a -> b -> c
(./) = (/)

infixl 7 ./

{-# INLINE compose #-}
compose :: Composition a b c => b -> a -> c
compose = (.)

infixr 9 `compose`
