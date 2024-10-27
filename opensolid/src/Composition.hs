module Composition
  ( Composition ((>>), (.))
  , (:>>:) (..)
  , type (:.:)
  , pattern (:.:)
  )
where

import Prelude (Eq, IO, Show, type (~))
import Prelude qualified

class Composition a b c | a b -> c where
  (>>) :: a -> b -> c
  (.) :: b -> a -> c
  a >> b = b . a
  a . b = b >> a
  {-# MINIMAL (>>) | (.) #-}

infixl 9 >>

infixr 9 .

instance b ~ b' => Composition (a -> b) (b' -> c) (a -> c) where
  (.) = (Prelude..)

instance Composition (IO ()) (IO a) (IO a) where
  (>>) = (Prelude.>>)

data a :>>: b = a :>>: b deriving (Eq, Show)

type a :.: b = b :>>: a

{-# COMPLETE (:.:) #-}

pattern (:.:) :: a -> b -> a :.: b
pattern a :.: b <- b :>>: a
  where
    a :.: b = b :>>: a
