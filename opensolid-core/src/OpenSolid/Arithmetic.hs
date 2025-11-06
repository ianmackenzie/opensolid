module OpenSolid.Arithmetic
  ( Negation (negative)
  , Addition ((+), (.+.))
  , (+.)
  , (.+)
  , Subtraction ((-), (.-.))
  , (-.)
  , (.-)
  , Multiplication# ((#*#))
  , (*#)
  , (#*)
  , Multiplication ((.*.))
  , (*.)
  , (.*)
  , Division# ((#/#))
  , (/#)
  , (#/)
  , Division ((./.))
  , (/.)
  , (./)
  , DotMultiplication# (dot#)
  , DotMultiplication (dot)
  , CrossMultiplication# (cross#)
  , CrossMultiplication (cross)
  )
where

import {-# SOURCE #-} OpenSolid.Number (Number)
import {-# SOURCE #-} OpenSolid.Sign (Sign)

class (Multiplication Sign a a, Multiplication a Sign a) => Negation a where
  negative :: a -> a

class Addition a b c | a b -> c where
  (+) :: a -> b -> c
  (.+.) :: a -> b -> c

  (+) = (.+.)
  (.+.) = (+)

  {-# MINIMAL (+) | (.+.) #-}

infixl 6 +

infixl 6 .+.

{-# INLINE (+.) #-}
(+.) :: Addition Number a b => Number -> a -> b
(+.) = (+)

infixl 6 +.

{-# INLINE (.+) #-}
(.+) :: Addition a Number b => a -> Number -> b
(.+) = (+)

infixl 6 .+

class Subtraction a b c | a b -> c where
  (-) :: a -> b -> c
  (.-.) :: a -> b -> c

  (-) = (.-.)
  (.-.) = (-)

  {-# MINIMAL (-) | (.-.) #-}

infixl 6 -

infixl 6 .-.

{-# INLINE (-.) #-}
(-.) :: Subtraction Number a b => Number -> a -> b
(-.) = (-)

infixl 6 -.

{-# INLINE (.-) #-}
(.-) :: Subtraction a Number b => a -> Number -> b
(.-) = (-)

infixl 6 .-

class Multiplication# a b c | a b -> c where
  (#*#) :: a -> b -> c

infixl 7 #*#

{-# INLINE (*#) #-}
(*#) :: Multiplication# Number a b => Number -> a -> b
(*#) = (#*#)

infixl 7 *#

{-# INLINE (#*) #-}
(#*) :: Multiplication# a Number b => a -> Number -> b
(#*) = (#*#)

infixl 7 #*

class Multiplication b a c => Multiplication a b c | a b -> c where
  (.*.) :: a -> b -> c

infixl 7 .*.

{-# INLINE (*.) #-}
(*.) :: Multiplication Number a b => Number -> a -> b
(*.) = (.*.)

infixl 7 *.

{-# INLINE (.*) #-}
(.*) :: Multiplication a Number b => a -> Number -> b
(.*) = (.*.)

infixl 7 .*

class Division# a b c | a b -> c where
  (#/#) :: a -> b -> c

infixl 7 #/#

{-# INLINE (/#) #-}
(/#) :: Division# Number a b => Number -> a -> b
(/#) = (#/#)

infixl 7 /#

{-# INLINE (#/) #-}
(#/) :: Division# a Number b => a -> Number -> b
(#/) = (#/#)

infixl 7 #/

class Division a b c | a b -> c where
  (./.) :: a -> b -> c

infixl 7 ./.

{-# INLINE (/.) #-}
(/.) :: Division Number a b => Number -> a -> b
(/.) = (./.)

infixl 7 /.

{-# INLINE (./) #-}
(./) :: Division a Number b => a -> Number -> b
(./) = (./.)

infixl 7 ./

class DotMultiplication# a b c | a b -> c where
  dot# :: a -> b -> c

infixl 7 `dot#`

class DotMultiplication b a c => DotMultiplication a b c | a b -> c where
  dot :: DotMultiplication a b c => a -> b -> c

infixl 7 `dot`

class CrossMultiplication# a b c | a b -> c where
  cross# :: a -> b -> c

infixl 7 `cross#`

class CrossMultiplication b a c => CrossMultiplication a b c | a b -> c where
  cross :: a -> b -> c

infixl 7 `cross`
