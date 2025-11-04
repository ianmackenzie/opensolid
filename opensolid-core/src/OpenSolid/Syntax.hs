module OpenSolid.Syntax
  ( negative
  , (.+.)
  , (.-.)
  , (.*.)
  , (./.)
  , (+.)
  , (-.)
  , (*.)
  , (/.)
  , (.+)
  , (.-)
  , (.*)
  , (./)
  , (#*#)
  , (#/#)
  , (*#)
  , (/#)
  , (#*)
  , (#/)
  , dot
  , dot#
  , cross
  , cross#
  , compose
  , (%)
  , (//)
  , (|>)
  , (@)
  , type (@)
  )
where

import OpenSolid.Prelude hiding ((*#), (/#))
import OpenSolid.Prelude qualified

{-# INLINE negative #-}
negative :: Negation a => a -> a
negative = negate

{-# INLINE (.+.) #-}
(.+.) :: Addition a b c => a -> b -> c
(.+.) = (+)

infixl 6 .+.

{-# INLINE (.-.) #-}
(.-.) :: Subtraction a b c => a -> b -> c
(.-.) = (-)

infixl 6 .-.

{-# INLINE (.*.) #-}
(.*.) :: Multiplication a b c => a -> b -> c
(.*.) = (*)

infixl 7 .*.

{-# INLINE (./.) #-}
(./.) :: Division a b c => a -> b -> c
(./.) = (/)

infixl 7 ./.

{-# INLINE (+.) #-}
(+.) :: Addition Number a b => Number -> a -> b
(+.) = (+)

infixl 6 +.

{-# INLINE (-.) #-}
(-.) :: Subtraction Number a b => Number -> a -> b
(-.) = (-)

infixl 6 -.

{-# INLINE (*.) #-}
(*.) :: Multiplication Number a b => Number -> a -> b
(*.) = (*)

infixl 7 *.

{-# INLINE (/.) #-}
(/.) :: Division Number a b => Number -> a -> b
(/.) = (/)

infixl 7 /.

{-# INLINE (.+) #-}
(.+) :: Addition a Number b => a -> Number -> b
(.+) = (+)

infixl 6 .+

{-# INLINE (.-) #-}
(.-) :: Subtraction a Number b => a -> Number -> b
(.-) = (-)

infixl 6 .-

{-# INLINE (.*) #-}
(.*) :: Multiplication a Number b => a -> Number -> b
(.*) = (*)

infixl 7 .*

{-# INLINE (./) #-}
(./) :: Division a Number b => a -> Number -> b
(./) = (/)

infixl 7 ./

{-# INLINE (#*#) #-}
(#*#) :: Multiplication# a b c => a -> b -> c
(#*#) = (OpenSolid.Prelude.*#)

infixl 7 #*#

{-# INLINE (#/#) #-}
(#/#) :: Division# a b c => a -> b -> c
(#/#) = (OpenSolid.Prelude./#)

infixl 7 #/#

{-# INLINE (*#) #-}
(*#) :: Multiplication# Number a b => Number -> a -> b
(*#) = (OpenSolid.Prelude.*#)

infixl 7 *#

{-# INLINE (/#) #-}
(/#) :: Division# Number a b => Number -> a -> b
(/#) = (OpenSolid.Prelude./#)

infixl 7 /#

{-# INLINE (#*) #-}
(#*) :: Multiplication# a Number b => a -> Number -> b
(#*) = (OpenSolid.Prelude.*#)

infixl 7 #*

{-# INLINE (#/) #-}
(#/) :: Division# a Number b => a -> Number -> b
(#/) = (OpenSolid.Prelude./#)

infixl 7 #/

{-# INLINE compose #-}
compose :: Composition a b c => b -> a -> c
compose = (.)

infixr 9 `compose`
