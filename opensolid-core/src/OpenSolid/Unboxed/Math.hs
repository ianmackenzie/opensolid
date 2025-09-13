module OpenSolid.Unboxed.Math
  ( Double#
  , Int#
  , Double (D#)
  , Int (I#)
  , negate#
  , (+#)
  , (-#)
  , (*#)
  , (/#)
  , half#
  , twice#
  , (==#)
  , (>#)
  , (<#)
  , (>=#)
  , (<=#)
  , sqrt#
  , max#
  , min#
  , abs#
  , hypot2#
  , hypot3#
  )
where

import GHC.Exts (Double (D#), Double#, Int (I#), Int#)
import GHC.Exts qualified

{-# INLINE negate# #-}
negate# :: Double# -> Double#
negate# = GHC.Exts.negateDouble#

{-# INLINE (+#) #-}
(+#) :: Double# -> Double# -> Double#
(+#) = (GHC.Exts.+##)

infixl 6 +#

{-# INLINE (-#) #-}
(-#) :: Double# -> Double# -> Double#
(-#) = (GHC.Exts.-##)

infixl 6 -#

{-# INLINE (*#) #-}
(*#) :: Double# -> Double# -> Double#
(*#) = (GHC.Exts.*##)

infixl 7 *#

{-# INLINE (/#) #-}
(/#) :: Double# -> Double# -> Double#
(/#) = (GHC.Exts./##)

infixl 7 /#

{-# INLINE half# #-}
half# :: Double# -> Double#
half# value# = 0.5## *# value#

{-# INLINE twice# #-}
twice# :: Double# -> Double#
twice# value# = 2.0## *# value#

{-# INLINE (==#) #-}
(==#) :: Double# -> Double# -> Int#
(==#) = (GHC.Exts.==##)

infix 4 ==#

{-# INLINE (>#) #-}
(>#) :: Double# -> Double# -> Int#
(>#) = (GHC.Exts.>##)

infix 4 >#

{-# INLINE (<#) #-}
(<#) :: Double# -> Double# -> Int#
(<#) = (GHC.Exts.<##)

infix 4 <#

{-# INLINE (>=#) #-}
(>=#) :: Double# -> Double# -> Int#
(>=#) = (GHC.Exts.>=##)

infix 4 >=#

{-# INLINE (<=#) #-}
(<=#) :: Double# -> Double# -> Int#
(<=#) = (GHC.Exts.<=##)

infix 4 <=#

{-# INLINE sqrt# #-}
sqrt# :: Double# -> Double#
sqrt# = GHC.Exts.sqrtDouble#

{-# INLINE max# #-}
max# :: Double# -> Double# -> Double#
max# a# b# = case a# >=# b# of 1# -> a#; _ -> b#

{-# INLINE min# #-}
min# :: Double# -> Double# -> Double#
min# a# b# = case a# <=# b# of 1# -> a#; _ -> b#

{-# INLINE abs# #-}
abs# :: Double# -> Double#
abs# = GHC.Exts.fabsDouble#

{-# INLINE hypot2# #-}
hypot2# :: Double# -> Double# -> Double#
hypot2# x# y# = sqrt# (x# *# x# +# y# *# y#)

{-# INLINE hypot3# #-}
hypot3# :: Double# -> Double# -> Double# -> Double#
hypot3# x# y# z# = sqrt# (x# *# x# +# y# *# y# +# z# *# z#)
