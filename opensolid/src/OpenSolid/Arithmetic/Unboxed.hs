module OpenSolid.Arithmetic.Unboxed
  ( Double#
  , sin#
  , cos#
  , tan#
  , asin#
  , acos#
  , atan#
  , negate#
  , sqrt#
  , (+#)
  , (-#)
  , (*#)
  , (/#)
  , (==#)
  , (<#)
  , (>#)
  , (<=#)
  , (>=#)
  , min#
  , max#
  )
where

import GHC.Exts (Double#)
import GHC.Exts qualified
import OpenSolid.Bootstrap

{-# INLINE negate# #-}
negate# :: Double# -> Double#
negate# = GHC.Exts.negateDouble#

{-# INLINE sqrt# #-}
sqrt# :: Double# -> Double#
sqrt# = GHC.Exts.sqrtDouble#

{-# INLINE sin# #-}
sin# :: Double# -> Double#
sin# = GHC.Exts.sinDouble#

{-# INLINE cos# #-}
cos# :: Double# -> Double#
cos# = GHC.Exts.cosDouble#

{-# INLINE tan# #-}
tan# :: Double# -> Double#
tan# = GHC.Exts.tanDouble#

{-# INLINE asin# #-}
asin# :: Double# -> Double#
asin# = GHC.Exts.asinDouble#

{-# INLINE acos# #-}
acos# :: Double# -> Double#
acos# = GHC.Exts.acosDouble#

{-# INLINE atan# #-}
atan# :: Double# -> Double#
atan# = GHC.Exts.atanDouble#

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

{-# INLINE (==#) #-}
(==#) :: Double# -> Double# -> Bool
x# ==# y# = case x# GHC.Exts.==## y# of 1# -> True; _ -> False

infix 4 ==#

{-# INLINE (<#) #-}
(<#) :: Double# -> Double# -> Bool
x# <# y# = case x# GHC.Exts.<## y# of 1# -> True; _ -> False

infix 4 <#

{-# INLINE (>#) #-}
(>#) :: Double# -> Double# -> Bool
x# ># y# = case x# GHC.Exts.>## y# of 1# -> True; _ -> False

infix 4 >#

{-# INLINE (>=#) #-}
(>=#) :: Double# -> Double# -> Bool
x# >=# y# = case x# GHC.Exts.>=## y# of 1# -> True; _ -> False

infix 4 >=#

{-# INLINE (<=#) #-}
(<=#) :: Double# -> Double# -> Bool
x# <=# y# = case x# GHC.Exts.<=## y# of 1# -> True; _ -> False

infix 4 <=#

{-# INLINE min# #-}
min# :: Double# -> Double# -> Double#
min# x# y# = case x# GHC.Exts.<=## y# of 1# -> x#; _ -> y#

{-# INLINE max# #-}
max# :: Double# -> Double# -> Double#
max# x# y# = case x# GHC.Exts.>=## y# of 1# -> x#; _ -> y#
