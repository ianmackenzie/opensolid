{-# LANGUAGE UnboxedTuples #-}

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
  , (==#)
  , (>#)
  , (<#)
  , (>=#)
  , (<=#)
  , squared#
  , sqrt#
  , max#
  , min#
  , abs#
  , hypot2#
  , hypot3#
  , hull2#
  , hull3#
  , hull4#
  , negateBounds#
  , doublePlusBounds#
  , boundsPlusDouble#
  , boundsPlusBounds#
  , doubleMinusBounds#
  , boundsMinusDouble#
  , boundsMinusBounds#
  , doubleTimesBounds#
  , boundsTimesDouble#
  , boundsTimesBounds#
  , doubleOverBounds#
  , boundsOverDouble#
  , boundsOverBounds#
  , determinantBounds2D#
  , determinantBounds3D#
  )
where

import GHC.Exts (Double (D#), Double#, Int (I#), Int#)
import GHC.Exts qualified

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

{-# INLINE negate# #-}
negate# :: Double# -> Double#
negate# = GHC.Exts.negateDouble#

{-# INLINE squared# #-}
squared# :: Double# -> Double#
squared# value# = value# *# value#

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

{-# INLINE hull2# #-}
hull2# :: Double# -> Double# -> (# Double#, Double# #)
hull2# a# b# =
  case (# a# <=# b#, b# <=# a# #) of
    (# 1#, _ #) -> (# a#, b# #)
    (# _, 1# #) -> (# b#, a# #)
    -- One of a or b must be NaN,
    -- so return an infinite bounds
    -- since we can't guarantee anything
    (# _, _ #) -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)

{-# INLINE hull3# #-}
hull3# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
hull3# a# b# c# = hull2# (min# a# (min# b# c#)) (max# a# (max# b# c#))

{-# INLINE hull4# #-}
hull4# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
hull4# a# b# c# d# =
  hull2# (min# a# (min# b# (min# c# d#))) (max# a# (max# b# (max# c# d#)))

{-# INLINE negateBounds# #-}
negateBounds# :: Double# -> Double# -> (# Double#, Double# #)
negateBounds# low# high# = (# high#, low# #)

{-# INLINE doublePlusBounds# #-}
doublePlusBounds# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doublePlusBounds# value# low# high# = hull2# (value# +# low#) (value# +# high#)

{-# INLINE boundsPlusDouble# #-}
boundsPlusDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsPlusDouble# low# high# value# = hull2# (low# +# value#) (high# +# value#)

{-# INLINE boundsPlusBounds# #-}
boundsPlusBounds# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsPlusBounds# low1# high1# low2# high2# = hull2# (low1# +# low2#) (high1# +# high2#)

{-# INLINE doubleMinusBounds# #-}
doubleMinusBounds# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doubleMinusBounds# value# low# high# = hull2# (value# -# high#) (value# -# low#)

{-# INLINE boundsMinusDouble# #-}
boundsMinusDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsMinusDouble# low# high# value# = hull2# (low# -# value#) (high# -# value#)

{-# INLINE boundsMinusBounds# #-}
boundsMinusBounds# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsMinusBounds# low1# high1# low2# high2# = hull2# (low1# -# high2#) (high1# -# low2#)

{-# INLINE doubleTimesBounds# #-}
doubleTimesBounds# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doubleTimesBounds# value# low# high# = hull2# (value# *# low#) (value# *# high#)

{-# INLINE boundsTimesDouble# #-}
boundsTimesDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsTimesDouble# low# high# value# = hull2# (low# *# value#) (high# *# value#)

{-# INLINE boundsTimesBounds# #-}
boundsTimesBounds# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsTimesBounds# low1# high1# low2# high2# =
  hull4# (low1# *# low2#) (low1# *# high2#) (high1# *# low2#) (high1# *# high2#)

{-# INLINE doubleOverBounds# #-}
doubleOverBounds# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doubleOverBounds# value# low# high# =
  case (# low# <=# 0.0##, high# >=# 0.0## #) of
    (# 1#, 1# #) -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)
    (# _, _ #) -> hull2# (value# /# low#) (value# /# high#)

{-# INLINE boundsOverDouble# #-}
boundsOverDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsOverDouble# low# high# value# =
  case value# ==# 0.0## of
    1# -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)
    _ -> hull2# (low# /# value#) (high# /# value#)

{-# INLINE boundsOverBounds# #-}
boundsOverBounds# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
boundsOverBounds# low1# high1# low2# high2# =
  case (# low2# <=# 0.0##, high2# >=# 0.0## #) of
    (# 1#, 1# #) -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)
    (# _, _ #) -> hull4# (low1# /# low2#) (low1# /# high2#) (high1# /# low2#) (high1# /# high2#)

{-# INLINE determinantBounds2D# #-}
determinantBounds2D# ::
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  (# Double#, Double# #)
determinantBounds2D# x1Min# x1Max# y1Min# y1Max# x2Min# x2Max# y2Min# y2Max# = do
  let !(# x1y2Min#, x1y2Max# #) = boundsTimesBounds# x1Min# x1Max# y2Min# y2Max#
  let !(# x2y1Min#, x2y1Max# #) = boundsTimesBounds# x2Min# x2Max# y1Min# y1Max#
  boundsMinusBounds# x1y2Min# x1y2Max# x2y1Min# x2y1Max#

determinantBounds3D# ::
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  (# Double#, Double# #)
determinantBounds3D#
  x1Min#
  x1Max#
  y1Min#
  y1Max#
  z1Min#
  z1Max#
  x2Min#
  x2Max#
  y2Min#
  y2Max#
  z2Min#
  z2Max#
  x3Min#
  x3Max#
  y3Min#
  y3Max#
  z3Min#
  z3Max# = do
    let !(# x1DetMin#, x1DetMax# #) =
          determinantBounds2D# y2Min# y2Max# z2Min# z2Max# y3Min# y3Max# z3Min# z3Max#
    let !(# y1DetMin#, y1DetMax# #) =
          determinantBounds2D# x2Min# x2Max# z2Min# z2Max# x3Min# x3Max# z3Min# z3Max#
    let !(# z1DetMin#, z1DetMax# #) =
          determinantBounds2D# x2Min# x2Max# y2Min# y2Max# x3Min# x3Max# y3Min# y3Max#
    let !(# x1ProdMin#, x1ProdMax# #) =
          boundsTimesBounds# x1Min# x1Max# x1DetMin# x1DetMax#
    let !(# y1ProdMin#, y1ProdMax# #) =
          boundsTimesBounds# y1Min# y1Max# y1DetMin# y1DetMax#
    let !(# z1ProdMin#, z1ProdMax# #) =
          boundsTimesBounds# z1Min# z1Max# z1DetMin# z1DetMax#
    let !(# dxyMin#, dxyMax# #) =
          boundsMinusBounds# x1ProdMin# x1ProdMax# y1ProdMin# y1ProdMax#
    boundsPlusBounds# dxyMin# dxyMax# z1ProdMin# z1ProdMax#
