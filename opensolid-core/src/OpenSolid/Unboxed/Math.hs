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
  , negateInterval#
  , doublePlusInterval#
  , intervalPlusDouble#
  , intervalPlusInterval#
  , doubleMinusInterval#
  , intervalMinusDouble#
  , intervalMinusInterval#
  , doubleTimesInterval#
  , intervalTimesDouble#
  , intervalTimesInterval#
  , doubleOverInterval#
  , intervalOverDouble#
  , intervalOverInterval#
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

{-# INLINE negateInterval# #-}
negateInterval# :: Double# -> Double# -> (# Double#, Double# #)
negateInterval# low# high# = (# high#, low# #)

{-# INLINE doublePlusInterval# #-}
doublePlusInterval# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doublePlusInterval# value# low# high# = hull2# (value# +# low#) (value# +# high#)

{-# INLINE intervalPlusDouble# #-}
intervalPlusDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalPlusDouble# low# high# value# = hull2# (low# +# value#) (high# +# value#)

{-# INLINE intervalPlusInterval# #-}
intervalPlusInterval# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalPlusInterval# low1# high1# low2# high2# = hull2# (low1# +# low2#) (high1# +# high2#)

{-# INLINE doubleMinusInterval# #-}
doubleMinusInterval# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doubleMinusInterval# value# low# high# = hull2# (value# -# high#) (value# -# low#)

{-# INLINE intervalMinusDouble# #-}
intervalMinusDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalMinusDouble# low# high# value# = hull2# (low# -# value#) (high# -# value#)

{-# INLINE intervalMinusInterval# #-}
intervalMinusInterval# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalMinusInterval# low1# high1# low2# high2# = hull2# (low1# -# high2#) (high1# -# low2#)

{-# INLINE doubleTimesInterval# #-}
doubleTimesInterval# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doubleTimesInterval# value# low# high# = hull2# (value# *# low#) (value# *# high#)

{-# INLINE intervalTimesDouble# #-}
intervalTimesDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalTimesDouble# low# high# value# = hull2# (low# *# value#) (high# *# value#)

{-# INLINE intervalTimesInterval# #-}
intervalTimesInterval# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalTimesInterval# low1# high1# low2# high2# =
  hull4# (low1# *# low2#) (low1# *# high2#) (high1# *# low2#) (high1# *# high2#)

{-# INLINE doubleOverInterval# #-}
doubleOverInterval# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
doubleOverInterval# value# low# high# =
  case (# low# <=# 0.0##, high# >=# 0.0## #) of
    (# 1#, 1# #) -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)
    (# _, _ #) -> hull2# (value# /# low#) (value# /# high#)

{-# INLINE intervalOverDouble# #-}
intervalOverDouble# :: Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalOverDouble# low# high# value# =
  case value# ==# 0.0## of
    1# -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)
    _ -> hull2# (low# /# value#) (high# /# value#)

{-# INLINE intervalOverInterval# #-}
intervalOverInterval# :: Double# -> Double# -> Double# -> Double# -> (# Double#, Double# #)
intervalOverInterval# low1# high1# low2# high2# =
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
  let !(# x1y2Min#, x1y2Max# #) = intervalTimesInterval# x1Min# x1Max# y2Min# y2Max#
  let !(# x2y1Min#, x2y1Max# #) = intervalTimesInterval# x2Min# x2Max# y1Min# y1Max#
  intervalMinusInterval# x1y2Min# x1y2Max# x2y1Min# x2y1Max#

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
          intervalTimesInterval# x1Min# x1Max# x1DetMin# x1DetMax#
    let !(# y1ProdMin#, y1ProdMax# #) =
          intervalTimesInterval# y1Min# y1Max# y1DetMin# y1DetMax#
    let !(# z1ProdMin#, z1ProdMax# #) =
          intervalTimesInterval# z1Min# z1Max# z1DetMin# z1DetMax#
    let !(# dxyMin#, dxyMax# #) =
          intervalMinusInterval# x1ProdMin# x1ProdMax# y1ProdMin# y1ProdMax#
    intervalPlusInterval# dxyMin# dxyMax# z1ProdMin# z1ProdMax#
