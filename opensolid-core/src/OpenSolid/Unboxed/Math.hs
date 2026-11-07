{-# LANGUAGE UnboxedTuples #-}

module OpenSolid.Unboxed.Math
  ( Double#
  , Int#
  , Double (D#)
  , data B#
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
  , equalWithin#
  , (&&#)
  , (||#)
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
  , interpolateFrom#
  , quadraticBezier#
  , cubicBezier#
  , quarticBezier#
  , quinticBezier#
  , intervalSeparation#
  , intervalIntersectsInterval#
  )
where

import GHC.Exts (Double (D#), Double#, Int#)
import GHC.Exts qualified
import OpenSolid.Prelude

{-# INLINE B# #-}
pattern B# :: Int# -> Bool
pattern B# i# <- (bool# -> i#)
 where
  B# 0# = False
  B# _ = True

{-# INLINE bool# #-}
bool# :: Bool -> Int#
bool# False = 0#
bool# True = 1#

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

{-# INLINE equalWithin# #-}
equalWithin# :: Double# -> Double# -> Double# -> Int#
equalWithin# tolerance# x# y# = abs# (x# -# y#) <=# tolerance#

{-# INLINE (&&#) #-}
(&&#) :: Int# -> Int# -> Int#
(&&#) = GHC.Exts.andI#

infixr 3 &&#

{-# INLINE (||#) #-}
(||#) :: Int# -> Int# -> Int#
(||#) = GHC.Exts.orI#

infixr 2 ||#

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
determinantBounds2D# m11Min# m11Max# m21Min# m21Max# m12Min# m12Max# m22Min# m22Max# = do
  let !(# m11m22Min#, m11m22Max# #) = intervalTimesInterval# m11Min# m11Max# m22Min# m22Max#
  let !(# m12m21Min#, m12m21Max# #) = intervalTimesInterval# m12Min# m12Max# m21Min# m21Max#
  intervalMinusInterval# m11m22Min# m11m22Max# m12m21Min# m12m21Max#

{-# INLINEABLE determinantBounds3D# #-}
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
  m11Min#
  m11Max#
  m21Min#
  m21Max#
  m31Min#
  m31Max#
  m12Min#
  m12Max#
  m22Min#
  m22Max#
  m32Min#
  m32Max#
  m13Min#
  m13Max#
  m23Min#
  m23Max#
  m33Min#
  m33Max# = do
    let !(# m11DetMin#, m11DetMax# #) =
          determinantBounds2D# m22Min# m22Max# m32Min# m32Max# m23Min# m23Max# m33Min# m33Max#
    let !(# m21DetMin#, m21DetMax# #) =
          determinantBounds2D# m12Min# m12Max# m32Min# m32Max# m13Min# m13Max# m33Min# m33Max#
    let !(# m31DetMin#, m31DetMax# #) =
          determinantBounds2D# m12Min# m12Max# m22Min# m22Max# m13Min# m13Max# m23Min# m23Max#
    let !(# m11ProdMin#, m11ProdMax# #) =
          intervalTimesInterval# m11Min# m11Max# m11DetMin# m11DetMax#
    let !(# m21ProdMin#, m21ProdMax# #) =
          intervalTimesInterval# m21Min# m21Max# m21DetMin# m21DetMax#
    let !(# m31ProdMin#, m31ProdMax# #) =
          intervalTimesInterval# m31Min# m31Max# m31DetMin# m31DetMax#
    let !(# dxyMin#, dxyMax# #) =
          intervalMinusInterval# m11ProdMin# m11ProdMax# m21ProdMin# m21ProdMax#
    intervalPlusInterval# dxyMin# dxyMax# m31ProdMin# m31ProdMax#

{-# INLINE interpolateFrom# #-}
interpolateFrom# :: Double# -> Double# -> Double# -> Double#
interpolateFrom# start# end# t# = start# +# t# *# (end# -# start#)

{-# INLINEABLE quadraticBezier# #-}
quadraticBezier# :: Double# -> Double# -> Double# -> Double# -> Double#
quadraticBezier# p1# p2# p3# t# = do
  let q1# = interpolateFrom# p1# p2# t#
  let q2# = interpolateFrom# p2# p3# t#
  interpolateFrom# q1# q2# t#

{-# INLINEABLE cubicBezier# #-}
cubicBezier# :: Double# -> Double# -> Double# -> Double# -> Double# -> Double#
cubicBezier# p1# p2# p3# p4# t# = do
  let q1# = interpolateFrom# p1# p2# t#
  let q2# = interpolateFrom# p2# p3# t#
  let q3# = interpolateFrom# p3# p4# t#
  quadraticBezier# q1# q2# q3# t#

{-# INLINEABLE quarticBezier# #-}
quarticBezier# :: Double# -> Double# -> Double# -> Double# -> Double# -> Double# -> Double#
quarticBezier# p1# p2# p3# p4# p5# t# = do
  let q1# = interpolateFrom# p1# p2# t#
  let q2# = interpolateFrom# p2# p3# t#
  let q3# = interpolateFrom# p3# p4# t#
  let q4# = interpolateFrom# p4# p5# t#
  cubicBezier# q1# q2# q3# q4# t#

{-# INLINEABLE quinticBezier# #-}
quinticBezier# ::
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double# ->
  Double#
quinticBezier# p1# p2# p3# p4# p5# p6# t# = do
  let q1# = interpolateFrom# p1# p2# t#
  let q2# = interpolateFrom# p2# p3# t#
  let q3# = interpolateFrom# p3# p4# t#
  let q4# = interpolateFrom# p4# p5# t#
  let q5# = interpolateFrom# p5# p6# t#
  quarticBezier# q1# q2# q3# q4# q5# t#

{-# INLINEABLE intervalSeparation# #-}
intervalSeparation# :: Double# -> Double# -> Double# -> Double# -> Double#
intervalSeparation# low1# high1# low2# high2# = max# (low1# -# high2#) (low2# -# high1#)

{-# INLINEABLE intervalIntersectsInterval# #-}
intervalIntersectsInterval# :: Double# -> Double# -> Double# -> Double# -> Double# -> Int#
intervalIntersectsInterval# tolerance# low1# high1# low2# high2# =
  intervalSeparation# low1# high1# low2# high2# <=# tolerance#
