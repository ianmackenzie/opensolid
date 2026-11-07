{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}

module OpenSolid.Unboxed.Math
  ( Bool# (True#, False#)
  , Double#
  , Int#
  , Double (D#)
  , data B#
  , Interval# (Interval#)
  , Vector2D# (Vector2D#)
  , Vector3D# (Vector3D#)
  , VectorBounds2D# (VectorBounds2D#)
  , VectorBounds3D# (VectorBounds3D#)
  , Matrix2D# (Matrix2D#)
  , Matrix3D# (Matrix3D#)
  , Matrix4D# (Matrix4D#)
  , MatrixBounds2D# (MatrixBounds2D#)
  , MatrixBounds3D# (MatrixBounds3D#)
  , MatrixBounds4D# (MatrixBounds4D#)
  , Negation# (negate#)
  , Addition# ((+#))
  , Subtraction# ((-#))
  , Multiplication# ((*#))
  , Division# ((/#))
  , (==#)
  , (>#)
  , (<#)
  , (>=#)
  , (<=#)
  , equalWithin#
  , (&&#)
  , (||#)
  , Diameter# (diameter#)
  , Exclusion# (exclusion#)
  , inclusion#
  , Separation# (separation#)
  , overlap#
  , Intersects# (intersects#)
  , squared#
  , sqrt#
  , max#
  , min#
  , abs#
  , hypot2#
  , hypot3#
  , hull3#
  , hull4#
  , determinantBounds2D#
  , determinantBounds3D#
  , determinant2D#
  , determinant3D#
  , inverse2D#
  , inverse3D#
  , inverse4D#
  , interpolateFrom#
  , quadraticBezier#
  , cubicBezier#
  , quarticBezier#
  , quinticBezier#
  )
where

import GHC.Exts (Double (D#), Double#, Int#, TYPE)
import GHC.Exts qualified
import OpenSolid.Prelude

----- TYPES -----

newtype Bool# = Bool# Int#

pattern False# :: Bool#
pattern False# = Bool# 0#

pattern True# :: Bool#
pattern True# = Bool# 1#

{-# COMPLETE False#, True# #-}

{-# INLINE B# #-}
pattern B# :: Bool# -> Bool
pattern B# bool# <- (\case False -> False#; True -> True# -> bool#)
 where
  B# (Bool# value#) = GHC.Exts.isTrue# value#

newtype Interval# = Interval_ (# Double#, Double# #)
{-# INLINE Interval# #-}
pattern Interval# :: Double# -> Double# -> Interval#
pattern Interval# low# high# <- Interval_ (# low#, high# #)
 where
  Interval# a# b# =
    Interval_ $ case (# a# <=# b#, b# <=# a# #) of
      (# True#, False# #) -> (# a#, b# #)
      (# False#, True# #) -> (# b#, a# #)
      -- Values are equal so either order would work
      (# True#, True# #) -> (# a#, b# #)
      -- One of a or b must be NaN,
      -- so return an infinite bounds
      -- since we can't guarantee anything
      (# False#, False# #) -> (# -1.0## /# 0.0##, 1.0## /# 0.0## #)

{-# COMPLETE Interval# #-}

newtype Vector2D# = Vector2D_ (# Double#, Double# #)
{-# INLINE Vector2D# #-}
pattern Vector2D# :: Double# -> Double# -> Vector2D#
pattern Vector2D# x# y# = Vector2D_ (# x#, y# #)

{-# COMPLETE Vector2D# #-}

newtype Vector3D# = Vector3D_ (# Double#, Double#, Double# #)
{-# INLINE Vector3D# #-}
pattern Vector3D# :: Double# -> Double# -> Double# -> Vector3D#
pattern Vector3D# r# f# u# = Vector3D_ (# r#, f#, u# #)

{-# COMPLETE Vector3D# #-}

newtype VectorBounds2D# = VectorBounds2D_ (# Interval#, Interval# #)
{-# INLINE VectorBounds2D# #-}
pattern VectorBounds2D# :: Interval# -> Interval# -> VectorBounds2D#
pattern VectorBounds2D# x# y# = VectorBounds2D_ (# x#, y# #)

{-# COMPLETE VectorBounds2D# #-}

newtype VectorBounds3D# = VectorBounds3D_ (# Interval#, Interval#, Interval# #)
{-# INLINE VectorBounds3D# #-}
pattern VectorBounds3D# :: Interval# -> Interval# -> Interval# -> VectorBounds3D#
pattern VectorBounds3D# r# f# u# = VectorBounds3D_ (# r#, f#, u# #)

{-# COMPLETE VectorBounds3D# #-}

type Row2D# = (# Double#, Double# #)

newtype Matrix2D# = Matrix2D_ (# Row2D#, Row2D# #)
{-# INLINE Matrix2D# #-}
pattern Matrix2D# ::
  (# Double#, Double# #) ->
  (# Double#, Double# #) ->
  Matrix2D#
pattern Matrix2D# row1# row2# = Matrix2D_ (# row1#, row2# #)

{-# COMPLETE Matrix2D# #-}

type Row3D# = (# Double#, Double#, Double# #)

newtype Matrix3D# = Matrix3D_ (# Row3D#, Row3D#, Row3D# #)
{-# INLINE Matrix3D# #-}
pattern Matrix3D# ::
  (# Double#, Double#, Double# #) ->
  (# Double#, Double#, Double# #) ->
  (# Double#, Double#, Double# #) ->
  Matrix3D#
pattern Matrix3D# row1# row2# row3# = Matrix3D_ (# row1#, row2#, row3# #)

{-# COMPLETE Matrix3D# #-}

type Row4D# = (# Double#, Double#, Double#, Double# #)

newtype Matrix4D# = Matrix4D_ (# Row4D#, Row4D#, Row4D#, Row4D# #)
{-# INLINE Matrix4D# #-}
pattern Matrix4D# ::
  (# Double#, Double#, Double#, Double# #) ->
  (# Double#, Double#, Double#, Double# #) ->
  (# Double#, Double#, Double#, Double# #) ->
  (# Double#, Double#, Double#, Double# #) ->
  Matrix4D#
pattern Matrix4D# row1# row2# row3# row4# = Matrix4D_ (# row1#, row2#, row3#, row4# #)

{-# COMPLETE Matrix4D# #-}

type RowBounds2D# = (# Interval#, Interval# #)

newtype MatrixBounds2D# = MatrixBounds2D_ (# RowBounds2D#, RowBounds2D# #)
{-# INLINE MatrixBounds2D# #-}
pattern MatrixBounds2D# ::
  (# Interval#, Interval# #) ->
  (# Interval#, Interval# #) ->
  MatrixBounds2D#
pattern MatrixBounds2D# row1# row2# = MatrixBounds2D_ (# row1#, row2# #)

{-# COMPLETE MatrixBounds2D# #-}

type RowBounds3D# = (# Interval#, Interval#, Interval# #)

newtype MatrixBounds3D# = MatrixBounds3D_ (# RowBounds3D#, RowBounds3D#, RowBounds3D# #)
{-# INLINE MatrixBounds3D# #-}
pattern MatrixBounds3D# ::
  (# Interval#, Interval#, Interval# #) ->
  (# Interval#, Interval#, Interval# #) ->
  (# Interval#, Interval#, Interval# #) ->
  MatrixBounds3D#
pattern MatrixBounds3D# row1# row2# row3# = MatrixBounds3D_ (# row1#, row2#, row3# #)

{-# COMPLETE MatrixBounds3D# #-}

type RowBounds4D# = (# Interval#, Interval#, Interval#, Interval# #)

newtype MatrixBounds4D#
  = MatrixBounds4D_ (# RowBounds4D#, RowBounds4D#, RowBounds4D#, RowBounds4D# #)
{-# INLINE MatrixBounds4D# #-}
pattern MatrixBounds4D# ::
  (# Interval#, Interval#, Interval#, Interval# #) ->
  (# Interval#, Interval#, Interval#, Interval# #) ->
  (# Interval#, Interval#, Interval#, Interval# #) ->
  (# Interval#, Interval#, Interval#, Interval# #) ->
  MatrixBounds4D#
pattern MatrixBounds4D# row1# row2# row3# row4# = MatrixBounds4D_ (# row1#, row2#, row3#, row4# #)

{-# COMPLETE MatrixBounds4D# #-}

----- NEGATION -----

class Negation# (a :: TYPE rep) where
  negate# :: a -> a

instance Negation# Double# where
  {-# INLINE negate# #-}
  negate# = GHC.Exts.negateDouble#

instance Negation# Interval# where
  {-# INLINE negate# #-}
  negate# (Interval_ (# low#, high# #)) = Interval_ (# negate# high#, negate# low# #)

instance Negation# Vector2D# where
  {-# INLINE negate# #-}
  negate# (Vector2D# x# y#) = Vector2D# (negate# x#) (negate# y#)

instance Negation# Vector3D# where
  {-# INLINE negate# #-}
  negate# (Vector3D# r# f# u#) = Vector3D# (negate# r#) (negate# f#) (negate# u#)

instance Negation# VectorBounds2D# where
  {-# INLINE negate# #-}
  negate# (VectorBounds2D# x# y#) = VectorBounds2D# (negate# x#) (negate# y#)

instance Negation# VectorBounds3D# where
  {-# INLINE negate# #-}
  negate# (VectorBounds3D# r# f# u#) = VectorBounds3D# (negate# r#) (negate# f#) (negate# u#)

----- ADDITION -----

class
  Addition# (lhs :: TYPE lhsRep) (rhs :: TYPE rhsRep) (result :: TYPE resultRep)
    | lhs rhs -> result
  where
  (+#) :: lhs -> rhs -> result

infixl 6 +#

instance Addition# Double# Double# Double# where
  {-# INLINE (+#) #-}
  (+#) = (GHC.Exts.+##)

instance Addition# Double# Interval# Interval# where
  {-# INLINE (+#) #-}
  value# +# Interval# low# high# = Interval# (value# +# low#) (value# +# high#)

instance Addition# Interval# Double# Interval# where
  {-# INLINE (+#) #-}
  Interval# low# high# +# value# = Interval# (low# +# value#) (high# +# value#)

instance Addition# Interval# Interval# Interval# where
  {-# INLINE (+#) #-}
  Interval# low1# high1# +# Interval# low2# high2# = Interval# (low1# +# low2#) (high1# +# high2#)

instance Addition# Vector2D# Vector2D# Vector2D# where
  {-# INLINE (+#) #-}
  Vector2D# x1# y1# +# Vector2D# x2# y2# = Vector2D# (x1# +# x2#) (y1# +# y2#)

instance Addition# Vector2D# VectorBounds2D# VectorBounds2D# where
  {-# INLINE (+#) #-}
  Vector2D# x1# y1# +# VectorBounds2D# x2# y2# = VectorBounds2D# (x1# +# x2#) (y1# +# y2#)

instance Addition# VectorBounds2D# Vector2D# VectorBounds2D# where
  {-# INLINE (+#) #-}
  VectorBounds2D# x1# y1# +# Vector2D# x2# y2# = VectorBounds2D# (x1# +# x2#) (y1# +# y2#)

instance Addition# VectorBounds2D# VectorBounds2D# VectorBounds2D# where
  {-# INLINE (+#) #-}
  VectorBounds2D# x1# y1# +# VectorBounds2D# x2# y2# = VectorBounds2D# (x1# +# x2#) (y1# +# y2#)

instance Addition# Vector3D# Vector3D# Vector3D# where
  {-# INLINE (+#) #-}
  Vector3D# r1# f1# u1# +# Vector3D# r2# f2# u2# =
    Vector3D# (r1# +# r2#) (f1# +# f2#) (u1# +# u2#)

instance Addition# Vector3D# VectorBounds3D# VectorBounds3D# where
  {-# INLINE (+#) #-}
  Vector3D# r1# f1# u1# +# VectorBounds3D# r2# f2# u2# =
    VectorBounds3D# (r1# +# r2#) (f1# +# f2#) (u1# +# u2#)

instance Addition# VectorBounds3D# Vector3D# VectorBounds3D# where
  {-# INLINE (+#) #-}
  VectorBounds3D# r1# f1# u1# +# Vector3D# r2# f2# u2# =
    VectorBounds3D# (r1# +# r2#) (f1# +# f2#) (u1# +# u2#)

instance Addition# VectorBounds3D# VectorBounds3D# VectorBounds3D# where
  {-# INLINE (+#) #-}
  VectorBounds3D# r1# f1# u1# +# VectorBounds3D# r2# f2# u2# =
    VectorBounds3D# (r1# +# r2#) (f1# +# f2#) (u1# +# u2#)

----- SUBTRACTION -----

class
  Subtraction# (lhs :: TYPE lhsRep) (rhs :: TYPE rhsRep) (result :: TYPE resultRep)
    | lhs rhs -> result
  where
  (-#) :: lhs -> rhs -> result

infixl 6 -#

instance Subtraction# Double# Double# Double# where
  {-# INLINE (-#) #-}
  (-#) = (GHC.Exts.-##)

instance Subtraction# Double# Interval# Interval# where
  {-# INLINE (-#) #-}
  value# -# Interval# low# high# = Interval# (value# -# high#) (value# -# low#)

instance Subtraction# Interval# Double# Interval# where
  {-# INLINE (-#) #-}
  Interval# low# high# -# value# = Interval# (low# -# value#) (high# -# value#)

instance Subtraction# Interval# Interval# Interval# where
  {-# INLINE (-#) #-}
  Interval# low1# high1# -# Interval# low2# high2# = Interval# (low1# -# high2#) (high1# -# low2#)

instance Subtraction# Vector2D# Vector2D# Vector2D# where
  {-# INLINE (-#) #-}
  Vector2D# x1# y1# -# Vector2D# x2# y2# = Vector2D# (x1# -# x2#) (y1# -# y2#)

instance Subtraction# Vector2D# VectorBounds2D# VectorBounds2D# where
  {-# INLINE (-#) #-}
  Vector2D# x1# y1# -# VectorBounds2D# x2# y2# = VectorBounds2D# (x1# -# x2#) (y1# -# y2#)

instance Subtraction# VectorBounds2D# Vector2D# VectorBounds2D# where
  {-# INLINE (-#) #-}
  VectorBounds2D# x1# y1# -# Vector2D# x2# y2# = VectorBounds2D# (x1# -# x2#) (y1# -# y2#)

instance Subtraction# VectorBounds2D# VectorBounds2D# VectorBounds2D# where
  {-# INLINE (-#) #-}
  VectorBounds2D# x1# y1# -# VectorBounds2D# x2# y2# = VectorBounds2D# (x1# -# x2#) (y1# -# y2#)

instance Subtraction# Vector3D# Vector3D# Vector3D# where
  {-# INLINE (-#) #-}
  Vector3D# r1# f1# u1# -# Vector3D# r2# f2# u2# =
    Vector3D# (r1# -# r2#) (f1# -# f2#) (u1# -# u2#)

instance Subtraction# Vector3D# VectorBounds3D# VectorBounds3D# where
  {-# INLINE (-#) #-}
  Vector3D# r1# f1# u1# -# VectorBounds3D# r2# f2# u2# =
    VectorBounds3D# (r1# -# r2#) (f1# -# f2#) (u1# -# u2#)

instance Subtraction# VectorBounds3D# Vector3D# VectorBounds3D# where
  {-# INLINE (-#) #-}
  VectorBounds3D# r1# f1# u1# -# Vector3D# r2# f2# u2# =
    VectorBounds3D# (r1# -# r2#) (f1# -# f2#) (u1# -# u2#)

instance Subtraction# VectorBounds3D# VectorBounds3D# VectorBounds3D# where
  {-# INLINE (-#) #-}
  VectorBounds3D# r1# f1# u1# -# VectorBounds3D# r2# f2# u2# =
    VectorBounds3D# (r1# -# r2#) (f1# -# f2#) (u1# -# u2#)

----- MULTIPLICATION -----

class
  Multiplication# (lhs :: TYPE lhsRep) (rhs :: TYPE rhsRep) (result :: TYPE resultRep)
    | lhs rhs -> result
  where
  (*#) :: lhs -> rhs -> result

infixl 7 *#

instance Multiplication# Double# Double# Double# where
  {-# INLINE (*#) #-}
  (*#) = (GHC.Exts.*##)

instance Multiplication# Double# Interval# Interval# where
  {-# INLINE (*#) #-}
  value# *# Interval# low# high# = Interval# (value# *# low#) (value# *# high#)

instance Multiplication# Interval# Double# Interval# where
  {-# INLINE (*#) #-}
  Interval# low# high# *# value# = Interval# (low# *# value#) (high# *# value#)

instance Multiplication# Interval# Interval# Interval# where
  {-# INLINE (*#) #-}
  Interval# low1# high1# *# Interval# low2# high2# =
    hull4# (low1# *# low2#) (low1# *# high2#) (high1# *# low2#) (high1# *# high2#)

instance Multiplication# Double# Vector2D# Vector2D# where
  {-# INLINE (*#) #-}
  value# *# Vector2D# x# y# = Vector2D# (value# *# x#) (value# *# y#)

instance Multiplication# Vector2D# Double# Vector2D# where
  {-# INLINE (*#) #-}
  Vector2D# x# y# *# value# = Vector2D# (x# *# value#) (y# *# value#)

instance Multiplication# Interval# Vector2D# VectorBounds2D# where
  {-# INLINE (*#) #-}
  interval# *# Vector2D# x# y# = VectorBounds2D# (interval# *# x#) (interval# *# y#)

instance Multiplication# Vector2D# Interval# VectorBounds2D# where
  {-# INLINE (*#) #-}
  Vector2D# x# y# *# interval# = VectorBounds2D# (x# *# interval#) (y# *# interval#)

instance Multiplication# Double# VectorBounds2D# VectorBounds2D# where
  {-# INLINE (*#) #-}
  value# *# VectorBounds2D# x# y# = VectorBounds2D# (value# *# x#) (value# *# y#)

instance Multiplication# VectorBounds2D# Double# VectorBounds2D# where
  {-# INLINE (*#) #-}
  VectorBounds2D# x# y# *# value# = VectorBounds2D# (x# *# value#) (y# *# value#)

instance Multiplication# Interval# VectorBounds2D# VectorBounds2D# where
  {-# INLINE (*#) #-}
  interval# *# VectorBounds2D# x# y# = VectorBounds2D# (interval# *# x#) (interval# *# y#)

instance Multiplication# VectorBounds2D# Interval# VectorBounds2D# where
  {-# INLINE (*#) #-}
  VectorBounds2D# x# y# *# interval# = VectorBounds2D# (x# *# interval#) (y# *# interval#)

instance Multiplication# Double# Vector3D# Vector3D# where
  {-# INLINE (*#) #-}
  value# *# Vector3D# r# f# u# =
    Vector3D# (value# *# r#) (value# *# f#) (value# *# u#)

instance Multiplication# Vector3D# Double# Vector3D# where
  {-# INLINE (*#) #-}
  Vector3D# r# f# u# *# value# =
    Vector3D# (r# *# value#) (f# *# value#) (u# *# value#)

instance Multiplication# Interval# Vector3D# VectorBounds3D# where
  {-# INLINE (*#) #-}
  interval# *# Vector3D# r# f# u# =
    VectorBounds3D# (interval# *# r#) (interval# *# f#) (interval# *# u#)

instance Multiplication# Vector3D# Interval# VectorBounds3D# where
  {-# INLINE (*#) #-}
  Vector3D# r# f# u# *# interval# =
    VectorBounds3D# (r# *# interval#) (f# *# interval#) (u# *# interval#)

instance Multiplication# Double# VectorBounds3D# VectorBounds3D# where
  {-# INLINE (*#) #-}
  value# *# VectorBounds3D# r# f# u# =
    VectorBounds3D# (value# *# r#) (value# *# f#) (value# *# u#)

instance Multiplication# VectorBounds3D# Double# VectorBounds3D# where
  {-# INLINE (*#) #-}
  VectorBounds3D# r# f# u# *# value# =
    VectorBounds3D# (r# *# value#) (f# *# value#) (u# *# value#)

instance Multiplication# Interval# VectorBounds3D# VectorBounds3D# where
  {-# INLINE (*#) #-}
  interval# *# VectorBounds3D# r# f# u# =
    VectorBounds3D# (interval# *# r#) (interval# *# f#) (interval# *# u#)

instance Multiplication# VectorBounds3D# Interval# VectorBounds3D# where
  {-# INLINE (*#) #-}
  VectorBounds3D# r# f# u# *# interval# =
    VectorBounds3D# (r# *# interval#) (f# *# interval#) (u# *# interval#)

instance Multiplication# Double# Matrix2D# Matrix2D# where
  {-# INLINE (*#) #-}
  value#
    *# Matrix2D#
         (# m11#, m12# #)
         (# m21#, m22# #) =
      Matrix2D#
        (# value# *# m11#, value# *# m12# #)
        (# value# *# m21#, value# *# m22# #)

instance Multiplication# Matrix2D# Double# Matrix2D# where
  {-# INLINE (*#) #-}
  matrix# *# value# = value# *# matrix#

instance Multiplication# Double# Matrix3D# Matrix3D# where
  {-# INLINE (*#) #-}
  value#
    *# Matrix3D#
         (# m11#, m12#, m13# #)
         (# m21#, m22#, m23# #)
         (# m31#, m32#, m33# #) =
      Matrix3D#
        (# value# *# m11#, value# *# m12#, value# *# m13# #)
        (# value# *# m21#, value# *# m22#, value# *# m23# #)
        (# value# *# m31#, value# *# m32#, value# *# m33# #)

instance Multiplication# Matrix3D# Double# Matrix3D# where
  {-# INLINE (*#) #-}
  matrix# *# value# = value# *# matrix#

instance Multiplication# Double# Matrix4D# Matrix4D# where
  {-# INLINE (*#) #-}
  value#
    *# Matrix4D#
         (# m11#, m12#, m13#, m14# #)
         (# m21#, m22#, m23#, m24# #)
         (# m31#, m32#, m33#, m34# #)
         (# m41#, m42#, m43#, m44# #) =
      Matrix4D#
        (# value# *# m11#, value# *# m12#, value# *# m13#, value# *# m14# #)
        (# value# *# m21#, value# *# m22#, value# *# m23#, value# *# m24# #)
        (# value# *# m31#, value# *# m32#, value# *# m33#, value# *# m34# #)
        (# value# *# m41#, value# *# m42#, value# *# m43#, value# *# m44# #)

instance Multiplication# Matrix4D# Double# Matrix4D# where
  {-# INLINE (*#) #-}
  matrix# *# value# = value# *# matrix#

----- DIVISION -----

class
  Division# (lhs :: TYPE lhsRep) (rhs :: TYPE rhsRep) (result :: TYPE resultRep)
    | lhs rhs -> result
  where
  (/#) :: lhs -> rhs -> result

infixl 7 /#

instance Division# Double# Double# Double# where
  {-# INLINE (/#) #-}
  (/#) = (GHC.Exts./##)

instance Division# Double# Interval# Interval# where
  {-# INLINE (/#) #-}
  value# /# Interval# low# high# =
    case (# low# <=# 0.0##, high# >=# 0.0## #) of
      (# True#, True# #) -> Interval# (-1.0## /# 0.0##) (1.0## /# 0.0##)
      (# _, _ #) -> Interval# (value# /# low#) (value# /# high#)

instance Division# Interval# Double# Interval# where
  {-# INLINE (/#) #-}
  Interval# low# high# /# value# =
    case value# ==# 0.0## of
      True# -> Interval# (-1.0## /# 0.0##) (1.0## /# 0.0##)
      False# -> Interval# (low# /# value#) (high# /# value#)

instance Division# Interval# Interval# Interval# where
  {-# INLINE (/#) #-}
  Interval# low1# high1# /# Interval# low2# high2# =
    case (# low2# <=# 0.0##, high2# >=# 0.0## #) of
      (# True#, True# #) -> Interval# (-1.0## /# 0.0##) (1.0## /# 0.0##)
      (# _, _ #) -> hull4# (low1# /# low2#) (low1# /# high2#) (high1# /# low2#) (high1# /# high2#)

instance Division# Vector2D# Double# Vector2D# where
  {-# INLINE (/#) #-}
  Vector2D# x# y# /# value# = Vector2D# (x# /# value#) (y# /# value#)

instance Division# Vector3D# Double# Vector3D# where
  {-# INLINE (/#) #-}
  Vector3D# r# f# u# /# value# = Vector3D# (r# /# value#) (f# /# value#) (u# /# value#)

instance Division# Vector2D# Interval# VectorBounds2D# where
  {-# INLINE (/#) #-}
  Vector2D# x# y# /# interval# = VectorBounds2D# (x# /# interval#) (y# /# interval#)

instance Division# Vector3D# Interval# VectorBounds3D# where
  {-# INLINE (/#) #-}
  Vector3D# r# f# u# /# interval# =
    VectorBounds3D# (r# /# interval#) (f# /# interval#) (u# /# interval#)

instance Division# VectorBounds2D# Double# VectorBounds2D# where
  {-# INLINE (/#) #-}
  VectorBounds2D# x# y# /# value# = VectorBounds2D# (x# /# value#) (y# /# value#)

instance Division# VectorBounds3D# Double# VectorBounds3D# where
  {-# INLINE (/#) #-}
  VectorBounds3D# r# f# u# /# value# = VectorBounds3D# (r# /# value#) (f# /# value#) (u# /# value#)

instance Division# VectorBounds2D# Interval# VectorBounds2D# where
  {-# INLINE (/#) #-}
  VectorBounds2D# x# y# /# interval# = VectorBounds2D# (x# /# interval#) (y# /# interval#)

instance Division# VectorBounds3D# Interval# VectorBounds3D# where
  {-# INLINE (/#) #-}
  VectorBounds3D# r# f# u# /# interval# =
    VectorBounds3D# (r# /# interval#) (f# /# interval#) (u# /# interval#)

instance Division# Matrix2D# Double# Matrix2D# where
  {-# INLINE (/#) #-}
  matrix# /# value# = (1.0## /# value#) *# matrix#

instance Division# Matrix3D# Double# Matrix3D# where
  {-# INLINE (/#) #-}
  matrix# /# value# = (1.0## /# value#) *# matrix#

instance Division# Matrix4D# Double# Matrix4D# where
  {-# INLINE (/#) #-}
  matrix# /# value# = (1.0## /# value#) *# matrix#

----- COMPARISON -----

class Eq# (a :: TYPE rep) where
  (==#) :: a -> a -> Bool#

instance Eq# Double# where
  {-# INLINE (==#) #-}
  first# ==# second# = Bool# (first# GHC.Exts.==## second#)

infix 4 ==#

{-# INLINE (>#) #-}
(>#) :: Double# -> Double# -> Bool#
first# ># second# = Bool# (first# GHC.Exts.>## second#)

infix 4 >#

{-# INLINE (<#) #-}
(<#) :: Double# -> Double# -> Bool#
first# <# second# = Bool# (first# GHC.Exts.<## second#)

infix 4 <#

{-# INLINE (>=#) #-}
(>=#) :: Double# -> Double# -> Bool#
first# >=# second# = Bool# (first# GHC.Exts.>=## second#)

infix 4 >=#

{-# INLINE (<=#) #-}
(<=#) :: Double# -> Double# -> Bool#
first# <=# second# = Bool# (first# GHC.Exts.<=## second#)

infix 4 <=#

{-# INLINE equalWithin# #-}
equalWithin# :: Double# -> Double# -> Double# -> Bool#
equalWithin# tolerance# x# y# = abs# (x# -# y#) <=# tolerance#

----- BOOLEAN OPERATIONS -----

{-# INLINE (&&#) #-}
(&&#) :: Bool# -> Bool# -> Bool#
Bool# first# &&# Bool# second# = Bool# (GHC.Exts.andI# first# second#)

infixr 3 &&#

{-# INLINE (||#) #-}
(||#) :: Bool# -> Bool# -> Bool#
Bool# first# ||# Bool# second# = Bool# (GHC.Exts.orI# first# second#)

infixr 2 ||#

----- DIAMETER -----

class Diameter# (a :: TYPE rep) where
  diameter# :: a -> Double#

instance Diameter# Interval# where
  {-# INLINE diameter# #-}
  diameter# (Interval# low# high#) = high# -# low#

instance Diameter# VectorBounds2D# where
  {-# INLINE diameter# #-}
  diameter# (VectorBounds2D# x# y#) = hypot2# (diameter# x#) (diameter# y#)

----- EXCLUSION -----

class Exclusion# (a :: TYPE aRep) (b :: TYPE bRep) | b -> a where
  exclusion# :: a -> b -> Double#

  -- The implementation of inclusion# should always just be the negation of exclusion#,
  -- but it doesn't seem to be possible to write a single default implementation of inclusion#
  -- since it would have to work with multiple different representation types
  inclusion# :: a -> b -> Double#

instance Exclusion# Double# Interval# where
  {-# INLINE exclusion# #-}
  exclusion# value# (Interval# low# high#) = max# (low# -# value#) (value# -# high#)
  {-# INLINE inclusion# #-}
  inclusion# value# interval# = negate# (exclusion# value# interval#)

instance Exclusion# Vector2D# VectorBounds2D# where
  {-# INLINEABLE exclusion# #-}
  exclusion# (Vector2D# vx# vy#) (VectorBounds2D# bx# by#) = do
    let exclusionX# = exclusion# vx# bx#
    let exclusionY# = exclusion# vy# by#
    let positiveX# = exclusionX# ># 0.0##
    let positiveY# = exclusionY# ># 0.0##
    case (# positiveX#, positiveY# #) of
      (# True#, True# #) -> hypot2# exclusionX# exclusionY#
      (# True#, False# #) -> exclusionX#
      (# False#, True# #) -> exclusionY#
      (# False#, False# #) -> max# exclusionX# exclusionY#
  {-# INLINE inclusion# #-}
  inclusion# vector# bounds# = negate# (exclusion# vector# bounds#)

----- SEPARATION -----

class Separation# (a :: TYPE rep) where
  separation# :: a -> a -> Double#

  -- The implementation of overlap# should always just be the negation of separation#,
  -- but it doesn't seem to be possible to write a single default implementation of overlap#
  -- since it would have to work with multiple different representation types
  overlap# :: a -> a -> Double#

instance Separation# Interval# where
  {-# INLINE separation# #-}
  separation# (Interval# low1# high1#) (Interval# low2# high2#) =
    max# (low1# -# high2#) (low2# -# high1#)
  {-# INLINE overlap# #-}
  overlap# first# second# = negate# (separation# first# second#)

instance Separation# VectorBounds2D# where
  {-# INLINEABLE separation# #-}
  separation# (VectorBounds2D# x1# y1#) (VectorBounds2D# x2# y2#) = do
    let separationX# = separation# x1# x2#
    let separationY# = separation# y1# y2#
    let positiveX# = separationX# ># 0.0##
    let positiveY# = separationY# ># 0.0##
    case (# positiveX#, positiveY# #) of
      (# True#, True# #) -> hypot2# separationX# separationY#
      (# True#, False# #) -> separationX#
      (# False#, True# #) -> separationY#
      (# False#, False# #) -> max# separationX# separationY#
  {-# INLINE overlap# #-}
  overlap# first# second# = negate# (separation# first# second#)

----- INTERSECTS -----

class Intersects# (a :: TYPE aRep) (b :: TYPE bRep) where
  intersects# :: Double# -> a -> b -> Bool#

instance Intersects# Double# Interval# where
  {-# INLINE intersects# #-}
  intersects# tolerance# value# interval# =
    exclusion# value# interval# <=# tolerance#

instance Intersects# Interval# Double# where
  {-# INLINE intersects# #-}
  intersects# tolerance# interval# value# =
    intersects# tolerance# value# interval#

instance Intersects# Interval# Interval# where
  {-# INLINE intersects# #-}
  intersects# tolerance# interval1# interval2# =
    separation# interval1# interval2# <=# tolerance#

instance Intersects# Vector2D# VectorBounds2D# where
  {-# INLINE intersects# #-}
  intersects# tolerance# (Vector2D# vx# vy#) (VectorBounds2D# bx# by#) =
    intersects# tolerance# vx# bx# &&# intersects# tolerance# vy# by#

instance Intersects# VectorBounds2D# Vector2D# where
  {-# INLINE intersects# #-}
  intersects# tolerance# vectorBounds# vector# =
    intersects# tolerance# vector# vectorBounds#

instance Intersects# VectorBounds2D# VectorBounds2D# where
  {-# INLINE intersects# #-}
  intersects# tolerance# (VectorBounds2D# x1# y1#) (VectorBounds2D# x2# y2#) =
    intersects# tolerance# x1# x2# &&# intersects# tolerance# y1# y2#

instance Intersects# Vector3D# VectorBounds3D# where
  {-# INLINE intersects# #-}
  intersects# tolerance# (Vector3D# vr# vf# vu#) (VectorBounds3D# br# bf# bu#) =
    intersects# tolerance# vr# br#
      &&# intersects# tolerance# vf# bf#
      &&# intersects# tolerance# vu# bu#

instance Intersects# VectorBounds3D# Vector3D# where
  {-# INLINE intersects# #-}
  intersects# tolerance# vectorBounds# vector# =
    intersects# tolerance# vector# vectorBounds#

instance Intersects# VectorBounds3D# VectorBounds3D# where
  {-# INLINE intersects# #-}
  intersects# tolerance# (VectorBounds3D# r1# f1# u1#) (VectorBounds3D# r2# f2# u2#) =
    intersects# tolerance# r1# r2#
      &&# intersects# tolerance# f1# f2#
      &&# intersects# tolerance# u1# u2#

----- FUNCTIONS -----

{-# INLINE squared# #-}
squared# :: Double# -> Double#
squared# value# = value# *# value#

{-# INLINE sqrt# #-}
sqrt# :: Double# -> Double#
sqrt# = GHC.Exts.sqrtDouble#

{-# INLINE max# #-}
max# :: Double# -> Double# -> Double#
max# a# b# =
  case (# a# >=# b#, b# >=# a# #) of
    (# True#, False# #) -> a#
    (# False#, True# #) -> b#
    -- Values are equal, so can return either
    (# True#, True# #) -> a#
    -- One of a# or b# is NaN, so return NaN
    (# False#, False# #) -> 0.0## /# 0.0##

{-# INLINE min# #-}
min# :: Double# -> Double# -> Double#
min# a# b# =
  case (# a# <=# b#, b# <=# a# #) of
    (# True#, False# #) -> a#
    (# False#, True# #) -> b#
    -- Values are equal, so can return either
    (# True#, True# #) -> a#
    -- One of a# or b# is NaN, so return NaN
    (# False#, False# #) -> 0.0## /# 0.0##

{-# INLINE abs# #-}
abs# :: Double# -> Double#
abs# = GHC.Exts.fabsDouble#

{-# INLINE hypot2# #-}
hypot2# :: Double# -> Double# -> Double#
hypot2# x# y# = sqrt# (x# *# x# +# y# *# y#)

{-# INLINE hypot3# #-}
hypot3# :: Double# -> Double# -> Double# -> Double#
hypot3# x# y# z# = sqrt# (x# *# x# +# y# *# y# +# z# *# z#)

{-# INLINE hull3# #-}
hull3# :: Double# -> Double# -> Double# -> Interval#
hull3# a# b# c# =
  Interval# (a# `min#` b# `min#` c#) (a# `max#` b# `max#` c#)

{-# INLINE hull4# #-}
hull4# :: Double# -> Double# -> Double# -> Double# -> Interval#
hull4# a# b# c# d# =
  Interval# (a# `min#` b# `min#` c# `min#` d#) (a# `max#` b# `max#` c# `max#` d#)

{-# INLINE determinant2D# #-}
determinant2D# :: Matrix2D# -> Double#
determinant2D# matrix# = do
  let !( Matrix2D#
           (# m11#, m12# #)
           (# m21#, m22# #)
         ) = matrix#
  m11# *# m22# -# m12# *# m21#

{-# INLINE determinantBounds2D# #-}
determinantBounds2D# :: MatrixBounds2D# -> Interval#
determinantBounds2D# matrixBounds# = do
  let !( MatrixBounds2D#
           (# m11#, m12# #)
           (# m21#, m22# #)
         ) = matrixBounds#
  m11# *# m22# -# m12# *# m21#

{-# INLINE determinant3D# #-}
determinant3D# :: Matrix3D# -> Double#
determinant3D# matrix# = do
  let !( Matrix3D#
           (# m11#, m12#, m13# #)
           (# m21#, m22#, m23# #)
           (# m31#, m32#, m33# #)
         ) = matrix#
  let cofactor11# =
        determinant2D# $
          Matrix2D#
            (# m22#, m23# #)
            (# m32#, m33# #)
  let cofactor21# =
        negate# $
          determinant2D# $
            Matrix2D#
              (# m12#, m13# #)
              (# m32#, m33# #)
  let cofactor31# =
        determinant2D# $
          Matrix2D#
            (# m12#, m13# #)
            (# m22#, m23# #)
  m11# *# cofactor11# +# m21# *# cofactor21# +# m31# *# cofactor31#

{-# INLINEABLE determinantBounds3D# #-}
determinantBounds3D# :: MatrixBounds3D# -> Interval#
determinantBounds3D# matrixBounds# = do
  let !( MatrixBounds3D#
           (# m11#, m12#, m13# #)
           (# m21#, m22#, m23# #)
           (# m31#, m32#, m33# #)
         ) = matrixBounds#
  let cofactor11# =
        determinantBounds2D# $
          MatrixBounds2D#
            (# m22#, m23# #)
            (# m32#, m33# #)
  let cofactor21# =
        negate# $
          determinantBounds2D# $
            MatrixBounds2D#
              (# m12#, m13# #)
              (# m32#, m33# #)
  let cofactor31# =
        determinantBounds2D# $
          MatrixBounds2D#
            (# m12#, m13# #)
            (# m22#, m23# #)
  m11# *# cofactor11# +# m21# *# cofactor21# +# m31# *# cofactor31#

{-# INLINE inverse2D# #-}
inverse2D# :: Matrix2D# -> Matrix2D#
inverse2D# matrix# = do
  let !( Matrix2D#
           (# m11#, m12# #)
           (# m21#, m22# #)
         ) = matrix#
  Matrix2D#
    (# m22#, negate# m12# #)
    (# negate# m21#, m11# #)
    /# determinant2D# matrix#

{-# INLINEABLE inverse3D# #-}
inverse3D# :: Matrix3D# -> Matrix3D#
inverse3D# matrix# = do
  let !( Matrix3D#
           (# m11#, m12#, m13# #)
           (# m21#, m22#, m23# #)
           (# m31#, m32#, m33# #)
         ) = matrix#
  let cofactor11# =
        determinant2D# $
          Matrix2D#
            (# m22#, m23# #)
            (# m32#, m33# #)
  let cofactor12# =
        negate# $
          determinant2D# $
            Matrix2D#
              (# m21#, m23# #)
              (# m31#, m33# #)
  let cofactor13# =
        determinant2D# $
          Matrix2D#
            (# m21#, m22# #)
            (# m31#, m32# #)
  let cofactor21# =
        negate# $
          determinant2D# $
            Matrix2D#
              (# m12#, m13# #)
              (# m32#, m33# #)
  let cofactor22# =
        determinant2D# $
          Matrix2D#
            (# m11#, m13# #)
            (# m31#, m33# #)
  let cofactor23# =
        negate# $
          determinant2D# $
            Matrix2D#
              (# m11#, m12# #)
              (# m31#, m32# #)
  let cofactor31# =
        determinant2D# $
          Matrix2D#
            (# m12#, m13# #)
            (# m22#, m23# #)
  let cofactor32# =
        negate# $
          determinant2D# $
            Matrix2D#
              (# m11#, m13# #)
              (# m21#, m23# #)
  let cofactor33# =
        determinant2D# $
          Matrix2D#
            (# m11#, m12# #)
            (# m21#, m22# #)
  -- Expansion along first column
  let determinant# = m11# *# cofactor11# +# m21# *# cofactor21# +# m31# *# cofactor31#
  -- Inverse is transpose of cofactor matrix divided by determinant
  Matrix3D#
    (# cofactor11#, cofactor21#, cofactor31# #)
    (# cofactor12#, cofactor22#, cofactor32# #)
    (# cofactor13#, cofactor23#, cofactor33# #)
    /# determinant#

{-# INLINEABLE inverse4D# #-}
inverse4D# :: Matrix4D# -> Matrix4D#
inverse4D# matrix# = do
  let !( Matrix4D#
           (# m11#, m12#, m13#, m14# #)
           (# m21#, m22#, m23#, m24# #)
           (# m31#, m32#, m33#, m34# #)
           (# m41#, m42#, m43#, m44# #)
         ) = matrix#
  -- Shared 2x2 minors from rows 1 and 2 (upperRC#) and rows 3 and 4 (lowerRC#)
  let minor2D# row1# row2# = determinant2D# (Matrix2D# row1# row2#)
  let upper12# =
        minor2D#
          (# m11#, m12# #)
          (# m21#, m22# #)
  let upper13# =
        minor2D#
          (# m11#, m13# #)
          (# m21#, m23# #)
  let upper14# =
        minor2D#
          (# m11#, m14# #)
          (# m21#, m24# #)
  let upper23# =
        minor2D#
          (# m12#, m13# #)
          (# m22#, m23# #)
  let upper24# =
        minor2D#
          (# m12#, m14# #)
          (# m22#, m24# #)
  let upper34# =
        minor2D#
          (# m13#, m14# #)
          (# m23#, m24# #)
  let lower12# =
        minor2D#
          (# m31#, m32# #)
          (# m41#, m42# #)
  let lower13# =
        minor2D#
          (# m31#, m33# #)
          (# m41#, m43# #)
  let lower14# =
        minor2D#
          (# m31#, m34# #)
          (# m41#, m44# #)
  let lower23# =
        minor2D#
          (# m32#, m33# #)
          (# m42#, m43# #)
  let lower24# =
        minor2D#
          (# m32#, m34# #)
          (# m42#, m44# #)
  let lower34# =
        minor2D#
          (# m33#, m34# #)
          (# m43#, m44# #)
  -- Cofactors for rows 1 and 2 are computed by expanding the corresponding
  -- 3x3 minor along its remaining top-row (row 1 or 2) entries using lowerRC#,
  -- and cofactors for rows 3 and 4 similarly using upperRC#
  let cofactor11# = m22# *# lower34# -# m23# *# lower24# +# m24# *# lower23#
  let cofactor12# = m23# *# lower14# -# m21# *# lower34# -# m24# *# lower13#
  let cofactor13# = m21# *# lower24# -# m22# *# lower14# +# m24# *# lower12#
  let cofactor14# = m22# *# lower13# -# m21# *# lower23# -# m23# *# lower12#
  let cofactor21# = m13# *# lower24# -# m12# *# lower34# -# m14# *# lower23#
  let cofactor22# = m11# *# lower34# -# m13# *# lower14# +# m14# *# lower13#
  let cofactor23# = m12# *# lower14# -# m11# *# lower24# -# m14# *# lower12#
  let cofactor24# = m11# *# lower23# -# m12# *# lower13# +# m13# *# lower12#
  let cofactor31# = m42# *# upper34# -# m43# *# upper24# +# m44# *# upper23#
  let cofactor32# = m43# *# upper14# -# m41# *# upper34# -# m44# *# upper13#
  let cofactor33# = m41# *# upper24# -# m42# *# upper14# +# m44# *# upper12#
  let cofactor34# = m42# *# upper13# -# m41# *# upper23# -# m43# *# upper12#
  let cofactor41# = m33# *# upper24# -# m32# *# upper34# -# m34# *# upper23#
  let cofactor42# = m31# *# upper34# -# m33# *# upper14# +# m34# *# upper13#
  let cofactor43# = m32# *# upper14# -# m31# *# upper24# -# m34# *# upper12#
  let cofactor44# = m31# *# upper23# -# m32# *# upper13# +# m33# *# upper12#
  -- Expansion along first column
  let determinant# =
        m11# *# cofactor11# +# m21# *# cofactor21# +# m31# *# cofactor31# +# m41# *# cofactor41#
  -- Inverse is transpose of cofactor matrix divided by determinant
  Matrix4D#
    (# cofactor11#, cofactor21#, cofactor31#, cofactor41# #)
    (# cofactor12#, cofactor22#, cofactor32#, cofactor42# #)
    (# cofactor13#, cofactor23#, cofactor33#, cofactor43# #)
    (# cofactor14#, cofactor24#, cofactor34#, cofactor44# #)
    /# determinant#

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
