{-# LANGUAGE UnboxedTuples #-}

module Tests.Unboxed.Math (tests) where

import OpenSolid.Prelude
import OpenSolid.Random qualified as Random
import OpenSolid.Unboxed.Math
import Test (Test, (~~))
import Test qualified

tests :: List Test
tests =
  [ inverse2D
  , inverse3D
  ]

matrixComponent :: Random.Generator Number
matrixComponent = Random.number -2.0 2.0

inverse2D :: Test
inverse2D = Test.check 100 "inverse2D" do
  N# m11# <- Test.generate matrixComponent
  N# m21# <- Test.generate matrixComponent
  N# m12# <- Test.generate matrixComponent
  N# m22# <- Test.generate matrixComponent
  let matrix# =
        Matrix2D#
          (# m11#, m12# #)
          (# m21#, m22# #)

  let !( Matrix2D#
           (# n11#, n12# #)
           (# n21#, n22# #)
         ) = inverse2D# matrix#
  Test.all
    [ Test.expect (N# (m11# *# n11# +# m12# *# n21#) ~~ 1.0)
    , Test.expect (N# (m11# *# n12# +# m12# *# n22#) ~~ 0.0)
    , Test.expect (N# (m21# *# n11# +# m22# *# n21#) ~~ 0.0)
    , Test.expect (N# (m21# *# n12# +# m22# *# n22#) ~~ 1.0)
    ]

inverse3D :: Test
inverse3D = Test.check 100 "inverse3D" do
  N# m11# <- Test.generate matrixComponent
  N# m21# <- Test.generate matrixComponent
  N# m31# <- Test.generate matrixComponent
  N# m12# <- Test.generate matrixComponent
  N# m22# <- Test.generate matrixComponent
  N# m32# <- Test.generate matrixComponent
  N# m13# <- Test.generate matrixComponent
  N# m23# <- Test.generate matrixComponent
  N# m33# <- Test.generate matrixComponent
  let matrix# =
        Matrix3D#
          (# m11#, m12#, m13# #)
          (# m21#, m22#, m23# #)
          (# m31#, m32#, m33# #)
  let !( Matrix3D#
           (# n11#, n12#, n13# #)
           (# n21#, n22#, n23# #)
           (# n31#, n32#, n33# #)
         ) = inverse3D# matrix#
  Test.all
    [ Test.expect (N# (m11# *# n11# +# m12# *# n21# +# m13# *# n31#) ~~ 1.0)
    , Test.expect (N# (m11# *# n12# +# m12# *# n22# +# m13# *# n32#) ~~ 0.0)
    , Test.expect (N# (m11# *# n13# +# m12# *# n23# +# m13# *# n33#) ~~ 0.0)
    , Test.expect (N# (m21# *# n11# +# m22# *# n21# +# m23# *# n31#) ~~ 0.0)
    , Test.expect (N# (m21# *# n12# +# m22# *# n22# +# m23# *# n32#) ~~ 1.0)
    , Test.expect (N# (m21# *# n13# +# m22# *# n23# +# m23# *# n33#) ~~ 0.0)
    , Test.expect (N# (m31# *# n11# +# m32# *# n21# +# m33# *# n31#) ~~ 0.0)
    , Test.expect (N# (m31# *# n12# +# m32# *# n22# +# m33# *# n32#) ~~ 0.0)
    , Test.expect (N# (m31# *# n13# +# m32# *# n23# +# m33# *# n33#) ~~ 1.0)
    ]
