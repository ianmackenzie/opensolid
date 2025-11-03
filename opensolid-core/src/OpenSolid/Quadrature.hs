module OpenSolid.Quadrature
  ( abscissae2
  , weights2
  , abscissae3
  , weights3
  , abscissae4
  , weights4
  , abscissae5
  , weights5
  )
where

import OpenSolid.Prelude

-- Values are adapted from https://pomax.github.io/bezierinfo/legendre-gauss.html
-- with abscissae converted to cover the range [0, 1] and weights halved as a result

abscissae2 :: (Float, Float)
abscissae2 =
  (0.21132486540518713, 0.7886751345948129)

weights2 :: (Float, Float)
weights2 =
  (0.5, 0.5)

abscissae3 :: (Float, Float, Float)
abscissae3 =
  (0.1127016653792583, 0.5, 0.8872983346207417)

weights3 :: (Float, Float, Float)
weights3 =
  (0.2777777777777778, 0.4444444444444444, 0.2777777777777778)

abscissae4 :: (Float, Float, Float, Float)
abscissae4 =
  (0.06943184420297371, 0.33000947820757187, 0.6699905217924281, 0.9305681557970262)

weights4 :: (Float, Float, Float, Float)
weights4 =
  (0.1739274225687269, 0.32607257743127305, 0.32607257743127305, 0.1739274225687269)

abscissae5 :: (Float, Float, Float, Float, Float)
abscissae5 =
  ( 0.04691007703066802
  , 0.23076534494715845
  , 0.5
  , 0.7692346550528415
  , 0.9530899229693319
  )

weights5 :: (Float, Float, Float, Float, Float)
weights5 =
  ( 0.11846344252809456
  , 0.23931433524968326
  , 0.28444444444444444
  , 0.23931433524968326
  , 0.11846344252809456
  )
