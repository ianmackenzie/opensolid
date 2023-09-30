module Domain
  ( Domain
  , unit
  , expand
  , sample
  )
where

import Float qualified
import OpenSolid
import Range (Range (Range))
import Range qualified

type Domain = Range Unitless

unit :: Domain
unit = Range.from 0.0 1.0

expand :: Domain -> Domain
expand (Range low high) =
  let expansion = 0.5 * (high - low)
   in Range.unsafe (Float.max 0.0 (low - expansion)) (Float.min 1.0 (high + expansion))

{- | Sample a given function at five "random" points within a given domain.
This can be useful for checking whether some property (likely) holds true everywhere within a domain,
for example:

- Is this function zero everywhere?
- Do these two curves overlap? (Does every point on the first curve lie on the second curve?)

The sampling points are based on Gaussian quadrature. It is of course possible that (for example)
a function could be zero at the five sampling points but non-zero elsewhere,
but should be extremely unlikely.
-}
sample :: (Float -> a) -> Domain -> List a
sample function domain =
  -- Parameter values are Gaussian quadrature abscissae,
  -- taken from https://pomax.github.io/bezierinfo/legendre-gauss.html#n5
  -- and converted to [0, 1] to allow interpolation within arbitrary domains
  [ function (Range.interpolate domain 0.04691007703066802)
  , function (Range.interpolate domain 0.23076534494715845)
  , function (Range.interpolate domain 0.5)
  , function (Range.interpolate domain 0.7692346550528415)
  , function (Range.interpolate domain 0.9530899229693319)
  ]
