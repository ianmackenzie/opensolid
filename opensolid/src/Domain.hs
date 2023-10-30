module Domain
  ( Domain
  , unit
  , expand
  , sample
  )
where

import Float qualified
import OpenSolid
import Quadrature qualified
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
  [ function (Range.interpolate domain Quadrature.t1)
  , function (Range.interpolate domain Quadrature.t2)
  , function (Range.interpolate domain Quadrature.t3)
  , function (Range.interpolate domain Quadrature.t4)
  , function (Range.interpolate domain Quadrature.t5)
  ]
