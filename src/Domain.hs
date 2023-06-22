module Domain
  ( Domain
  , unit
  , expand
  )
where

import OpenSolid
import Range (Range (Range))
import Range qualified
import Units (Unitless)

type Domain = Range Unitless

unit :: Domain
unit = Range.from 0.0 1.0

expand :: Domain -> Domain
expand (Range low high) =
  let expansion = 0.5 * (high - low)
   in Range.unsafe (max 0.0 (low - expansion)) (min 1.0 (high + expansion))
