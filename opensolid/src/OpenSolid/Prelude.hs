module OpenSolid.Prelude
  ( module OpenSolid.Bootstrap
  , module Composition
  , module OpenSolid.Arithmetic
  , module Result
  , module OpenSolid.Qty
  , module Float
  , module NonEmpty
  , module Sign
  , module CoordinateSystem
  , module Tolerance
  , module Fuzzy
  , module Units
  , module Intersects
  )
where

import Composition (Composition ((>>)), (.))
import CoordinateSystem (CoordinateSystem, Defines, LocalSpace, type (@))
import Float (Float, fromRational)
import Fuzzy (Fuzzy (Resolved, Unresolved))
import Intersects (Intersects ((^)))
import NonEmpty (NonEmpty ((:|)), (|:), pattern NonEmpty)
import OpenSolid.Arithmetic hiding
  ( CrossProduct (CrossProduct)
  , Difference (Difference)
  , DotProduct (DotProduct)
  , Product (Product)
  , Quotient (Quotient)
  , Sum (Sum)
  )
import OpenSolid.Bootstrap
import OpenSolid.Qty (Qty)
import Result (Result (Failure, Success))
import Sign (Sign (Negative, Positive))
import Tolerance (ApproximateEquality ((~=)), Tolerance, (!=))
import Units (HasUnits (UnitsOf), Unitless, (:*:), (:/:))
