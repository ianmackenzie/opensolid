module OpenSolid.Curve.Intersections
  ( Intersections (..)
  , Error (..)
  )
where

import OpenSolid.Curve.IntersectionPoint (IntersectionPoint)
import OpenSolid.Interval (Interval)
import OpenSolid.Point (Point)
import OpenSolid.Prelude

data Intersections
  = IntersectionPoints (NonEmpty IntersectionPoint)
  | OverlappingSegments
      Sign
      (NonEmpty (Interval Unitless, Interval Unitless))
      (List IntersectionPoint)
  deriving (Show)

data Error dimension units space
  = DegenerateCoincident (Point dimension units space)
  | DegenerateFirstOnSecond (Point dimension units space) (NonEmpty Number)
  | DegenerateSecondOnFirst (Point dimension units space) (NonEmpty Number)

deriving instance Eq (Point dimension units space) => Eq (Error dimension units space)

deriving instance Show (Point dimension units space) => Show (Error dimension units space)

deriving instance Show (Point dimension units space) => Err (Error dimension units space)
