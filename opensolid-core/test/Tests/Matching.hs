module Tests.Matching
  ( Matching (impl)
  , matching
  , matchingBy
  )
where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint qualified as Curve.IntersectionPoint
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Root qualified as Curve1D.Root
import OpenSolid.Curve3D qualified as Curve3D
import OpenSolid.Curve3D.IntersectionPointWithSurface qualified as Curve3D.IntersectionPointWithSurface
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance

class Matching a where
  impl :: Tolerance Meters => a -> a -> Bool

matching :: Matching a => a -> a -> Bool
matching first second = Tolerance.using Length.defaultTolerance (impl first second)

matchingBy :: Matching b => (a -> b) -> a -> a -> Bool
matchingBy function first second = matching (function first) (function second)

instance (Matching a, Matching b) => Matching (a, b) where
  impl (a1, b1) (a2, b2) = matching a1 a2 && matching b1 b2

instance Matching a => Matching (List a) where
  impl [] [] = True
  impl NonEmpty{} [] = False
  impl [] NonEmpty{} = False
  impl (x : xs) (y : ys) = matching x y && matching xs ys

instance Matching a => Matching (NonEmpty a) where
  impl (x :| xs) (y :| ys) = matching x y && matching xs ys

instance Matching a => Matching (Maybe a) where
  impl (Just first) (Just second) = matching first second
  impl Nothing Nothing = True
  impl Just{} Nothing = False
  impl Nothing Just{} = False

instance Matching Number where
  impl first second = unitless (first ~= second)

instance Matching Length where
  impl first second = first ~= second

instance Matching Curve1D.Root where
  impl root1 root2 =
    matching (Curve1D.Root.location root1) (Curve1D.Root.location root2)
      && (Curve1D.Root.order root1 == Curve1D.Root.order root2)
      && (Curve1D.Root.sign root1 == Curve1D.Root.sign root2)

instance Matching Curve.IntersectionPoint where
  impl first second =
    (Curve.IntersectionPoint.continuity first == Curve.IntersectionPoint.continuity second)
      && matching
        (Curve.IntersectionPoint.parameterValues first)
        (Curve.IntersectionPoint.parameterValues second)

instance Matching Curve3D.IntersectionPointWithSurface where
  impl first second =
    first.kind == second.kind
      && unitless (first.t ~= second.t)
      && unitless (first.uv ~= second.uv)
