module Tests.Matching
  ( Matching (matching)
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
  matching :: a -> a -> Bool

matchingBy :: Matching b => (a -> b) -> a -> a -> Bool
matchingBy function first second = matching (function first) (function second)

instance (Matching a, Matching b) => Matching (a, b) where
  matching (a1, b1) (a2, b2) = matching a1 a2 && matching b1 b2

instance Matching a => Matching (List a) where
  matching [] [] = True
  matching NonEmpty{} [] = False
  matching [] NonEmpty{} = False
  matching (x : xs) (y : ys) = matching x y && matching xs ys

instance Matching a => Matching (NonEmpty a) where
  matching (x :| xs) (y :| ys) = matching x y && matching xs ys

instance Matching a => Matching (Maybe a) where
  matching (Just first) (Just second) = matching first second
  matching Nothing Nothing = True
  matching Just{} Nothing = False
  matching Nothing Just{} = False

instance Matching Number where
  matching first second = Tolerance.using Tolerance.unitless (first ~= second)

instance Matching Length where
  matching first second = Tolerance.using Length.defaultTolerance (first ~= second)

instance Matching Curve1D.Root where
  matching root1 root2 =
    matching (Curve1D.Root.location root1) (Curve1D.Root.location root2)
      && (Curve1D.Root.order root1 == Curve1D.Root.order root2)
      && (Curve1D.Root.sign root1 == Curve1D.Root.sign root2)

instance Matching Curve.IntersectionPoint where
  matching first second =
    (Curve.IntersectionPoint.continuity first == Curve.IntersectionPoint.continuity second)
      && matching
        (Curve.IntersectionPoint.parameterValues first)
        (Curve.IntersectionPoint.parameterValues second)

instance Matching Curve3D.IntersectionPointWithSurface where
  matching first second = Tolerance.using Tolerance.unitless do
    first.kind == second.kind
      && first.t ~= second.t
      && first.uv ~= second.uv
