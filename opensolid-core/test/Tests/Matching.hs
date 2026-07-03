module Tests.Matching (Matching (matching)) where

import OpenSolid.Curve qualified as Curve
import OpenSolid.Curve.IntersectionPoint qualified as Curve.IntersectionPoint
import OpenSolid.Curve1D qualified as Curve1D
import OpenSolid.Curve1D.Zero qualified as Curve1D.Zero
import OpenSolid.CurvePoint (CurvePoint)
import OpenSolid.CurvePoint qualified as CurvePoint
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

instance Matching Curve1D.Zero where
  matching zero1 zero2 =
    matching zero1.location zero2.location
      && zero1.order == zero2.order
      && zero1.sign == zero2.sign

instance Matching (CurvePoint dimension units space) where
  matching = matchingBy CurvePoint.parameterValue

instance Matching (Curve.IntersectionPoint dimension units space) where
  matching first second =
    (Curve.IntersectionPoint.continuity first == Curve.IntersectionPoint.continuity second)
      && matching (Curve.IntersectionPoint.curvePoints first) (Curve.IntersectionPoint.curvePoints second)
