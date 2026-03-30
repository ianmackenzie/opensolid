module OpenSolid.Desingularization.Curve (Curve (..)) where

import OpenSolid.Bezier qualified as Bezier
import OpenSolid.Prelude

class
  Bezier.Constraints point vector =>
  Curve curve point vector
    | curve -> point
    , curve -> vector
  where
  value :: curve -> Number -> point
  derivativeValue :: curve -> Number -> vector
  secondDerivativeValue :: curve -> Number -> vector
  bezier :: NonEmpty point -> curve
  desingularized :: curve -> curve -> curve -> curve
