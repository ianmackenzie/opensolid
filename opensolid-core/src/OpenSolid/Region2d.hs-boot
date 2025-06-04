module OpenSolid.Region2d (Region2d) where

import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude

type role Region2d nominal

data Region2d (coordinateSystem :: CoordinateSystem)

instance
  HasField
    "outerLoop"
    (Region2d (space @ units))
    (NonEmpty (Curve2d (space @ units)))

instance
  HasField
    "innerLoops"
    (Region2d (space @ units))
    (List (NonEmpty (Curve2d (space @ units))))

instance
  HasField
    "boundaryCurves"
    (Region2d (space @ units))
    (NonEmpty (Curve2d (space @ units)))
