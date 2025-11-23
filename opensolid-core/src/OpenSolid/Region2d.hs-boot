module OpenSolid.Region2d (Region2d, outerLoop, innerLoops, boundaryCurves) where

import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude

type role Region2d nominal nominal

type Region2d :: Type -> Type -> Type
data Region2d space units

outerLoop :: Region2d space units -> NonEmpty (Curve2d space units)
innerLoops :: Region2d space units -> List (NonEmpty (Curve2d space units))
boundaryCurves :: Region2d space units -> NonEmpty (Curve2d space units)
