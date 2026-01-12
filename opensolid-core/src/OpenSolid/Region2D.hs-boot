module OpenSolid.Region2D (Region2D, outerLoop, innerLoops, boundaryCurves) where

import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude

type role Region2D nominal nominal

type Region2D :: Type -> Type -> Type
data Region2D units space

outerLoop :: Region2D units space -> NonEmpty (Curve2D units space)
innerLoops :: Region2D units space -> List (NonEmpty (Curve2D units space))
boundaryCurves :: Region2D units space -> NonEmpty (Curve2D units space)
