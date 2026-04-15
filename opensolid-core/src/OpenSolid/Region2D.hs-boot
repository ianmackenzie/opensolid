module OpenSolid.Region2D (Region2D, outerLoop, innerLoops, boundaryCurves) where

import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude
import OpenSolid.Set2D (Set2D)

type role Region2D nominal

type Region2D :: Type -> Type
data Region2D units

outerLoop :: Region2D units -> Set2D units (Curve2D units)
innerLoops :: Region2D units -> List (Set2D units (Curve2D units))
boundaryCurves :: Region2D units -> Set2D units (Curve2D units)
