module OpenSolid.Region2d (Region2d, outerLoop, innerLoops, boundaryCurves) where

import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Prelude

type role Region2d nominal nominal

type Region2d :: Type -> Type -> Type
data Region2d units space

outerLoop :: Region2d units space -> NonEmpty (Curve2d units space)
innerLoops :: Region2d units space -> List (NonEmpty (Curve2d units space))
boundaryCurves :: Region2d units space -> NonEmpty (Curve2d units space)
