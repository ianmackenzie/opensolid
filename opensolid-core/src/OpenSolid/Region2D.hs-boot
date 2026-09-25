module OpenSolid.Region2D
  ( Region2D
  , PointClassification (..)
  , BoundsClassification (..)
  , outerBoundary
  , innerBoundaries
  , boundaryCurves
  )
where

import OpenSolid.Bag2D (Bag2D)
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Prelude
import {-# SOURCE #-} OpenSolid.Region2D.Boundary (Boundary)
import OpenSolid.Set2D (Set2D)

type role Region2D nominal

type Region2D :: Type -> Type
data Region2D units

data PointClassification
  = InteriorPoint
  | ExteriorPoint
  | BoundaryPoint

data BoundsClassification
  = InteriorBounds
  | ExteriorBounds

outerBoundary :: Region2D units -> Boundary units
innerBoundaries :: Region2D units -> Bag2D units (Boundary units)
boundaryCurves :: Region2D units -> Set2D units (Curve2D units)
