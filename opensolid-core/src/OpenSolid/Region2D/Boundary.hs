module OpenSolid.Region2D.Boundary
  ( Boundary
  , build
  , new
  , bounds
  , curves
  , transformBy
  , placeIn
  , relativeTo
  , convert
  , unconvert
  , pointSweptAngle
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Bounds2D (Bounds2D)
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D.BoundaryTree (BoundaryTree)
import OpenSolid.Region2D.BoundaryTree qualified as Region2D.BoundaryTree
import OpenSolid.Set2D (Set2D)
import OpenSolid.Set2D qualified as Set2D
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units (HasUnits)
import OpenSolid.Units qualified as Units

data Boundary units = Boundary
  { curves :: Set2D units (Curve2D units)
  , tree :: ~(BoundaryTree units)
  }

instance HasUnits (Boundary units) units

instance Units.Coercion (Boundary units1) (Boundary units2) where
  coerce boundary =
    Boundary
      { curves = Units.coerce boundary.curves
      , tree = Units.coerce boundary.tree
      }

instance units1 ~ units2 => Intersects (Point2D units1) (Boundary units2) (Tolerance units1) where
  intersects point boundary = Set2D.any (intersects point) (intersects point) (curves boundary)

instance units1 ~ units2 => Intersects (Boundary units2) (Point2D units1) (Tolerance units1) where
  intersects boundary point = intersects point boundary

build :: NonEmpty (Curve2D units) -> Boundary units
build givenCurves = new (Set2D.linear Curve2D.overallBounds givenCurves)

new :: Set2D units (Curve2D units) -> Boundary units
new givenCurves = Boundary givenCurves (Region2D.BoundaryTree.build givenCurves)

bounds :: Boundary units -> Bounds2D units
bounds boundary = Region2D.BoundaryTree.bounds boundary.tree

curves :: Boundary units -> Set2D units (Curve2D units)
curves = (.curves)

map :: Sign -> (Curve2D units1 -> Curve2D units2) -> Boundary units1 -> Boundary units2
map sign function boundary =
  new $ case sign of
    Positive -> Set2D.map function Curve2D.overallBounds (curves boundary)
    Negative -> Set2D.reverseMap (Curve2D.reverse . function) Curve2D.overallBounds (curves boundary)

transformBy :: Transform2D tag units -> Boundary units -> Boundary units
transformBy transform = map (Transform2D.handedness transform) (Curve2D.transformBy transform)

placeIn :: Frame2D units -> Boundary units -> Boundary units
placeIn frame = map Positive (Curve2D.placeIn frame)

relativeTo :: Frame2D units -> Boundary units -> Boundary units
relativeTo frame = map Positive (Curve2D.relativeTo frame)

convert :: Quantity (units2 ?/? units1) -> Boundary units1 -> Boundary units2
convert factor = map (Quantity.sign factor) (Curve2D.convert factor)

unconvert :: Quantity (units2 ?/? units1) -> Boundary units2 -> Boundary units1
unconvert factor = map (Quantity.sign factor) (Curve2D.unconvert factor)

pointSweptAngle :: Tolerance units => Point2D units -> Boundary units -> Maybe Angle
pointSweptAngle point boundary
  | intersects point boundary = Nothing
  | otherwise = Just (Region2D.BoundaryTree.pointSweptAngle point boundary.tree)
