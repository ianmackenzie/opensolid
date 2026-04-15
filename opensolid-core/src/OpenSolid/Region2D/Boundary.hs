module OpenSolid.Region2D.Boundary
  ( Boundary
  , Classification (Internal, External, Intersected)
  , build
  , new
  , bounds
  , curves
  , loop
  , transformBy
  , placeIn
  , relativeTo
  , convert
  , unconvert
  , classifyPoint
  )
where

import OpenSolid.Angle qualified as Angle
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

instance Indexed (Boundary units) Int (Curve2D units) where
  boundary !! index = boundary.curves !! index

instance units1 ~ units2 => Intersects (Point2D units1) (Boundary units2) (Tolerance units1) where
  intersects point boundary = Set2D.any (intersects point) (intersects point) (curves boundary)

instance units1 ~ units2 => Intersects (Boundary units2) (Point2D units1) (Tolerance units1) where
  intersects boundary point = intersects point boundary

data Classification = Internal | External | Intersected deriving (Eq, Show, Bounded, Enum)

build :: NonEmpty (Curve2D units) -> Boundary units
build givenCurves = new (Set2D.linear Curve2D.overallBounds givenCurves)

new :: Set2D units (Curve2D units) -> Boundary units
new givenCurves = Boundary givenCurves (Region2D.BoundaryTree.build givenCurves)

bounds :: Boundary units -> Bounds2D units
bounds boundary = Region2D.BoundaryTree.bounds boundary.tree

curves :: Boundary units -> Set2D units (Curve2D units)
curves = (.curves)

loop :: Boundary units -> NonEmpty (Curve2D units)
loop = Set2D.toNonEmpty . curves

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

classifyPoint :: Tolerance units => Point2D units -> Boundary units -> Classification
classifyPoint point boundary
  | intersects point boundary = Intersected
  | otherwise = do
      let sweptAngle = Region2D.BoundaryTree.pointSweptAngle point boundary.tree
      if Quantity.abs sweptAngle > Angle.pi then Internal else External
