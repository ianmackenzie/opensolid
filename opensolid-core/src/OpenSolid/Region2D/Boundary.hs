module OpenSolid.Region2D.Boundary
  ( Boundary
  , PointClassification (InteriorPoint, ExteriorPoint, IncidentPoint)
  , BoundsClassification (InteriorBounds, ExteriorBounds)
  , unsafe
  , bounds
  , curves
  , loop
  , transformBy
  , placeIn
  , relativeTo
  , convert
  , unconvert
  , classifyPoint
  , classifyBounds
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Bounded (Bounded)
import OpenSolid.Bounded qualified as Bounded
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
import OpenSolid.Transform.Tag qualified as Transform.Tag
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units (Units)
import OpenSolid.Units qualified as Units

data Boundary units = Boundary
  { curves :: Set2D units (Curve2D units)
  , tree :: ~(BoundaryTree units)
  }

instance Units (Boundary units) units

instance Units.Coercion (Boundary units1) (Boundary units2) where
  coerce boundary =
    Boundary
      { curves = Set2D.map Units.coerce boundary.curves
      , tree = Units.coerce boundary.tree
      }

instance units1 ~ units2 => Bounded (Boundary units1) (Bounds2D units2) where
  {-# INLINE bounds #-}
  bounds = bounds

instance Indexed (Boundary units) Int (Curve2D units) where
  boundary @ index = boundary.curves @ index

instance units1 ~ units2 => Intersects (Point2D units1) (Boundary units2) units1 where
  point ^ boundary = point ^ curves boundary

instance units1 ~ units2 => Intersects (Boundary units2) (Point2D units1) units1 where
  boundary ^ point = point ^ boundary

data PointClassification
  = InteriorPoint
  | ExteriorPoint
  | IncidentPoint
  deriving (Eq, Show)

data BoundsClassification
  = InteriorBounds
  | ExteriorBounds
  deriving (Eq, Show)

unsafe :: NonEmpty (Curve2D units) -> Boundary units
unsafe givenCurves = build (Set2D.linear givenCurves)

build :: Set2D units (Curve2D units) -> Boundary units
build givenCurves = Boundary givenCurves (Region2D.BoundaryTree.build givenCurves)

bounds :: Boundary units -> Bounds2D units
bounds boundary = Region2D.BoundaryTree.bounds boundary.tree

curves :: Boundary units -> Set2D units (Curve2D units)
curves = (.curves)

loop :: Boundary units -> NonEmpty (Curve2D units)
loop = Set2D.toNonEmpty . curves

map :: Sign -> (Curve2D units1 -> Curve2D units2) -> Boundary units1 -> Boundary units2
map sign function boundary =
  build $ case sign of
    Positive -> Set2D.map function (curves boundary)
    Negative -> Set2D.reverseMap (Curve2D.reverse . function) (curves boundary)

transformBy ::
  Transform.Tag.IsOrthonormal tag =>
  Transform2D tag units ->
  Boundary units ->
  Boundary units
transformBy transform = map (Transform2D.handedness transform) (Curve2D.transformBy transform)

placeIn :: Frame2D units -> Boundary units -> Boundary units
placeIn frame = map Positive (Curve2D.placeIn frame)

relativeTo :: Frame2D units -> Boundary units -> Boundary units
relativeTo frame = map Positive (Curve2D.relativeTo frame)

convert :: Quantity (units2 ?/? units1) -> Boundary units1 -> Boundary units2
convert factor = map (Quantity.sign factor) (Curve2D.convert factor)

unconvert :: Quantity (units2 ?/? units1) -> Boundary units2 -> Boundary units1
unconvert factor = map (Quantity.sign factor) (Curve2D.unconvert factor)

isInterior :: Angle -> Bool
isInterior sweptAngle
  | angular (sweptAngle ~= Angle.zero) = False
  | angular (Quantity.abs sweptAngle ~= Angle.twoPi) = True
  | otherwise = error "Boundary swept angle should be either zero or a full turn"

classifyPoint :: Tolerance units => Point2D units -> Boundary units -> PointClassification
classifyPoint point boundary
  | point ^ boundary = IncidentPoint
  | otherwise = do
      let sweptAngle = Region2D.BoundaryTree.pointSweptAngle point boundary.tree
      if isInterior sweptAngle then InteriorPoint else ExteriorPoint

classifyBounds :: Bounds2D units -> Boundary units -> Fuzzy BoundsClassification
classifyBounds givenBounds boundary = do
  sweptAngle <- Region2D.BoundaryTree.boundsSweptAngle givenBounds boundary.tree
  Resolved (if isInterior sweptAngle then InteriorBounds else ExteriorBounds)
