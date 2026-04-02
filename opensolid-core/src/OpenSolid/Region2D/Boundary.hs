module OpenSolid.Region2D.Boundary
  ( Boundary
  , new
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
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Region2D.BoundaryTree (BoundaryTree)
import OpenSolid.Region2D.BoundaryTree qualified as Region2D.BoundaryTree
import OpenSolid.Transform2D (Transform2D)
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Units qualified as Units

data Boundary units = Boundary
  { curves :: NonEmpty (Curve2D units)
  , tree :: ~(BoundaryTree units)
  }

instance Units.Coercion (Boundary units1) (Boundary units2) where
  coerce boundary =
    Boundary
      { curves = NonEmpty.map Units.coerce boundary.curves
      , tree = Units.coerce boundary.tree
      }

new :: NonEmpty (Curve2D units) -> Boundary units
new givenCurves =
  Boundary
    { curves = givenCurves
    , tree = Region2D.BoundaryTree.build givenCurves
    }

curves :: Boundary units -> NonEmpty (Curve2D units)
curves = (.curves)

rebuild :: Sign -> (Curve2D units1 -> Curve2D units2) -> Boundary units1 -> Boundary units2
rebuild sign function boundary =
  new $ case sign of
    Positive -> NonEmpty.map function (curves boundary)
    Negative -> NonEmpty.reverseMap (Curve2D.reverse . function) (curves boundary)

transformBy :: Transform2D tag units -> Boundary units -> Boundary units
transformBy transform = rebuild (Transform2D.handedness transform) (Curve2D.transformBy transform)

placeIn :: Frame2D units -> Boundary units -> Boundary units
placeIn frame = rebuild Positive (Curve2D.placeIn frame)

relativeTo :: Frame2D units -> Boundary units -> Boundary units
relativeTo frame = rebuild Positive (Curve2D.relativeTo frame)

convert :: Quantity (units2 ?/? units1) -> Boundary units1 -> Boundary units2
convert factor = rebuild (Quantity.sign factor) (Curve2D.convert factor)

unconvert :: Quantity (units2 ?/? units1) -> Boundary units2 -> Boundary units1
unconvert factor = rebuild (Quantity.sign factor) (Curve2D.unconvert factor)

pointSweptAngle :: Tolerance units => Point2D units -> Boundary units -> Maybe Angle
pointSweptAngle point boundary
  | NonEmpty.any (intersects point) (curves boundary) = Nothing
  | otherwise = Just (Region2D.BoundaryTree.pointSweptAngle point boundary.tree)
