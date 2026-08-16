module OpenSolid.Internal.CurvePoint (CurvePoint (..)) where

import OpenSolid.CurveLocation (CurveLocation)
import OpenSolid.Direction (Direction)
import OpenSolid.Direction qualified as Direction
import OpenSolid.Nondegenerate qualified as Nondegenerate
import OpenSolid.Point (Point)
import OpenSolid.Point qualified as Point
import OpenSolid.Prelude
import OpenSolid.Vector (Vector)
import OpenSolid.Vector qualified as Vector

data CurvePoint dimension units space = CurvePoint
  { location :: CurveLocation
  , point :: ~(Point dimension units space)
  , derivative :: ~(Vector dimension units space)
  , tangentDirection :: ~(Direction dimension space)
  , curvatureVector_ :: Nondegenerate.Field (Vector dimension (Unitless ?/? units) space)
  }

deriving instance
  ( Point.Exists dimension units space
  , Vector.Exists dimension units space
  , Vector.Exists dimension (Unitless ?/? units) space
  , Direction.Exists dimension space
  ) =>
  Show (CurvePoint dimension units space)
