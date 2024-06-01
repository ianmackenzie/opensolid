module Line2d
  ( from
  , pattern Line2d
  , Line2d (startPoint, endPoint)
  )
where

import Curve2d (Curve2d)
import Curve2d.Internal qualified
import OpenSolid
import Point2d (Point2d)

from :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
from = Curve2d.Internal.Line

pattern Line2d :: Line2d (space @ units) -> Curve2d (space @ units)
pattern Line2d line <- (extractLine -> Just line)

data Line2d (coordinateSystem :: CoordinateSystem) where
  Line2d_ ::
    { startPoint :: Point2d (space @ units)
    , endPoint :: Point2d (space @ units)
    } ->
    Line2d (space @ units)

extractLine :: Curve2d (space @ units) -> Maybe (Line2d (space @ units))
extractLine curve = case curve of
  Curve2d.Internal.Line p1 p2 -> Just (Line2d_ p1 p2)
  _ -> Nothing
