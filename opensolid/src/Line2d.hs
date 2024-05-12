module Line2d (from) where

import Curve2d (Curve2d)
import Curve2d.Internal qualified
import OpenSolid
import Point2d (Point2d)

from :: Point2d (space @ units) -> Point2d (space @ units) -> Curve2d (space @ units)
from = Curve2d.Internal.Line
