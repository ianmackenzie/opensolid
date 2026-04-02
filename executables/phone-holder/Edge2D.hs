module Edge2D
  ( Edge2D (..)
  , startPoint
  , endPoint
  , startTangent
  , endTangent
  , reverse
  , offsetLeftwardBy
  , offsetRightwardBy
  , toCurve
  )
where

import OpenSolid.Arc2D (Arc2D)
import OpenSolid.Arc2D qualified as Arc2D
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Length (Length)
import OpenSolid.Line2D (Line2D)
import OpenSolid.Line2D qualified as Line2D
import OpenSolid.Nondegenerate (IsDegenerate)
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Result qualified as Result

data Edge2D = Line (Line2D Meters) | Arc (Arc2D Meters)

startPoint :: Edge2D -> Point2D Meters
startPoint (Line line) = Line2D.startPoint line
startPoint (Arc arc) = Arc2D.startPoint arc

endPoint :: Edge2D -> Point2D Meters
endPoint (Line line) = Line2D.endPoint line
endPoint (Arc arc) = Arc2D.endPoint arc

startTangent :: Tolerance Meters => Edge2D -> Result IsDegenerate Direction2D
startTangent (Line line) = Line2D.direction line
startTangent (Arc arc) = Ok (Arc2D.tangentDirection arc 0.0)

endTangent :: Tolerance Meters => Edge2D -> Result IsDegenerate Direction2D
endTangent (Line line) = Line2D.direction line
endTangent (Arc arc) = Ok (Arc2D.tangentDirection arc 1.0)

reverse :: Edge2D -> Edge2D
reverse (Line line) = Line (Line2D.reverse line)
reverse (Arc arc) = Arc (Arc2D.reverse arc)

offsetLeftwardBy :: Tolerance Meters => Length -> Edge2D -> Result IsDegenerate (Edge2D)
offsetLeftwardBy distance (Line line) = Result.map Line (Line2D.offsetLeftwardBy distance line)
offsetLeftwardBy distance (Arc arc) = Ok (Arc (Arc2D.offsetLeftwardBy distance arc))

offsetRightwardBy :: Tolerance Meters => Length -> Edge2D -> Result IsDegenerate (Edge2D)
offsetRightwardBy distance = offsetLeftwardBy -distance

toCurve :: Edge2D -> Curve2D Meters
toCurve (Line line) = Curve2D.line line
toCurve (Arc arc) = Curve2D.arc arc
