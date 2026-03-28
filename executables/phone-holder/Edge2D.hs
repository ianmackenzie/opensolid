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

data Edge2D space = Line (Line2D Meters space) | Arc (Arc2D Meters space)

startPoint :: Edge2D space -> Point2D Meters space
startPoint (Line line) = Line2D.startPoint line
startPoint (Arc arc) = Arc2D.startPoint arc

endPoint :: Edge2D space -> Point2D Meters space
endPoint (Line line) = Line2D.endPoint line
endPoint (Arc arc) = Arc2D.endPoint arc

startTangent :: Tolerance Meters => Edge2D space -> Result IsDegenerate (Direction2D space)
startTangent (Line line) = Line2D.direction line
startTangent (Arc arc) = Ok (Arc2D.tangentDirection arc 0.0)

endTangent :: Tolerance Meters => Edge2D space -> Result IsDegenerate (Direction2D space)
endTangent (Line line) = Line2D.direction line
endTangent (Arc arc) = Ok (Arc2D.tangentDirection arc 1.0)

reverse :: Edge2D space -> Edge2D space
reverse (Line line) = Line (Line2D.reverse line)
reverse (Arc arc) = Arc (Arc2D.reverse arc)

offsetLeftwardBy :: Tolerance Meters => Length -> Edge2D space -> Result IsDegenerate (Edge2D space)
offsetLeftwardBy distance (Line line) = Result.map Line (Line2D.offsetLeftwardBy distance line)
offsetLeftwardBy distance (Arc arc) = Ok (Arc (Arc2D.offsetLeftwardBy distance arc))

offsetRightwardBy :: Tolerance Meters => Length -> Edge2D space -> Result IsDegenerate (Edge2D space)
offsetRightwardBy distance = offsetLeftwardBy -distance

toCurve :: Edge2D space -> Curve2D Meters space
toCurve (Line line) = Curve2D.line line
toCurve (Arc arc) = Curve2D.arc arc
