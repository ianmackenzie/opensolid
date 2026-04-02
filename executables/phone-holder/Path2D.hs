module Path2D
  ( Path2D
  , startPoint
  , endPoint
  , startTangent
  , endTangent
  , reverse
  , offsetLeftwardBy
  , offsetRightwardBy
  , thickenLeftwardBy
  , thickenRightwardBy
  , addArc
  , addLine
  )
where

import Edge2D (Edge2D)
import Edge2D qualified
import OpenSolid.Angle (Angle)
import OpenSolid.Arc2D qualified as Arc2D
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Error (IsDegenerate)
import OpenSolid.Length (Length)
import OpenSolid.Line2D (pattern Line2D)
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2D (Point2D)
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Result qualified as Result

type Path2D = NonEmpty Edge2D

startPoint :: Path2D -> Point2D Meters
startPoint path = Edge2D.startPoint (NonEmpty.first path)

endPoint :: Path2D -> Point2D Meters
endPoint path = Edge2D.endPoint (NonEmpty.last path)

startTangent :: Tolerance Meters => Path2D -> Result IsDegenerate Direction2D
startTangent path = Edge2D.startTangent (NonEmpty.first path)

endTangent :: Tolerance Meters => Path2D -> Result IsDegenerate Direction2D
endTangent path = Edge2D.endTangent (NonEmpty.last path)

reverse :: Path2D -> Path2D
reverse = NonEmpty.reverseMap Edge2D.reverse

offsetLeftwardBy :: Tolerance Meters => Length -> Path2D -> Result IsDegenerate Path2D
offsetLeftwardBy distance path = Result.collect (Edge2D.offsetLeftwardBy distance) path

offsetRightwardBy :: Tolerance Meters => Length -> Path2D -> Result IsDegenerate Path2D
offsetRightwardBy distance = offsetLeftwardBy -distance

thickenLeftwardBy :: Tolerance Meters => Length -> Path2D -> Result IsDegenerate Path2D
thickenLeftwardBy distance path = do
  offsetPath <- offsetLeftwardBy distance path & Result.map reverse
  Ok $
    NonEmpty.concat $
      NonEmpty.four
        path
        (NonEmpty.one (Edge2D.Line (Line2D (endPoint path) (startPoint offsetPath))))
        offsetPath
        (NonEmpty.one (Edge2D.Line (Line2D (endPoint offsetPath) (startPoint path))))

thickenRightwardBy :: Tolerance Meters => Length -> Path2D -> Result IsDegenerate Path2D
thickenRightwardBy distance path = thickenLeftwardBy distance (reverse path)

addArc :: Tolerance Meters => Length -> Angle -> Path2D -> Result IsDegenerate Path2D
addArc radius sweptAngle path = do
  arcStartTangent <- endTangent path
  let arcStartPoint = endPoint path
  let arcCenterOffsetDirection = Quantity.sign sweptAngle * Direction2D.rotateLeft arcStartTangent
  let arcCenterPoint = arcStartPoint + radius * arcCenterOffsetDirection
  let arc = Arc2D.sweptAround arcCenterPoint arcStartPoint sweptAngle
  Ok (add (Edge2D.Arc arc) path)

addLine :: Tolerance Meters => Length -> Path2D -> Result IsDegenerate Path2D
addLine length path = do
  lineDirection <- endTangent path
  let lineStartPoint = endPoint path
  let line = Line2D lineStartPoint (lineStartPoint + length * lineDirection)
  Ok (add (Edge2D.Line line) path)

add :: Edge2D -> Path2D -> Path2D
add edge path = path <> NonEmpty.one edge
