module Tests.Random
  ( length
  , lengthRange
  , point2d
  , vector2d
  , vectorBounds3d
  , axis2d
  , frame2d
  , bounds2d
  , vectorBounds2d
  , line2d
  , arc2d
  , quadraticSpline2d
  , cubicSpline2d
  , rigidTransform2d
  , affineTransform2d
  )
where

import Angle qualified
import Arc2d qualified
import Axis2d (Axis2d)
import Axis2d qualified
import Bounds2d (Bounds2d)
import Bounds2d qualified
import CubicSpline2d qualified
import Curve2d (Curve2d)
import Direction2d qualified
import Frame2d (Frame2d)
import Frame2d qualified
import Length (Length)
import Length qualified
import Line2d qualified
import NonEmpty qualified
import OpenSolid
import Point2d (Point2d)
import Point2d qualified
import QuadraticSpline2d qualified
import Random (Generator)
import Random qualified
import Range (Range)
import Range qualified
import Transform2d qualified
import Units (Meters)
import Vector2d (Vector2d)
import Vector2d qualified
import VectorBounds2d (VectorBounds2d (VectorBounds2d))
import VectorBounds3d (VectorBounds3d (VectorBounds3d))

length :: Generator Length
length = Random.qty (Length.meters -10.0) (Length.meters 10.0)

lengthRange :: Generator (Range Meters)
lengthRange = Range.generator length

point2d :: Generator (Point2d (space @ Meters))
point2d = Random.map2 Point2d.xy length length

vector2d :: Generator (Vector2d (space @ Meters))
vector2d = Random.map2 Vector2d.xy length length

vectorBounds3d :: Generator (VectorBounds3d (space @ Meters))
vectorBounds3d = Random.map3 VectorBounds3d lengthRange lengthRange lengthRange

axis2d :: Generator (Axis2d (space @ Meters))
axis2d = Random.map2 Axis2d.through point2d Direction2d.generator

frame2d :: Generator (Frame2d (global @ Meters) (Defines local))
frame2d = Random.map Frame2d.fromXAxis axis2d

bounds2d :: Generator (Bounds2d (space @ Meters))
bounds2d = Random.map2 Bounds2d.xy lengthRange lengthRange

vectorBounds2d :: Generator (VectorBounds2d (space @ Meters))
vectorBounds2d = Random.map2 VectorBounds2d lengthRange lengthRange

line2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
line2d = Random.map2 Line2d.from point2d point2d

arc2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
arc2d = Random.do
  startPoint <- point2d
  endPoint <- point2d
  sweptAngle <- Random.qty (Angle.degrees -315.0) (Angle.degrees 315.0)
  Random.return (Arc2d.from startPoint endPoint sweptAngle)

quadraticSpline2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
quadraticSpline2d = Random.map3 QuadraticSpline2d.fromControlPoints point2d point2d point2d

cubicSpline2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
cubicSpline2d = Random.map4 CubicSpline2d.fromControlPoints point2d point2d point2d point2d

translation2d :: Generator (Transform2d.Rigid (space @ Meters))
translation2d = Random.map Transform2d.translateBy vector2d

rotation2d :: Generator (Transform2d.Rigid (space @ Meters))
rotation2d = Random.do
  centerPoint <- point2d
  angle <- Random.qty (Angle.degrees -360.0) (Angle.degrees 360.0)
  Random.return (Transform2d.rotateAround centerPoint angle)

mirror2d :: Generator (Transform2d.Rigid (space @ Meters))
mirror2d = Random.map Transform2d.mirrorAcross axis2d

rigidTransform2d :: Generator (Transform2d.Rigid (space @ Meters))
rigidTransform2d = Random.oneOf (NonEmpty.of3 translation2d rotation2d mirror2d)

scalingFactor :: Generator Float
scalingFactor = Random.float 0.5 2.0

uniformScaling2d :: Generator (Transform2d.Uniform (space @ Meters))
uniformScaling2d = Random.map2 Transform2d.scaleAbout point2d scalingFactor

nonUniformScaling2d :: Generator (Transform2d.Affine (space @ Meters))
nonUniformScaling2d = Random.map2 Transform2d.scaleAlong axis2d scalingFactor

affineTransform2d :: Generator (Transform2d.Affine (space @ Meters))
affineTransform2d =
  Random.oneOf $
    NonEmpty.of5
      (Random.map Transform2d.toAffine translation2d)
      (Random.map Transform2d.toAffine rotation2d)
      (Random.map Transform2d.toAffine mirror2d)
      (Random.map Transform2d.toAffine uniformScaling2d)
      nonUniformScaling2d
