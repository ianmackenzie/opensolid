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
  , orthonormalTransform2d
  , affineTransform2d
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Arc2d qualified as Arc2d
import OpenSolid.Axis2d (Axis2d)
import OpenSolid.Axis2d qualified as Axis2d
import OpenSolid.Bounds2d (Bounds2d)
import OpenSolid.Bounds2d qualified as Bounds2d
import OpenSolid.CubicSpline2d qualified as CubicSpline2d
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Line2d qualified as Line2d
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Point2d (Point2d)
import OpenSolid.Point2d qualified as Point2d
import OpenSolid.Prelude
import OpenSolid.Qty qualified as Qty
import OpenSolid.QuadraticSpline2d qualified as QuadraticSpline2d
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Range (Range)
import OpenSolid.Range qualified as Range
import OpenSolid.Sign qualified as Sign
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Units (Meters)
import OpenSolid.Vector2d (Vector2d)
import OpenSolid.Vector2d qualified as Vector2d
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))
import OpenSolid.VectorBounds3d (VectorBounds3d (VectorBounds3d))

length :: Generator Length
length = Qty.random (Length.meters -10.0) (Length.meters 10.0)

lengthRange :: Generator (Range Meters)
lengthRange = Range.random length

point2d :: Generator (Point2d (space @ Meters))
point2d = Random.map2 Point2d.xy length length

vector2d :: Generator (Vector2d (space @ Meters))
vector2d = Random.map2 Vector2d.xy length length

vectorBounds3d :: Generator (VectorBounds3d (space @ Meters))
vectorBounds3d = Random.map3 VectorBounds3d lengthRange lengthRange lengthRange

axis2d :: Generator (Axis2d (space @ Meters))
axis2d = Random.map2 Axis2d.through point2d Direction2d.random

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
  angleSign <- Sign.random
  angleMagnitude <- Qty.random (Angle.degrees 5.0) (Angle.degrees 355.0)
  let sweptAngle = angleSign * angleMagnitude
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
  angle <- Qty.random (Angle.degrees -360.0) (Angle.degrees 360.0)
  Random.return (Transform2d.rotateAround centerPoint angle)

mirror2d :: Generator (Transform2d.Orthonormal (space @ Meters))
mirror2d = Random.map Transform2d.mirrorAcross axis2d

rigidTransform2d :: Generator (Transform2d.Rigid (space @ Meters))
rigidTransform2d = Random.oneOf (NonEmpty.two translation2d rotation2d)

orthonormalTransform2d :: Generator (Transform2d.Orthonormal (space @ Meters))
orthonormalTransform2d =
  Random.oneOf $
    NonEmpty.three
      (Random.map Transform2d.toOrthonormal translation2d)
      (Random.map Transform2d.toOrthonormal rotation2d)
      mirror2d

scalingFactor :: Generator Float
scalingFactor = Float.random 0.5 2.0

uniformScaling2d :: Generator (Transform2d.Uniform (space @ Meters))
uniformScaling2d = Random.map2 Transform2d.scaleAbout point2d scalingFactor

nonUniformScaling2d :: Generator (Transform2d.Affine (space @ Meters))
nonUniformScaling2d = Random.map2 Transform2d.scaleAlong axis2d scalingFactor

affineTransform2d :: Generator (Transform2d.Affine (space @ Meters))
affineTransform2d =
  Random.oneOf $
    NonEmpty.five
      (Random.map Transform2d.toAffine translation2d)
      (Random.map Transform2d.toAffine rotation2d)
      (Random.map Transform2d.toAffine mirror2d)
      (Random.map Transform2d.toAffine uniformScaling2d)
      nonUniformScaling2d
