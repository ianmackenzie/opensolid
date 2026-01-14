module Tests.Random
  ( length
  , lengthInterval
  , point2D
  , point3D
  , vector2D
  , vector3D
  , vectorBounds3D
  , axis2D
  , axis3D
  , plane3D
  , orientation2D
  , planeOrientation3D
  , orientation3D
  , frame2D
  , frame3D
  , bounds2D
  , bounds3D
  , vectorBounds2D
  , line2D
  , arc2D
  , quadraticSpline2D
  , cubicSpline2D
  , rigidTransform2D
  , rigidTransform3D
  , orthonormalTransform2D
  , orthonormalTransform3D
  , uniformTransform2D
  , uniformTransform3D
  , affineTransform2D
  , affineTransform3D
  , surfaceParameter
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2D (Axis2D (Axis2D))
import OpenSolid.Axis3D (Axis3D (Axis3D))
import OpenSolid.Bounds2D (Bounds2D (Bounds2D))
import OpenSolid.Bounds3D (Bounds3D)
import OpenSolid.Curve2D (Curve2D)
import OpenSolid.Curve2D qualified as Curve2D
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Frame3D (Frame3D)
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Interval (Interval)
import OpenSolid.Interval qualified as Interval
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Orientation2D (Orientation2D)
import OpenSolid.Orientation2D qualified as Orientation2D
import OpenSolid.Orientation3D (Orientation3D)
import OpenSolid.Plane3D (Plane3D (Plane3D))
import OpenSolid.PlaneOrientation3D (PlaneOrientation3D)
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Point2D (Point2D, pattern Point2D)
import OpenSolid.Point3D (Point3D)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds3D (Bounds3D)
  , Point3D (Point3D)
  , Vector3D (Vector3D)
  , VectorBounds3D (VectorBounds3D)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Sign qualified as Sign
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2D qualified as Transform2D
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector2D (Vector2D, pattern Vector2D)
import OpenSolid.VectorBounds2D (VectorBounds2D (VectorBounds2D))

length :: Generator Length
length = Quantity.random (Length.meters -10) (Length.meters 10)

lengthInterval :: Generator (Interval Meters)
lengthInterval = Interval.random length

point2D :: Generator (Point2D Meters space)
point2D = Random.map2 Point2D length length

point3D :: Generator (Point3D space)
point3D = Random.map3 Point3D length length length

vector2D :: Generator (Vector2D Meters space)
vector2D = Random.map2 Vector2D length length

vector3D :: Generator (Vector3D Meters space)
vector3D = Random.map3 Vector3D length length length

vectorBounds3D :: Generator (VectorBounds3D Meters space)
vectorBounds3D =
  Random.map3
    VectorBounds3D
    lengthInterval
    lengthInterval
    lengthInterval

axis2D :: Generator (Axis2D Meters space)
axis2D = Random.map2 Axis2D point2D Direction2D.random

axis3D :: Generator (Axis3D space)
axis3D = Random.map2 Axis3D point3D Direction3D.random

orientation2D :: Generator (Orientation2D global)
orientation2D = Random.map Orientation2D.fromXDirection Direction2D.random

planeOrientation3D :: Generator (PlaneOrientation3D global)
planeOrientation3D =
  Random.retry $
    Random.map2
      (Tolerance.using 0.1 PlaneOrientation3D.fromDirections)
      Direction3D.random
      Direction3D.random

orientation3D :: Generator (Orientation3D global)
orientation3D = Random.map Frame3D.orientation frame3D

plane3D :: Generator (Plane3D global local)
plane3D = Random.map2 Plane3D point3D planeOrientation3D

frame2D :: Generator (Frame2D Meters global local)
frame2D = Random.map Frame2D.fromXAxis axis2D

frame3D :: Generator (Frame3D global local)
frame3D = Random.map Frame3D.fromTopPlane plane3D

bounds2D :: Generator (Bounds2D Meters space)
bounds2D = Random.map2 Bounds2D lengthInterval lengthInterval

bounds3D :: Generator (Bounds3D space)
bounds3D = Random.map3 Bounds3D lengthInterval lengthInterval lengthInterval

vectorBounds2D :: Generator (VectorBounds2D Meters space)
vectorBounds2D = Random.map2 VectorBounds2D lengthInterval lengthInterval

line2D :: Tolerance Meters => Generator (Curve2D Meters space)
line2D = Random.map2 Curve2D.lineFrom point2D point2D

arc2D :: Tolerance Meters => Generator (Curve2D Meters space)
arc2D = do
  startPoint <- point2D
  endPoint <- point2D
  angleSign <- Sign.random
  angleMagnitude <- Quantity.random (Angle.degrees 5) (Angle.degrees 355)
  let sweptAngle = angleSign .*. angleMagnitude
  Random.return (Curve2D.arcFrom startPoint endPoint sweptAngle)

quadraticSpline2D :: Tolerance Meters => Generator (Curve2D Meters space)
quadraticSpline2D = Random.map3 Curve2D.quadraticBezier point2D point2D point2D

cubicSpline2D :: Tolerance Meters => Generator (Curve2D Meters space)
cubicSpline2D = Random.map4 Curve2D.cubicBezier point2D point2D point2D point2D

translation2D :: Generator (Transform2D.Rigid Meters space)
translation2D = Random.map Transform2D.translateBy vector2D

rotation2D :: Generator (Transform2D.Rigid Meters space)
rotation2D = do
  centerPoint <- point2D
  angle <- Quantity.random (Angle.degrees -360) (Angle.degrees 360)
  Random.return (Transform2D.rotateAround centerPoint angle)

mirror2D :: Generator (Transform2D.Orthonormal Meters space)
mirror2D = Random.map Transform2D.mirrorAcross axis2D

rigidTransform2D :: Generator (Transform2D.Rigid Meters space)
rigidTransform2D = Random.merge (NonEmpty.two translation2D rotation2D)

orthonormalTransform2D :: Generator (Transform2D.Orthonormal Meters space)
orthonormalTransform2D =
  Random.merge $
    NonEmpty.three
      (Random.map Transform2D.toOrthonormal translation2D)
      (Random.map Transform2D.toOrthonormal rotation2D)
      mirror2D

scalingFactor :: Generator Number
scalingFactor = Number.random 0.5 2

uniformScaling2D :: Generator (Transform2D.Uniform Meters space)
uniformScaling2D = Random.map2 Transform2D.scaleAbout point2D scalingFactor

uniformTransform2D :: Generator (Transform2D.Uniform Meters space)
uniformTransform2D =
  Random.merge $
    NonEmpty.four
      (Random.map Transform2D.toUniform translation2D)
      (Random.map Transform2D.toUniform rotation2D)
      (Random.map Transform2D.toUniform mirror2D)
      uniformScaling2D

nonUniformScaling2D :: Generator (Transform2D.Affine Meters space)
nonUniformScaling2D = Random.map2 Transform2D.scaleAlong axis2D scalingFactor

affineTransform2D :: Generator (Transform2D.Affine Meters space)
affineTransform2D =
  Random.merge $
    NonEmpty.five
      (Random.map Transform2D.toAffine translation2D)
      (Random.map Transform2D.toAffine rotation2D)
      (Random.map Transform2D.toAffine mirror2D)
      (Random.map Transform2D.toAffine uniformScaling2D)
      nonUniformScaling2D

surfaceParameter :: Generator SurfaceParameter
surfaceParameter = Random.oneOf (NonEmpty.two U V)

translation3D :: Generator (Transform3D.Rigid space)
translation3D = Random.map Transform3D.translateBy vector3D

rotation3D :: Generator (Transform3D.Rigid space)
rotation3D = do
  axis <- axis3D
  angle <- Quantity.random (Angle.degrees -360) (Angle.degrees 360)
  Random.return (Transform3D.rotateAround axis angle)

mirror3D :: Generator (Transform3D.Orthonormal space)
mirror3D = Random.map Transform3D.mirrorAcross plane3D

rigidTransform3D :: Generator (Transform3D.Rigid space)
rigidTransform3D = Random.merge (NonEmpty.two translation3D rotation3D)

orthonormalTransform3D :: Generator (Transform3D.Orthonormal space)
orthonormalTransform3D =
  Random.merge $
    NonEmpty.three
      (Random.map Transform3D.toOrthonormal translation3D)
      (Random.map Transform3D.toOrthonormal rotation3D)
      mirror3D

uniformScaling3D :: Generator (Transform3D.Uniform space)
uniformScaling3D = Random.map2 Transform3D.scaleAbout point3D scalingFactor

uniformTransform3D :: Generator (Transform3D.Uniform space)
uniformTransform3D =
  Random.merge $
    NonEmpty.four
      (Random.map Transform3D.toUniform translation3D)
      (Random.map Transform3D.toUniform rotation3D)
      (Random.map Transform3D.toUniform mirror3D)
      uniformScaling3D

nonUniformScaling3D :: Generator (Transform3D.Affine space)
nonUniformScaling3D = Random.map2 Transform3D.scaleAlong axis3D scalingFactor

affineTransform3D :: Generator (Transform3D.Affine space)
affineTransform3D =
  Random.merge $
    NonEmpty.five
      (Random.map Transform3D.toAffine translation3D)
      (Random.map Transform3D.toAffine rotation3D)
      (Random.map Transform3D.toAffine mirror3D)
      (Random.map Transform3D.toAffine uniformScaling3D)
      nonUniformScaling3D
