module Tests.Random
  ( length
  , lengthBounds
  , point2d
  , point3d
  , vector2d
  , vector3d
  , vectorBounds3d
  , axis2d
  , axis3d
  , plane3d
  , orientation2d
  , planeOrientation3d
  , orientation3d
  , frame2d
  , frame3d
  , bounds2d
  , bounds3d
  , vectorBounds2d
  , line2d
  , arc2d
  , quadraticSpline2d
  , cubicSpline2d
  , rigidTransform2d
  , rigidTransform3d
  , orthonormalTransform2d
  , orthonormalTransform3d
  , uniformTransform2d
  , uniformTransform3d
  , affineTransform2d
  , affineTransform3d
  , surfaceParameter
  )
where

import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis2d (Axis2d (Axis2d))
import OpenSolid.Axis3d (Axis3d (Axis3d))
import OpenSolid.Bounds (Bounds)
import OpenSolid.Bounds qualified as Bounds
import OpenSolid.Bounds2d (Bounds2d (Bounds2d))
import OpenSolid.Bounds3d (Bounds3d)
import OpenSolid.Curve2d (Curve2d)
import OpenSolid.Curve2d qualified as Curve2d
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Frame2d (Frame2d)
import OpenSolid.Frame2d qualified as Frame2d
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.NonEmpty qualified as NonEmpty
import OpenSolid.Number qualified as Number
import OpenSolid.Orientation2d (Orientation2d)
import OpenSolid.Orientation2d qualified as Orientation2d
import OpenSolid.Orientation3d (Orientation3d)
import OpenSolid.Plane3d (Plane3d (Plane3d))
import OpenSolid.PlaneOrientation3d (PlaneOrientation3d)
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point2d (Point2d (Point2d))
import OpenSolid.Point3d (Point3d)
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Bounds3d (Bounds3d)
  , Point3d (Point3d)
  , Vector3d (Vector3d)
  , VectorBounds3d (VectorBounds3d)
  )
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Random (Generator)
import OpenSolid.Random qualified as Random
import OpenSolid.Sign qualified as Sign
import OpenSolid.SurfaceParameter (SurfaceParameter (U, V))
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform2d qualified as Transform2d
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector2d (Vector2d (Vector2d))
import OpenSolid.VectorBounds2d (VectorBounds2d (VectorBounds2d))

length :: Generator Length
length = Quantity.random (Length.meters -10.0) (Length.meters 10.0)

lengthBounds :: Generator (Bounds Meters)
lengthBounds = Bounds.random length

point2d :: Generator (Point2d (space @ Meters))
point2d = Random.map2 Point2d length length

point3d :: Generator (Point3d (space @ Meters))
point3d = Random.map3 Point3d length length length

vector2d :: Generator (Vector2d (space @ Meters))
vector2d = Random.map2 Vector2d length length

vector3d :: Generator (Vector3d (space @ Meters))
vector3d = Random.map3 Vector3d length length length

vectorBounds3d :: Generator (VectorBounds3d (space @ Meters))
vectorBounds3d =
  Random.map3 VectorBounds3d
    @ lengthBounds
    @ lengthBounds
    @ lengthBounds

axis2d :: Generator (Axis2d (space @ Meters))
axis2d = Random.map2 Axis2d point2d Direction2d.random

axis3d :: Generator (Axis3d (space @ Meters))
axis3d = Random.map2 Axis3d point3d Direction3d.random

orientation2d :: Generator (Orientation2d global)
orientation2d = Random.map Orientation2d.fromXDirection Direction2d.random

planeOrientation3d :: Generator (PlaneOrientation3d global)
planeOrientation3d =
  Random.retry $
    Random.map2 (Tolerance.using 0.1 PlaneOrientation3d.fromDirections)
      @ Direction3d.random
      @ Direction3d.random

orientation3d :: Generator (Orientation3d global)
orientation3d = Random.map Frame3d.orientation frame3d

plane3d :: Generator (Plane3d (global @ Meters) (Defines local))
plane3d = Random.map2 Plane3d point3d planeOrientation3d

frame2d :: Generator (Frame2d (global @ Meters) (Defines local))
frame2d = Random.map Frame2d.fromXAxis axis2d

frame3d :: Generator (Frame3d (global @ Meters) (Defines local))
frame3d = Random.map Frame3d.fromTopPlane plane3d

bounds2d :: Generator (Bounds2d (space @ Meters))
bounds2d = Random.map2 Bounds2d lengthBounds lengthBounds

bounds3d :: Generator (Bounds3d (space @ Meters))
bounds3d = Random.map3 Bounds3d lengthBounds lengthBounds lengthBounds

vectorBounds2d :: Generator (VectorBounds2d (space @ Meters))
vectorBounds2d = Random.map2 VectorBounds2d lengthBounds lengthBounds

line2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
line2d = Random.map2 Curve2d.line point2d point2d

arc2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
arc2d = do
  startPoint <- point2d
  endPoint <- point2d
  angleSign <- Sign.random
  angleMagnitude <- Quantity.random (Angle.degrees 5.0) (Angle.degrees 355.0)
  let sweptAngle = angleSign .*. angleMagnitude
  Random.return (Curve2d.arc startPoint endPoint sweptAngle)

quadraticSpline2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
quadraticSpline2d = Random.map3 Curve2d.quadraticBezier point2d point2d point2d

cubicSpline2d :: Tolerance Meters => Generator (Curve2d (space @ Meters))
cubicSpline2d = Random.map4 Curve2d.cubicBezier point2d point2d point2d point2d

translation2d :: Generator (Transform2d.Rigid (space @ Meters))
translation2d = Random.map Transform2d.translateBy vector2d

rotation2d :: Generator (Transform2d.Rigid (space @ Meters))
rotation2d = do
  centerPoint <- point2d
  angle <- Quantity.random (Angle.degrees -360.0) (Angle.degrees 360.0)
  Random.return (Transform2d.rotateAround centerPoint angle)

mirror2d :: Generator (Transform2d.Orthonormal (space @ Meters))
mirror2d = Random.map Transform2d.mirrorAcross axis2d

rigidTransform2d :: Generator (Transform2d.Rigid (space @ Meters))
rigidTransform2d = Random.merge (NonEmpty.two translation2d rotation2d)

orthonormalTransform2d :: Generator (Transform2d.Orthonormal (space @ Meters))
orthonormalTransform2d =
  Random.merge $
    NonEmpty.three
      @ Random.map Transform2d.toOrthonormal translation2d
      @ Random.map Transform2d.toOrthonormal rotation2d
      @ mirror2d

scalingFactor :: Generator Number
scalingFactor = Number.random 0.5 2.0

uniformScaling2d :: Generator (Transform2d.Uniform (space @ Meters))
uniformScaling2d = Random.map2 Transform2d.scaleAbout point2d scalingFactor

uniformTransform2d :: Generator (Transform2d.Uniform (space @ Meters))
uniformTransform2d =
  Random.merge $
    NonEmpty.four
      @ Random.map Transform2d.toUniform translation2d
      @ Random.map Transform2d.toUniform rotation2d
      @ Random.map Transform2d.toUniform mirror2d
      @ uniformScaling2d

nonUniformScaling2d :: Generator (Transform2d.Affine (space @ Meters))
nonUniformScaling2d = Random.map2 Transform2d.scaleAlong axis2d scalingFactor

affineTransform2d :: Generator (Transform2d.Affine (space @ Meters))
affineTransform2d =
  Random.merge $
    NonEmpty.five
      @ Random.map Transform2d.toAffine translation2d
      @ Random.map Transform2d.toAffine rotation2d
      @ Random.map Transform2d.toAffine mirror2d
      @ Random.map Transform2d.toAffine uniformScaling2d
      @ nonUniformScaling2d

surfaceParameter :: Generator SurfaceParameter
surfaceParameter = Random.oneOf (NonEmpty.two U V)

translation3d :: Generator (Transform3d.Rigid (space @ Meters))
translation3d = Random.map Transform3d.translateBy vector3d

rotation3d :: Generator (Transform3d.Rigid (space @ Meters))
rotation3d = do
  axis <- axis3d
  angle <- Quantity.random (Angle.degrees -360.0) (Angle.degrees 360.0)
  Random.return (Transform3d.rotateAround axis angle)

mirror3d :: Generator (Transform3d.Orthonormal (space @ Meters))
mirror3d = Random.map Transform3d.mirrorAcross plane3d

rigidTransform3d :: Generator (Transform3d.Rigid (space @ Meters))
rigidTransform3d = Random.merge (NonEmpty.two translation3d rotation3d)

orthonormalTransform3d :: Generator (Transform3d.Orthonormal (space @ Meters))
orthonormalTransform3d =
  Random.merge $
    NonEmpty.three
      @ Random.map Transform3d.toOrthonormal translation3d
      @ Random.map Transform3d.toOrthonormal rotation3d
      @ mirror3d

uniformScaling3d :: Generator (Transform3d.Uniform (space @ Meters))
uniformScaling3d = Random.map2 Transform3d.scaleAbout point3d scalingFactor

uniformTransform3d :: Generator (Transform3d.Uniform (space @ Meters))
uniformTransform3d =
  Random.merge $
    NonEmpty.four
      @ Random.map Transform3d.toUniform translation3d
      @ Random.map Transform3d.toUniform rotation3d
      @ Random.map Transform3d.toUniform mirror3d
      @ uniformScaling3d

nonUniformScaling3d :: Generator (Transform3d.Affine (space @ Meters))
nonUniformScaling3d = Random.map2 Transform3d.scaleAlong axis3d scalingFactor

affineTransform3d :: Generator (Transform3d.Affine (space @ Meters))
affineTransform3d =
  Random.merge $
    NonEmpty.five
      @ Random.map Transform3d.toAffine translation3d
      @ Random.map Transform3d.toAffine rotation3d
      @ Random.map Transform3d.toAffine mirror3d
      @ Random.map Transform3d.toAffine uniformScaling3d
      @ nonUniformScaling3d
