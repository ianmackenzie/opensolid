module OpenSolid.Ap242
  ( length
  , direction2D
  , direction3D
  , vector2D
  , vector3D
  , point2D
  , point3D
  , axisPlacement2D
  , axisPlacement3D
  )
where

import OpenSolid.Convention3D (Convention3D)
import OpenSolid.Convention3D qualified as Convention3D
import OpenSolid.Direction2D (Direction2D (Direction2D))
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.Frame2D (Frame2D)
import OpenSolid.Frame2D qualified as Frame2D
import OpenSolid.Length (Length)
import OpenSolid.Length qualified as Length
import OpenSolid.Plane3D (Plane3D)
import OpenSolid.Plane3D qualified as Plane3D
import OpenSolid.Point2D (Point2D (Point2D))
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Step qualified as Step
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector2D (Vector2D)
import OpenSolid.Vector2D qualified as Vector2D
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.World3D qualified as World3D

convention3D :: Convention3D
convention3D = Convention3D.zUp

length :: Length -> Step.Attribute
length value = Step.number (Length.inMeters value)

direction :: List Number -> Step.Entity
direction components = Step.entity "DIRECTION" [Step.text "", Step.list Step.number components]

direction2D :: Direction2D -> Step.Entity
direction2D (Direction2D x y) = direction [x, y]

direction3D :: Direction3D space -> Step.Entity
direction3D givenDirection = do
  let (x, y, z) = Direction3D.components convention3D givenDirection
  direction [x, y, z]

vector :: Step.Entity -> Length -> Step.Entity
vector givenDirection givenLength =
  Step.entity "VECTOR" [Step.text "", Step.referenceTo givenDirection, length givenLength]

vector2D :: Tolerance Meters => Vector2D Meters -> Step.Entity
vector2D givenVector =
  case Vector2D.magnitudeAndDirection givenVector of
    Ok (vectorLength, vectorDirection) -> vector (direction2D vectorDirection) vectorLength
    Error Vector.IsZero -> vector (direction2D Direction2D.x) Length.zero

vector3D :: Tolerance Meters => Vector3D Meters space -> Step.Entity
vector3D givenVector =
  case Vector3D.magnitudeAndDirection givenVector of
    Ok (vectorLength, vectorDirection) -> vector (direction3D vectorDirection) vectorLength
    Error Vector.IsZero -> vector (direction3D World3D.upwardDirection) Length.zero

cartesianPoint :: List Length -> Step.Entity
cartesianPoint coordinates =
  Step.entity "CARTESIAN_POINT" [Step.text "", Step.list length coordinates]

point2D :: Point2D Meters -> Step.Entity
point2D (Point2D x y) = cartesianPoint [x, y]

point3D :: Point3D space -> Step.Entity
point3D point = do
  let (x, y, z) = Point3D.coordinates convention3D point
  cartesianPoint [x, y, z]

axisPlacement2D :: Frame2D Meters -> Step.Entity
axisPlacement2D frame =
  Step.entity "AXIS2_PLACEMENT_2D" $
    [ Step.text ""
    , Step.referenceTo (point2D (Frame2D.originPoint frame))
    , Step.referenceTo (direction2D (Frame2D.xDirection frame))
    ]

axisPlacement3D :: Plane3D space -> Step.Entity
axisPlacement3D plane =
  Step.entity "AXIS2_PLACEMENT_3D" $
    [ Step.text ""
    , Step.referenceTo (point3D (Plane3D.originPoint plane))
    , Step.referenceTo (direction3D (Plane3D.normalDirection plane))
    , Step.referenceTo (direction3D (Plane3D.xDirection plane))
    ]
