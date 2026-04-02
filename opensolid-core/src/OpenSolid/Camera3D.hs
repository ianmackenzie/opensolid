module OpenSolid.Camera3D
  ( Camera3D
  , CameraSpace
  , Projection (Perspective, Orthographic)
  , perspective
  , orthographic
  , new
  , lookAt
  , orbit
  , isometric
  , isometricElevation
  , moveTo
  , frame
  , focalDistance
  , projection
  , eyePoint
  , forwardDirection
  , backwardDirection
  , leftwardDirection
  , rightwardDirection
  , upwardDirection
  , downwardDirection
  , focalPoint
  , viewPlane
  , placeIn
  , relativeTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis3D (Axis3D)
import OpenSolid.Direction3D (Direction3D)
import OpenSolid.Direction3D qualified as Direction3D
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3D (Frame3D (Frame3D))
import OpenSolid.Frame3D qualified as Frame3D
import OpenSolid.Length (Length)
import OpenSolid.Number qualified as Number
import OpenSolid.Plane3D (Plane3D (Plane3D))
import OpenSolid.PlaneOrientation3D qualified as PlaneOrientation3D
import OpenSolid.Point3D (Point3D)
import OpenSolid.Point3D qualified as Point3D
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3D qualified as Transform3D
import OpenSolid.Vector qualified as Vector
import OpenSolid.Vector3D (Vector3D)
import OpenSolid.Vector3D qualified as Vector3D
import OpenSolid.World3D qualified as World3D

data CameraSpace

-- | A perspective or orthographic camera in 3D.
data Camera3D space = Camera3D
  { frame :: Frame3D space CameraSpace
  , focalDistance :: Length
  , projection :: Projection
  }

instance FFI (Camera3D FFI.Space) where
  representation = FFI.classRepresentation "Camera3D"

-- | What kind of projection (perspective or orthographic) a camera should use.
data Projection
  = Perspective Angle
  | Orthographic Length

instance FFI Projection where
  representation = FFI.nestedClassRepresentation "Camera3D" "Projection"

-- | Define a perspective projection with a given vertical field of view.
perspective :: "verticalFov" ::: Angle -> Projection
perspective ("verticalFov" ::: verticalFov) = Perspective verticalFov

-- | Define an orthographic projection with a given viewport height.
orthographic :: "viewportHeight" ::: Length -> Projection
orthographic ("viewportHeight" ::: viewportHeight) = Orthographic viewportHeight

new :: Frame3D space CameraSpace -> Length -> Projection -> Camera3D space
new givenFrame givenFocalDistance givenProjection =
  Camera3D
    { frame = givenFrame
    , focalDistance = givenFocalDistance
    , projection = givenProjection
    }

{-| Construct a camera at a given point, looking at a given focal point.

The camera will be oriented such that its local up direction
will be as close as possible to the global up direction.
-}
lookAt ::
  "eyePoint" ::: Point3D space ->
  "focalPoint" ::: Point3D space ->
  "projection" ::: Projection ->
  Camera3D space
lookAt
  ("eyePoint" ::: givenEyePoint)
  ("focalPoint" ::: givenFocalPoint)
  ("projection" ::: givenProjection) = do
    let computedFocalDistance = Point3D.distanceFrom givenEyePoint givenFocalPoint
    let computedFrame =
          case Tolerance.using Quantity.zero (Vector3D.direction (givenFocalPoint - givenEyePoint)) of
            Ok computedForwardDirection -> do
              case PlaneOrientation3D.fromDirections computedForwardDirection World3D.upwardDirection of
                Just rightPlaneOrientation ->
                  Frame3D.fromRightPlane (Plane3D givenEyePoint rightPlaneOrientation)
                Nothing -- View direction is either straight up or straight down
                  | Direction3D.upwardComponent computedForwardDirection > 0.0 ->
                      Frame3D givenEyePoint World3D.upwardOrientation
                  | otherwise ->
                      Frame3D givenEyePoint World3D.downwardOrientation
            Error Vector.IsZero ->
              -- Given eye and focal points are coincident,
              -- so just look straight forward
              Frame3D givenEyePoint World3D.forwardOrientation
    new computedFrame computedFocalDistance givenProjection

{-| Construct a camera orbiting around a given focal point, a given distance away.

The azimuth is the horizontal angle towards the camera from the focal point,
measured clockwise from the global forward direction.
The elevation is the vertical angle towards the camera from the focal point,
measure upwards from the global top plane.
-}
orbit ::
  "focalPoint" ::: Point3D space ->
  "azimuth" ::: Angle ->
  "elevation" ::: Angle ->
  "distance" ::: Length ->
  "projection" ::: Projection ->
  Camera3D space
orbit
  ("focalPoint" ::: givenFocalPoint)
  ("azimuth" ::: azimuth)
  ("elevation" ::: elevation)
  ("distance" ::: distance)
  ("projection" ::: givenProjection) = do
    let computedFrame =
          Frame3D givenFocalPoint World3D.backwardOrientation
            & Frame3D.turnRightBy azimuth
            & Frame3D.tiltDownBy elevation
            & Frame3D.offsetBackwardBy distance
    new computedFrame distance givenProjection

isometricElevation :: Angle
isometricElevation = Angle.atan2 1.0 (Number.sqrt 2.0)

isometric :: Point3D space -> Length -> Projection -> Camera3D space
isometric givenFocalPoint distance givenProjection =
  orbit
    (#focalPoint givenFocalPoint)
    (#azimuth (Angle.degrees 45.0))
    (#elevation isometricElevation)
    (#distance distance)
    (#projection givenProjection)

frame :: Camera3D space -> Frame3D space CameraSpace
frame = (.frame)

focalDistance :: Camera3D space -> Length
focalDistance = (.focalDistance)

projection :: Camera3D space -> Projection
projection = (.projection)

eyePoint :: Camera3D space -> Point3D space
eyePoint camera = Frame3D.originPoint (frame camera)

forwardDirection :: Camera3D space -> Direction3D space
forwardDirection camera = Frame3D.forwardDirection (frame camera)

backwardDirection :: Camera3D space -> Direction3D space
backwardDirection camera = Frame3D.backwardDirection (frame camera)

leftwardDirection :: Camera3D space -> Direction3D space
leftwardDirection camera = Frame3D.leftwardDirection (frame camera)

rightwardDirection :: Camera3D space -> Direction3D space
rightwardDirection camera = Frame3D.rightwardDirection (frame camera)

upwardDirection :: Camera3D space -> Direction3D space
upwardDirection camera = Frame3D.upwardDirection (frame camera)

downwardDirection :: Camera3D space -> Direction3D space
downwardDirection camera = Frame3D.downwardDirection (frame camera)

focalPoint :: Camera3D space -> Point3D space
focalPoint camera = eyePoint camera + focalDistance camera * forwardDirection camera

viewPlane :: Camera3D space -> Plane3D space
viewPlane camera = Frame3D.backPlane (frame camera)

moveTo :: Point3D space -> Camera3D space -> Camera3D space
moveTo newEyePoint camera =
  Camera3D
    { frame = Frame3D.moveTo newEyePoint camera.frame
    , focalDistance = camera.focalDistance
    , projection = camera.projection
    }

placeIn :: Frame3D global local -> Camera3D local -> Camera3D global
placeIn givenFrame camera =
  Camera3D
    { frame = Frame3D.placeIn givenFrame camera.frame
    , focalDistance = camera.focalDistance
    , projection = camera.projection
    }

relativeTo :: Frame3D global local -> Camera3D global -> Camera3D local
relativeTo givenFrame = placeIn (Frame3D.inverse givenFrame)

transformBy :: Transform3D.Rigid space -> Camera3D space -> Camera3D space
transformBy transform camera =
  Camera3D
    { frame = Frame3D.transformBy transform camera.frame
    , focalDistance = camera.focalDistance
    , projection = camera.projection
    }

translateBy :: Vector3D Meters space -> Camera3D space -> Camera3D space
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D space -> Length -> Camera3D space -> Camera3D space
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D space -> Length -> Camera3D space -> Camera3D space
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D space -> Angle -> Camera3D space -> Camera3D space
rotateAround = Transform3D.rotateAroundImpl transformBy
