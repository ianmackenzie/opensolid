module OpenSolid.Camera3D
  ( Camera3D (frame, focalDistance, projection)
  , CameraSpace
  , ScreenSpace
  , Projection (Perspective, Orthographic)
  , perspective
  , orthographic
  , new
  , lookAt
  , orbit
  , isometric
  , isometricElevation
  , moveTo
  , placeIn
  , relativeTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  )
where

import GHC.Records (HasField (getField))
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

data ScreenSpace

-- | A perspective or orthographic camera in 3D.
data Camera3D space = Camera3D
  { frame :: Frame3D space CameraSpace
  , focalDistance :: Length
  , projection :: Projection
  }

instance HasField "eyePoint" (Camera3D space) (Point3D space) where
  getField camera = camera.frame.originPoint

instance HasField "forwardDirection" (Camera3D space) (Direction3D space) where
  getField camera = camera.frame.forwardDirection

instance HasField "backwardDirection" (Camera3D space) (Direction3D space) where
  getField camera = camera.frame.backwardDirection

instance HasField "leftwardDirection" (Camera3D space) (Direction3D space) where
  getField camera = camera.frame.leftwardDirection

instance HasField "rightwardDirection" (Camera3D space) (Direction3D space) where
  getField camera = camera.frame.rightwardDirection

instance HasField "upwardDirection" (Camera3D space) (Direction3D space) where
  getField camera = camera.frame.upwardDirection

instance HasField "downwardDirection" (Camera3D space) (Direction3D space) where
  getField camera = camera.frame.downwardDirection

instance HasField "focalPoint" (Camera3D space) (Point3D space) where
  getField camera = camera.eyePoint .+. camera.focalDistance .*. camera.forwardDirection

instance
  HasField
    "viewPlane"
    (Camera3D space)
    (Plane3D space ScreenSpace)
  where
  getField camera = Frame3D.backPlane camera.frame

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
perspective (Named verticalFov) = Perspective verticalFov

-- | Define an orthographic projection with a given viewport height.
orthographic :: "viewportHeight" ::: Length -> Projection
orthographic (Named viewportHeight) = Orthographic viewportHeight

new :: Frame3D space CameraSpace -> Length -> Projection -> Camera3D space
new givenFrame givenFocalDistance projection =
  Camera3D
    { frame = givenFrame
    , focalDistance = givenFocalDistance
    , projection = projection
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
lookAt (Named eyePoint) (Named focalPoint) (Named projection) = do
  let computedFocalDistance = Point3D.distanceFrom eyePoint focalPoint
  let computedFrame =
        case Tolerance.using Quantity.zero (Vector3D.direction (focalPoint .-. eyePoint)) of
          Ok computedForwardDirection -> do
            let viewVector = Vector3D.unit computedForwardDirection
            let upVector = Vector3D.unit World3D.upwardDirection
            case Tolerance.using 1e-9 (PlaneOrientation3D.fromVectors viewVector upVector) of
              Just rightPlaneOrientation ->
                Frame3D.fromRightPlane (Plane3D eyePoint rightPlaneOrientation)
              Nothing -- View direction is either straight up or straight down
                | Direction3D.upwardComponent computedForwardDirection > 0 ->
                    Frame3D eyePoint World3D.upwardOrientation
                | otherwise ->
                    Frame3D eyePoint World3D.downwardOrientation
          Error Vector.IsZero ->
            -- Given eye and focal points are coincident,
            -- so just look straight forward
            Frame3D eyePoint World3D.forwardOrientation
  new computedFrame computedFocalDistance projection

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
orbit (Named focalPoint) (Named azimuth) (Named elevation) (Named distance) (Named projection) = do
  let computedFrame =
        Frame3D focalPoint World3D.backwardOrientation
          & Frame3D.turnRightBy azimuth
          & Frame3D.tiltDownBy elevation
          & Frame3D.offsetBackwardBy distance
  new computedFrame distance projection

isometricElevation :: Angle
isometricElevation = Angle.atan2 1 (Number.sqrt 2)

isometric :: Point3D space -> Length -> Projection -> Camera3D space
isometric givenFocalPoint distance givenProjection =
  orbit
    (#focalPoint givenFocalPoint)
    (#azimuth (Angle.degrees 45))
    (#elevation isometricElevation)
    (#distance distance)
    (#projection givenProjection)

moveTo :: Point3D space -> Camera3D space -> Camera3D space
moveTo newEyePoint Camera3D{frame, focalDistance, projection} =
  Camera3D{frame = Frame3D.moveTo newEyePoint frame, focalDistance, projection}

placeIn :: Frame3D global local -> Camera3D local -> Camera3D global
placeIn givenFrame Camera3D{frame, focalDistance, projection} =
  Camera3D{frame = Frame3D.placeIn givenFrame frame, focalDistance, projection}

relativeTo :: Frame3D global local -> Camera3D global -> Camera3D local
relativeTo givenFrame = placeIn (Frame3D.inverse givenFrame)

transformBy :: Transform3D.Rigid space -> Camera3D space -> Camera3D space
transformBy transform Camera3D{frame, focalDistance, projection} =
  Camera3D{frame = Frame3D.transformBy transform frame, focalDistance, projection}

translateBy :: Vector3D Meters space -> Camera3D space -> Camera3D space
translateBy = Transform3D.translateByImpl transformBy

translateIn :: Direction3D space -> Length -> Camera3D space -> Camera3D space
translateIn = Transform3D.translateInImpl transformBy

translateAlong :: Axis3D space -> Length -> Camera3D space -> Camera3D space
translateAlong = Transform3D.translateAlongImpl transformBy

rotateAround :: Axis3D space -> Angle -> Camera3D space -> Camera3D space
rotateAround = Transform3D.rotateAroundImpl transformBy
