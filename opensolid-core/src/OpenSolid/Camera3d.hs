module OpenSolid.Camera3d
  ( Camera3d (frame, focalDistance, projection)
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
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Frame3d (Frame3d (Frame3d))
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Length (Length)
import OpenSolid.Number qualified as Number
import OpenSolid.Plane3d (Plane3d (Plane3d))
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Quantity qualified as Quantity
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d
import OpenSolid.World3d qualified as World3d

data CameraSpace

data ScreenSpace

-- | A perspective or orthographic camera in 3D.
data Camera3d space = Camera3d
  { frame :: Frame3d space Meters (Defines CameraSpace)
  , focalDistance :: Length
  , projection :: Projection
  }

instance HasField "eyePoint" (Camera3d space) (Point3d space Meters) where
  getField camera = camera.frame.originPoint

instance HasField "forwardDirection" (Camera3d space) (Direction3d space) where
  getField camera = camera.frame.forwardDirection

instance HasField "backwardDirection" (Camera3d space) (Direction3d space) where
  getField camera = camera.frame.backwardDirection

instance HasField "leftwardDirection" (Camera3d space) (Direction3d space) where
  getField camera = camera.frame.leftwardDirection

instance HasField "rightwardDirection" (Camera3d space) (Direction3d space) where
  getField camera = camera.frame.rightwardDirection

instance HasField "upwardDirection" (Camera3d space) (Direction3d space) where
  getField camera = camera.frame.upwardDirection

instance HasField "downwardDirection" (Camera3d space) (Direction3d space) where
  getField camera = camera.frame.downwardDirection

instance HasField "focalPoint" (Camera3d space) (Point3d space Meters) where
  getField camera = camera.eyePoint .+. camera.focalDistance .*. camera.forwardDirection

instance
  HasField
    "viewPlane"
    (Camera3d space)
    (Plane3d space Meters (Defines ScreenSpace))
  where
  getField camera = Frame3d.backPlane camera.frame

instance FFI (Camera3d FFI.Space) where
  representation = FFI.classRepresentation "Camera3d"

-- | What kind of projection (perspective or orthographic) a camera should use.
data Projection
  = Perspective Angle
  | Orthographic Length

instance FFI Projection where
  representation = FFI.nestedClassRepresentation "Camera3d" "Projection"

-- | Define a perspective projection with a given vertical field of view.
perspective :: "verticalFov" ::: Angle -> Projection
perspective (Named verticalFov) = Perspective verticalFov

-- | Define an orthographic projection with a given viewport height.
orthographic :: "viewportHeight" ::: Length -> Projection
orthographic (Named viewportHeight) = Orthographic viewportHeight

new :: Frame3d space Meters (Defines CameraSpace) -> Length -> Projection -> Camera3d space
new givenFrame givenFocalDistance projection =
  Camera3d
    { frame = givenFrame
    , focalDistance = givenFocalDistance
    , projection = projection
    }

{-| Construct a camera at a given point, looking at a given focal point.

The camera will be oriented such that its local up direction
will be as close as possible to the global up direction.
-}
lookAt ::
  "eyePoint" ::: Point3d space Meters ->
  "focalPoint" ::: Point3d space Meters ->
  "projection" ::: Projection ->
  Camera3d space
lookAt (Named eyePoint) (Named focalPoint) (Named projection) = do
  let computedFocalDistance = Point3d.distanceFrom eyePoint focalPoint
  let computedFrame =
        case Tolerance.using Quantity.zero (Vector3d.direction (focalPoint .-. eyePoint)) of
          Ok computedForwardDirection -> do
            let viewVector = Vector3d.unit computedForwardDirection
            let upVector = Vector3d.unit World3d.upwardDirection
            case Tolerance.using 1e-9 (PlaneOrientation3d.fromVectors viewVector upVector) of
              Just rightPlaneOrientation ->
                Frame3d.fromRightPlane (Plane3d eyePoint rightPlaneOrientation)
              Nothing -- View direction is either straight up or straight down
                | Direction3d.upwardComponent computedForwardDirection > 0 ->
                    Frame3d eyePoint World3d.upwardOrientation
                | otherwise ->
                    Frame3d eyePoint World3d.downwardOrientation
          Error Vector3d.IsZero ->
            -- Given eye and focal points are coincident,
            -- so just look straight forward
            Frame3d eyePoint World3d.forwardOrientation
  new computedFrame computedFocalDistance projection

{-| Construct a camera orbiting around a given focal point, a given distance away.

The azimuth is the horizontal angle towards the camera from the focal point,
measured clockwise from the global forward direction.
The elevation is the vertical angle towards the camera from the focal point,
measure upwards from the global top plane.
-}
orbit ::
  "focalPoint" ::: Point3d space Meters ->
  "azimuth" ::: Angle ->
  "elevation" ::: Angle ->
  "distance" ::: Length ->
  "projection" ::: Projection ->
  Camera3d space
orbit (Named focalPoint) (Named azimuth) (Named elevation) (Named distance) (Named projection) = do
  let computedFrame =
        Frame3d focalPoint World3d.backwardOrientation
          & Frame3d.turnRightBy azimuth
          & Frame3d.tiltDownBy elevation
          & Frame3d.offsetBackwardBy distance
  new computedFrame distance projection

isometricElevation :: Angle
isometricElevation = Angle.atan2 1 (Number.sqrt 2)

isometric :: Point3d space Meters -> Length -> Projection -> Camera3d space
isometric givenFocalPoint distance givenProjection =
  orbit
    (#focalPoint givenFocalPoint)
    (#azimuth (Angle.degrees 45))
    (#elevation isometricElevation)
    (#distance distance)
    (#projection givenProjection)

moveTo :: Point3d space Meters -> Camera3d space -> Camera3d space
moveTo newEyePoint Camera3d{frame, focalDistance, projection} =
  Camera3d{frame = Frame3d.moveTo newEyePoint frame, focalDistance, projection}

placeIn :: Frame3d global Meters (Defines local) -> Camera3d local -> Camera3d global
placeIn givenFrame Camera3d{frame, focalDistance, projection} =
  Camera3d{frame = Frame3d.placeIn givenFrame frame, focalDistance, projection}

relativeTo :: Frame3d global Meters (Defines local) -> Camera3d global -> Camera3d local
relativeTo givenFrame = placeIn (Frame3d.inverse givenFrame)

transformBy :: Transform3d.Rigid space Meters -> Camera3d space -> Camera3d space
transformBy transform Camera3d{frame, focalDistance, projection} =
  Camera3d{frame = Frame3d.transformBy transform frame, focalDistance, projection}

translateBy :: Vector3d space Meters -> Camera3d space -> Camera3d space
translateBy = Transform3d.translateByImpl transformBy

translateIn :: Direction3d space -> Length -> Camera3d space -> Camera3d space
translateIn = Transform3d.translateInImpl transformBy

translateAlong :: Axis3d space Meters -> Length -> Camera3d space -> Camera3d space
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround :: Axis3d space Meters -> Angle -> Camera3d space -> Camera3d space
rotateAround = Transform3d.rotateAroundImpl transformBy
