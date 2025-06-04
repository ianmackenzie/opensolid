module OpenSolid.Camera3d
  ( Camera3d (frame, focalDistance, projection, fovAngle)
  , CameraSpace
  , ScreenSpace
  , Projection (Perspective, Orthographic)
  , FieldOfView
  , angle
  , height
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

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis3d (Axis3d)
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame3d (Frame3d (Frame3d))
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.Plane3d (Plane3d (Plane3d))
import OpenSolid.PlaneOrientation3d qualified as PlaneOrientation3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

data CameraSpace

type ScreenSpace = BackPlane CameraSpace

data Camera3d (coordinateSystem :: CoordinateSystem) where
  Camera3d ::
    { frame :: Frame3d (space @ units) (Defines CameraSpace)
    , focalDistance :: Qty units
    , projection :: Projection
    , fovAngle :: Angle
    } ->
    Camera3d (space @ units)

instance HasField "eyePoint" (Camera3d (space @ units)) (Point3d (space @ units)) where
  getField camera = Frame3d.originPoint camera.frame

instance HasField "viewDirection" (Camera3d (space @ units)) (Direction3d space) where
  getField camera = Frame3d.forwardDirection camera.frame

instance
  HasField
    "viewPlane"
    (Camera3d (space @ units))
    (Plane3d (space @ units) (Defines ScreenSpace))
  where
  getField camera = camera.frame.backPlane

instance HasField "frustumSlope" (Camera3d (space @ units)) Float where
  getField camera = Angle.tan (0.5 * camera.fovAngle)

instance HasField "fovHeight" (Camera3d (space @ units)) (Qty units) where
  getField camera = 2.0 * camera.focalDistance * camera.frustumSlope

data Projection
  = Perspective
  | Orthographic

data FieldOfView units
  = Angle Angle
  | Height (Qty units)

{-| Specify vertical field of view as an angle. For an orthographic camera, this will be converted
to a height at the camera's focal distance.
-}
angle :: Angle -> FieldOfView units
angle = Angle

{-| Specify vertical field of view as a height. For a perspective camera, this will be converted to
an angle at the camera's focal distance.
-}
height :: Qty units -> FieldOfView units
height = Height

new ::
  Frame3d (space @ units) (Defines CameraSpace) ->
  Qty units ->
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
new givenFrame givenFocalDistance projection givenFieldOfView =
  Camera3d
    { frame = givenFrame
    , focalDistance = givenFocalDistance
    , projection = projection
    , fovAngle = case givenFieldOfView of
        Angle givenAngle -> givenAngle
        Height givenHeight -> 2.0 * Angle.atan2 (0.5 * givenHeight) givenFocalDistance
    }

lookAt ::
  Tolerance units =>
  Point3d (space @ units) -> -- Eye point
  Point3d (space @ units) -> -- Focal point
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
lookAt givenEyePoint givenFocalPoint givenProjection givenFieldOfView = do
  let world = Frame3d.world
  let computedFocalDistance = Point3d.distanceFrom givenEyePoint givenFocalPoint
  let computedFrame =
        case Vector3d.direction (givenFocalPoint - givenEyePoint) of
          Success computedViewDirection -> do
            let viewVector = Vector3d.unit computedViewDirection
            let upVector = Vector3d.unit world.upwardDirection
            case Tolerance.using 1e-9 (PlaneOrientation3d.fromVectors viewVector upVector) of
              Just rightPlaneOrientation ->
                Frame3d.fromRightPlane (Plane3d givenEyePoint rightPlaneOrientation)
              Nothing -- View direction is either straight up or straight down
                | Direction3d.upwardComponent computedViewDirection > 0.0 ->
                    Frame3d givenEyePoint world.upwardOrientation
                | otherwise ->
                    Frame3d givenEyePoint world.downwardOrientation
          Failure Vector3d.IsZero ->
            -- Given eye and focal points are coincident,
            -- so just look straight forward
            Frame3d givenEyePoint world.orientation
  new computedFrame computedFocalDistance givenProjection givenFieldOfView

orbit ::
  Point3d (space @ units) ->
  "azimuth" ::: Angle ->
  "elevation" ::: Angle ->
  Qty units ->
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
orbit focalPoint (Field azimuth) (Field elevation) distance givenProjection givenFieldOfView = do
  let world = Frame3d.world
  let computedFrame =
        Frame3d focalPoint world.backwardOrientation
          |> Frame3d.turnRightwardBy azimuth
          |> Frame3d.tiltDownwardBy elevation
          |> Frame3d.offsetBackwardBy distance
  new computedFrame distance givenProjection givenFieldOfView

isometricElevation :: Angle
isometricElevation = Angle.atan2 1.0 (Float.sqrt 2.0)

isometric ::
  Point3d (space @ units) ->
  Qty units ->
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
isometric givenFocalPoint distance givenProjection givenFieldOfView =
  orbit
    @ givenFocalPoint
    @ #azimuth (Angle.degrees 45.0)
    @ #elevation isometricElevation
    @ distance
    @ givenProjection
    @ givenFieldOfView

moveTo ::
  Point3d (space @ units) ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
moveTo newEyePoint Camera3d{frame, focalDistance, projection, fovAngle} =
  Camera3d{frame = Frame3d.moveTo newEyePoint frame, focalDistance, projection, fovAngle}

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Camera3d (local @ units) ->
  Camera3d (global @ units)
placeIn givenFrame Camera3d{frame, focalDistance, projection, fovAngle} =
  Camera3d{frame = Frame3d.placeIn givenFrame frame, focalDistance, projection, fovAngle}

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Camera3d (global @ units) ->
  Camera3d (local @ units)
relativeTo givenFrame = placeIn (Frame3d.inverse givenFrame)

transformBy ::
  Transform3d.Rigid (space @ units) ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
transformBy transform Camera3d{frame, focalDistance, projection, fovAngle} =
  Camera3d{frame = Frame3d.transformBy transform frame, focalDistance, projection, fovAngle}

translateBy ::
  Vector3d (space @ units) ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
translateBy = Transform3d.translateByImpl transformBy

translateIn ::
  Direction3d space ->
  Qty units ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
translateIn = Transform3d.translateInImpl transformBy

translateAlong ::
  Axis3d (space @ units) ->
  Qty units ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround ::
  Axis3d (space @ units) ->
  Angle ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
rotateAround = Transform3d.rotateAroundImpl transformBy
