module OpenSolid.Camera3d
  ( Camera3d
  , ScreenSpace
  , Projection (..)
  , FieldOfView
  , angle
  , height
  , new
  , lookAt
  , orbit
  , isometric
  , isometricElevation
  , eyePoint
  , viewDirection
  , viewPlane
  , frame
  , upDirection
  , focalDistance
  , projection
  , fovAngle
  , fovHeight
  , frustumSlope
  , moveTo
  , placeIn
  , relativeTo
  , transformBy
  , translateBy
  , translateIn
  , translateAlong
  , rotateAround
  , translateByOwn
  , translateInOwn
  , translateAlongOwn
  , rotateAroundOwn
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Axis3d (Axis3d (Axis3d))
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.Float qualified as Float
import OpenSolid.Frame3d (Frame3d)
import OpenSolid.Frame3d qualified as Frame3d
import OpenSolid.PlanarBasis3d qualified as PlanarBasis3d
import OpenSolid.Plane3d (Plane3d (Plane3d))
import OpenSolid.Plane3d qualified as Plane3d
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude
import OpenSolid.Tolerance qualified as Tolerance
import OpenSolid.Transform3d qualified as Transform3d
import OpenSolid.Vector3d (Vector3d)
import OpenSolid.Vector3d qualified as Vector3d

data ScreenSpace

data Camera3d (coordinateSystem :: CoordinateSystem) where
  Camera3d ::
    { frame :: Frame3d (space @ units) (Defines ScreenSpace)
    , upDirection :: Direction3d space
    , focalDistance :: Qty units
    , projection :: Projection
    , fovAngle :: Angle
    } ->
    Camera3d (space @ units)

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
  Frame3d (space @ units) (Defines ScreenSpace) ->
  Direction3d space ->
  Qty units ->
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
new givenFrame givenUpDirection givenFocalDistance projection givenFieldOfView =
  Camera3d
    { frame = givenFrame
    , upDirection = givenUpDirection
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
  Direction3d space -> -- Up direction
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
lookAt givenEyePoint givenFocalPoint givenUpDirection givenProjection givenFieldOfView = do
  let computedFocalDistance = Point3d.distanceFrom givenEyePoint givenFocalPoint
  let computedFrame =
        case Vector3d.direction (givenFocalPoint - givenEyePoint) of
          Success computedViewDirection -> do
            let viewVector = Vector3d.unit computedViewDirection
            let upVector = Vector3d.unit givenUpDirection
            case Tolerance.using 1e-9 (PlanarBasis3d.orthonormalize viewVector upVector) of
              Just basis -> Frame3d.fromZyPlane (Plane3d givenEyePoint (PlanarBasis3d.flipX basis))
              Nothing ->
                -- View direction is straight up or straight down,
                -- so choose an arbitrary frame with the given view direction
                Frame3d.fromZAxis (Axis3d givenEyePoint -computedViewDirection)
          Failure Vector3d.IsZero ->
            -- Given eye and focal points are coincident,
            -- so choose an arbitrary frame with given up direction
            Frame3d.fromYAxis (Axis3d givenEyePoint givenUpDirection)
  new computedFrame givenUpDirection computedFocalDistance givenProjection givenFieldOfView

orbit ::
  Plane3d (space @ units) defines ->
  Point3d (space @ units) ->
  Angle ->
  Angle ->
  Qty units ->
  Projection ->
  FieldOfView units ->
  Camera3d (space @ units)
orbit groundPlane givenFocalPoint azimuth elevation distance givenProjection givenFieldOfView = do
  let computedFrame =
        Frame3d.fromZxPlane groundPlane
          |> Frame3d.moveTo givenFocalPoint
          |> Frame3d.rotateAroundOwn Frame3d.yAxis azimuth
          |> Frame3d.rotateAroundOwn Frame3d.xAxis -elevation
          |> Frame3d.translateInOwn Frame3d.zDirection distance
  new computedFrame (Plane3d.normalDirection groundPlane) distance givenProjection givenFieldOfView

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
    Plane3d.xy
    givenFocalPoint
    (Angle.degrees 45.0)
    isometricElevation
    distance
    givenProjection
    givenFieldOfView

eyePoint :: Camera3d (space @ units) -> Point3d (space @ units)
eyePoint camera = Frame3d.originPoint (frame camera)

viewDirection :: Camera3d (space @ units) -> Direction3d space
viewDirection camera = negate (Frame3d.zDirection (frame camera))

viewPlane :: Camera3d (space @ units) -> Plane3d (space @ units) (Defines ScreenSpace)
viewPlane camera = Frame3d.xyPlane (frame camera)

frustumSlope :: Camera3d (space @ units) -> Float
frustumSlope camera = Angle.tan (0.5 * fovAngle camera)

fovHeight :: Camera3d (space @ units) -> Qty units
fovHeight camera = 2.0 * focalDistance camera * frustumSlope camera

moveTo ::
  Point3d (space @ units) ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
moveTo newEyePoint Camera3d{frame, upDirection, focalDistance, projection, fovAngle} =
  Camera3d
    { frame = Frame3d.moveTo newEyePoint frame
    , upDirection
    , focalDistance
    , projection
    , fovAngle
    }

placeIn ::
  Frame3d (global @ units) (Defines local) ->
  Camera3d (local @ units) ->
  Camera3d (global @ units)
placeIn givenFrame Camera3d{frame, upDirection, focalDistance, projection, fovAngle} =
  Camera3d
    { frame = Frame3d.placeIn givenFrame frame
    , upDirection = Direction3d.placeIn (Frame3d.basis givenFrame) upDirection
    , focalDistance
    , projection
    , fovAngle
    }

relativeTo ::
  Frame3d (global @ units) (Defines local) ->
  Camera3d (global @ units) ->
  Camera3d (local @ units)
relativeTo givenFrame = placeIn (Frame3d.inverse givenFrame)

transformBy ::
  Transform3d.Rigid (space @ units) ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
transformBy transform Camera3d{frame, upDirection, focalDistance, projection, fovAngle} =
  Camera3d
    { frame = Frame3d.transformBy transform frame
    , upDirection = Direction3d.transformBy transform upDirection
    , focalDistance
    , projection
    , fovAngle
    }

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

translateByOwn ::
  (Camera3d (space @ units) -> Vector3d (space @ units)) ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
translateByOwn = Transform3d.translateByOwnImpl transformBy

translateInOwn ::
  (Camera3d (space @ units) -> Direction3d space) ->
  Qty units ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
translateInOwn = Transform3d.translateInOwnImpl transformBy

translateAlongOwn ::
  (Camera3d (space @ units) -> Axis3d (space @ units)) ->
  Qty units ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
translateAlongOwn = Transform3d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  (Camera3d (space @ units) -> Axis3d (space @ units)) ->
  Angle ->
  Camera3d (space @ units) ->
  Camera3d (space @ units)
rotateAroundOwn = Transform3d.rotateAroundOwnImpl transformBy
