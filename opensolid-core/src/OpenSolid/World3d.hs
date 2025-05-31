module OpenSolid.World3d
  ( originPoint
  , orientation
  , frame
  , rightwardDirection
  , leftwardDirection
  , forwardDirection
  , backwardDirection
  , upwardDirection
  , downwardDirection
  , rightwardForwardDirection
  , forwardRightwardDirection
  , forwardUpwardDirection
  , upwardForwardDirection
  , rightwardUpwardDirection
  , upwardRightwardDirection
  , rightwardOrientation
  , leftwardOrientation
  , forwardOrientation
  , backwardOrientation
  , upwardOrientation
  , downwardOrientation
  , rightPlaneOrientation
  , leftPlaneOrientation
  , frontPlaneOrientation
  , backPlaneOrientation
  , topPlaneOrientation
  , bottomPlaneOrientation
  , rightPlane
  , leftPlane
  , frontPlane
  , backPlane
  , topPlane
  , bottomPlane
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Angle qualified as Angle
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Direction3d (Direction3d, Unit3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  , Plane3d (Plane3d)
  , PlaneOrientation3d (PlaneOrientation3d)
  , Point3d (Point3d)
  )
import OpenSolid.Qty qualified as Qty

-- | The origin point of the current global coordinate system.
originPoint :: Point3d (space @ units)
originPoint = Point3d Qty.zero Qty.zero Qty.zero

-- | The rightward direction of the current global coordinate system.
rightwardDirection :: Direction3d space
rightwardDirection = Direction3d 1.0 0.0 0.0

-- | The leftward direction of the current global coordinate system.
leftwardDirection :: Direction3d space
leftwardDirection = Direction3d -1.0 0.0 0.0

-- | The forward direction of the current global coordinate system.
forwardDirection :: Direction3d space
forwardDirection = Direction3d 0.0 1.0 0.0

-- | The backward direction of the current global coordinate system.
backwardDirection :: Direction3d space
backwardDirection = Direction3d 0.0 -1.0 0.0

-- | The upward direction of the current global coordinate system.
upwardDirection :: Direction3d space
upwardDirection = Direction3d 0.0 0.0 1.0

-- | The downward direction of the current global coordinate system.
downwardDirection :: Direction3d space
downwardDirection = Direction3d 0.0 0.0 -1.0

rightwardForwardDirection :: Angle -> Direction3d space
rightwardForwardDirection angle =
  Unit3d (Angle.cos angle * rightwardDirection + Angle.sin angle * forwardDirection)

forwardRightwardDirection :: Angle -> Direction3d space
forwardRightwardDirection angle =
  Unit3d (Angle.cos angle * forwardDirection + Angle.sin angle * rightwardDirection)

forwardUpwardDirection :: Angle -> Direction3d space
forwardUpwardDirection angle =
  Unit3d (Angle.cos angle * forwardDirection + Angle.sin angle * upwardDirection)

upwardForwardDirection :: Angle -> Direction3d space
upwardForwardDirection angle =
  Unit3d (Angle.cos angle * upwardDirection + Angle.sin angle * forwardDirection)

rightwardUpwardDirection :: Angle -> Direction3d space
rightwardUpwardDirection angle =
  Unit3d (Angle.cos angle * rightwardDirection + Angle.sin angle * upwardDirection)

upwardRightwardDirection :: Angle -> Direction3d space
upwardRightwardDirection angle =
  Unit3d (Angle.cos angle * upwardDirection + Angle.sin angle * rightwardDirection)

-- | The global coordinate system orientation.
orientation :: Orientation3d space (Defines space)
orientation = Orientation3d rightwardDirection forwardDirection upwardDirection

-- | The global coordinate system frame.
frame :: Frame3d (space @ units) (Defines space)
frame = Frame3d originPoint orientation

forwardOrientation :: Orientation3d space defines
forwardOrientation = Orientation3d rightwardDirection forwardDirection upwardDirection

backwardOrientation :: Orientation3d space defines
backwardOrientation = Orientation3d leftwardDirection backwardDirection upwardDirection

leftwardOrientation :: Orientation3d space defines
leftwardOrientation = Orientation3d forwardDirection leftwardDirection upwardDirection

rightwardOrientation :: Orientation3d space defines
rightwardOrientation = Orientation3d backwardDirection rightwardDirection upwardDirection

upwardOrientation :: Orientation3d space defines
upwardOrientation = Orientation3d leftwardDirection upwardDirection forwardDirection

downwardOrientation :: Orientation3d space defines
downwardOrientation = Orientation3d rightwardDirection downwardDirection forwardDirection

rightPlaneOrientation :: PlaneOrientation3d space defines
rightPlaneOrientation = PlaneOrientation3d forwardDirection upwardDirection

leftPlaneOrientation :: PlaneOrientation3d space defines
leftPlaneOrientation = PlaneOrientation3d backwardDirection upwardDirection

frontPlaneOrientation :: PlaneOrientation3d space defines
frontPlaneOrientation = PlaneOrientation3d leftwardDirection upwardDirection

backPlaneOrientation :: PlaneOrientation3d space defines
backPlaneOrientation = PlaneOrientation3d rightwardDirection upwardDirection

topPlaneOrientation :: PlaneOrientation3d space defines
topPlaneOrientation = PlaneOrientation3d rightwardDirection forwardDirection

bottomPlaneOrientation :: PlaneOrientation3d space defines
bottomPlaneOrientation = PlaneOrientation3d leftwardDirection forwardDirection

-- | The rightward-facing plane of the global coordinate system.
rightPlane :: Plane3d (space @ units) defines
rightPlane = Plane3d originPoint rightPlaneOrientation

-- | The leftward-facing plane of the global coordinate system.
leftPlane :: Plane3d (space @ units) defines
leftPlane = Plane3d originPoint leftPlaneOrientation

-- | The forward-facing plane of the global coordinate system.
frontPlane :: Plane3d (space @ units) defines
frontPlane = Plane3d originPoint frontPlaneOrientation

-- | The backward-facing plane of the global coordinate system.
backPlane :: Plane3d (space @ units) defines
backPlane = Plane3d originPoint backPlaneOrientation

-- | The upward-facing plane of the global coordinate system.
topPlane :: Plane3d (space @ units) defines
topPlane = Plane3d originPoint topPlaneOrientation

-- | The downward-facing plane of the global coordinate system.
bottomPlane :: Plane3d (space @ units) defines
bottomPlane = Plane3d originPoint bottomPlaneOrientation
