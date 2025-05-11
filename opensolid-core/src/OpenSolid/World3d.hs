module OpenSolid.World3d
  ( originPoint
  , basis
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
  , rightwardBasis
  , leftwardBasis
  , forwardBasis
  , backwardBasis
  , upwardBasis
  , downwardBasis
  , rightPlaneBasis
  , leftPlaneBasis
  , frontPlaneBasis
  , backPlaneBasis
  , topPlaneBasis
  , bottomPlaneBasis
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
  ( Basis3d (Basis3d)
  , Direction3d (Direction3d, Unit3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
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

-- | The global coordinate system basis.
basis :: Basis3d space (Defines space)
basis = Basis3d rightwardDirection forwardDirection upwardDirection

-- | The global coordinate system frame.
frame :: Frame3d (space @ units) (Defines space)
frame = Frame3d originPoint basis

forwardBasis :: Basis3d space defines
forwardBasis = Basis3d rightwardDirection forwardDirection upwardDirection

backwardBasis :: Basis3d space defines
backwardBasis = Basis3d leftwardDirection backwardDirection upwardDirection

leftwardBasis :: Basis3d space defines
leftwardBasis = Basis3d forwardDirection leftwardDirection upwardDirection

rightwardBasis :: Basis3d space defines
rightwardBasis = Basis3d backwardDirection rightwardDirection upwardDirection

upwardBasis :: Basis3d space defines
upwardBasis = Basis3d leftwardDirection upwardDirection forwardDirection

downwardBasis :: Basis3d space defines
downwardBasis = Basis3d rightwardDirection downwardDirection forwardDirection

rightPlaneBasis :: PlanarBasis3d space defines
rightPlaneBasis = PlanarBasis3d forwardDirection upwardDirection

leftPlaneBasis :: PlanarBasis3d space defines
leftPlaneBasis = PlanarBasis3d backwardDirection upwardDirection

frontPlaneBasis :: PlanarBasis3d space defines
frontPlaneBasis = PlanarBasis3d leftwardDirection upwardDirection

backPlaneBasis :: PlanarBasis3d space defines
backPlaneBasis = PlanarBasis3d rightwardDirection upwardDirection

topPlaneBasis :: PlanarBasis3d space defines
topPlaneBasis = PlanarBasis3d rightwardDirection forwardDirection

bottomPlaneBasis :: PlanarBasis3d space defines
bottomPlaneBasis = PlanarBasis3d leftwardDirection forwardDirection

-- | The rightward-facing plane of the global coordinate system.
rightPlane :: Plane3d (space @ units) defines
rightPlane = Plane3d originPoint rightPlaneBasis

-- | The leftward-facing plane of the global coordinate system.
leftPlane :: Plane3d (space @ units) defines
leftPlane = Plane3d originPoint leftPlaneBasis

-- | The forward-facing plane of the global coordinate system.
frontPlane :: Plane3d (space @ units) defines
frontPlane = Plane3d originPoint frontPlaneBasis

-- | The backward-facing plane of the global coordinate system.
backPlane :: Plane3d (space @ units) defines
backPlane = Plane3d originPoint backPlaneBasis

-- | The upward-facing plane of the global coordinate system.
topPlane :: Plane3d (space @ units) defines
topPlane = Plane3d originPoint topPlaneBasis

-- | The downward-facing plane of the global coordinate system.
bottomPlane :: Plane3d (space @ units) defines
bottomPlane = Plane3d originPoint bottomPlaneBasis
