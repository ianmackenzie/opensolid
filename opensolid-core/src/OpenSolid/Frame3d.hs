module OpenSolid.Frame3d
  ( Frame3d (Frame3d)
  , coerce
  , erase
  , identity
  , forwardFacing
  , backwardFacing
  , leftwardFacing
  , rightwardFacing
  , upwardFacing
  , downwardFacing
  , originPoint
  , basis
  , rightwardDirection
  , leftwardDirection
  , forwardDirection
  , backwardDirection
  , upwardDirection
  , downwardDirection
  , rightwardAxis
  , leftwardAxis
  , forwardAxis
  , backwardAxis
  , upwardAxis
  , downwardAxis
  , frontPlane
  , backPlane
  , leftPlane
  , rightPlane
  , topPlane
  , bottomPlane
  , fromFrontPlane
  , fromBackPlane
  , fromLeftPlane
  , fromRightPlane
  , fromTopPlane
  , fromBottomPlane
  , reverse
  , offsetForwardBy
  , offsetBackwardBy
  , offsetRightwardBy
  , offsetLeftwardBy
  , offsetUpwardBy
  , offsetDownwardBy
  , turnRightBy
  , turnLeftBy
  , rollRightBy
  , rollLeftBy
  , rotateUpBy
  , rotateDownBy
  , turnRight
  , turnLeft
  , rollRight
  , rollLeft
  , rotateUp
  , rotateDown
  , placeIn
  , relativeTo
  , inverse
  , moveTo
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
import OpenSolid.Basis3d (Basis3d)
import OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Direction3d (Direction3d)
import OpenSolid.Point3d (Point3d)
import OpenSolid.Point3d qualified as Point3d
import OpenSolid.Prelude hiding (identity)
import OpenSolid.Primitives
  ( Basis3d (Basis3d)
  , Direction3d (Unit3d)
  , Frame3d (Frame3d)
  , PlanarBasis3d (PlanarBasis3d)
  , Plane3d (Plane3d)
  , Vector3d
  )
import OpenSolid.Transform3d qualified as Transform3d

identity :: Frame3d (space @ units) defines
identity = forwardFacing Point3d.origin

forwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
forwardFacing p0 = Frame3d p0 Basis3d.forwardFacing

backwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
backwardFacing p0 = Frame3d p0 Basis3d.backwardFacing

leftwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
leftwardFacing p0 = Frame3d p0 Basis3d.leftwardFacing

rightwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
rightwardFacing p0 = Frame3d p0 Basis3d.rightwardFacing

upwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
upwardFacing p0 = Frame3d p0 Basis3d.upwardFacing

downwardFacing :: Point3d (space @ units) -> Frame3d (space @ units) defines
downwardFacing p0 = Frame3d p0 Basis3d.downwardFacing

originPoint :: Frame3d (space @ units) defines -> Point3d (space @ units)
originPoint (Frame3d p0 _) = p0

basis :: Frame3d (space @ units) defines -> Basis3d space defines
basis (Frame3d _ b) = b

coerce :: Frame3d (space1 @ units1) defines1 -> Frame3d (space2 @ units2) defines2
coerce (Frame3d p0 b) = Frame3d (Point3d.coerce p0) (Basis3d.coerce b)

erase :: Frame3d (space @ units) defines -> Frame3d (space @ Unitless) defines
erase = coerce

rightwardDirection :: Frame3d (space @ units) defines -> Direction3d space
rightwardDirection frame = Basis3d.rightwardDirection (basis frame)

leftwardDirection :: Frame3d (space @ units) defines -> Direction3d space
leftwardDirection frame = Basis3d.leftwardDirection (basis frame)

forwardDirection :: Frame3d (space @ units) defines -> Direction3d space
forwardDirection frame = Basis3d.forwardDirection (basis frame)

backwardDirection :: Frame3d (space @ units) defines -> Direction3d space
backwardDirection frame = Basis3d.backwardDirection (basis frame)

upwardDirection :: Frame3d (space @ units) defines -> Direction3d space
upwardDirection frame = Basis3d.upwardDirection (basis frame)

downwardDirection :: Frame3d (space @ units) defines -> Direction3d space
downwardDirection frame = Basis3d.downwardDirection (basis frame)

rightwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
rightwardAxis frame = Axis3d (originPoint frame) (rightwardDirection frame)

leftwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
leftwardAxis frame = Axis3d (originPoint frame) (leftwardDirection frame)

forwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
forwardAxis frame = Axis3d (originPoint frame) (forwardDirection frame)

backwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
backwardAxis frame = Axis3d (originPoint frame) (backwardDirection frame)

upwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
upwardAxis frame = Axis3d (originPoint frame) (upwardDirection frame)

downwardAxis :: Frame3d (space @ units) defines -> Axis3d (space @ units)
downwardAxis frame = Axis3d (originPoint frame) (downwardDirection frame)

frontPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
frontPlane (Frame3d p0 (Basis3d r _ u)) = Plane3d p0 (PlanarBasis3d -r u)

backPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
backPlane (Frame3d p0 (Basis3d r _ u)) = Plane3d p0 (PlanarBasis3d r u)

leftPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
leftPlane (Frame3d p0 (Basis3d _ f u)) = Plane3d p0 (PlanarBasis3d -f u)

rightPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
rightPlane (Frame3d p0 (Basis3d _ f u)) = Plane3d p0 (PlanarBasis3d f u)

topPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
topPlane (Frame3d p0 (Basis3d r f _)) = Plane3d p0 (PlanarBasis3d r f)

bottomPlane :: Frame3d (space @ units) defines1 -> Plane3d (space @ units) defines2
bottomPlane (Frame3d p0 (Basis3d r f _)) = Plane3d p0 (PlanarBasis3d -r f)

fromFrontPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromFrontPlane (Plane3d p0 (PlanarBasis3d l u)) = Frame3d p0 (Basis3d -l (Unit3d (l `cross` u)) u)

fromBackPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromBackPlane (Plane3d p0 (PlanarBasis3d r u)) = Frame3d p0 (Basis3d r (Unit3d (u `cross` r)) u)

fromLeftPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromLeftPlane (Plane3d p0 (PlanarBasis3d b u)) = Frame3d p0 (Basis3d (Unit3d (u `cross` b)) -b u)

fromRightPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromRightPlane (Plane3d p0 (PlanarBasis3d f u)) = Frame3d p0 (Basis3d (Unit3d (f `cross` u)) f u)

fromTopPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromTopPlane (Plane3d p0 (PlanarBasis3d r f)) = Frame3d p0 (Basis3d r f (Unit3d (r `cross` f)))

fromBottomPlane :: Plane3d (space @ units) defines1 -> Frame3d (space @ units) defines2
fromBottomPlane (Plane3d p0 (PlanarBasis3d l f)) = Frame3d p0 (Basis3d -l f (Unit3d (f `cross` l)))

reverse :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
reverse (Frame3d p0 (Basis3d r f u)) = Frame3d p0 (Basis3d -r -f u)

offsetForwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetForwardBy distance = translateInOwn forwardDirection distance

offsetBackwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetBackwardBy distance = translateInOwn backwardDirection distance

offsetRightwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetRightwardBy distance = translateInOwn rightwardDirection distance

offsetLeftwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetLeftwardBy distance = translateInOwn leftwardDirection distance

offsetUpwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetUpwardBy distance = translateInOwn upwardDirection distance

offsetDownwardBy ::
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
offsetDownwardBy distance = translateInOwn downwardDirection distance

turnLeftBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnLeftBy angle = rotateAroundOwn upwardAxis angle

turnRightBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnRightBy angle = rotateAroundOwn upwardAxis -angle

rollRightBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollRightBy angle = rotateAroundOwn forwardAxis angle

rollLeftBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollLeftBy angle = rotateAroundOwn forwardAxis -angle

rotateUpBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateUpBy angle = rotateAroundOwn rightwardAxis angle

rotateDownBy :: Angle -> Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateDownBy angle = rotateAroundOwn rightwardAxis -angle

turnLeft :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnLeft = turnLeftBy Angle.halfPi

turnRight :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
turnRight = turnRightBy Angle.halfPi

rollLeft :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollLeft = rollLeftBy Angle.halfPi

rollRight :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rollRight = rollRightBy Angle.halfPi

rotateUp :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateUp = rotateUpBy Angle.halfPi

rotateDown :: Frame3d (space @ units) defines1 -> Frame3d (space @ units) defines2
rotateDown = rotateDownBy Angle.halfPi

-- | Convert a frame defined in local coordinates to one defined in global coordinates.
placeIn ::
  Frame3d (global @ units) (Defines space) ->
  Frame3d (space @ units) (Defines local) ->
  Frame3d (global @ units) (Defines local)
placeIn globalFrame frame =
  Frame3d
    (Point3d.placeIn globalFrame (originPoint frame))
    (Basis3d.placeIn (basis globalFrame) (basis frame))

-- | Convert a frame defined in global coordinates to one defined in local coordinates.
relativeTo ::
  Frame3d (global @ units) (Defines space) ->
  Frame3d (global @ units) (Defines local) ->
  Frame3d (space @ units) (Defines local)
relativeTo globalFrame frame =
  Frame3d
    (Point3d.relativeTo globalFrame (originPoint frame))
    (Basis3d.relativeTo (basis globalFrame) (basis frame))

inverse :: Frame3d (global @ units) (Defines local) -> Frame3d (local @ units) (Defines global)
inverse frame = identity |> relativeTo frame

moveTo ::
  Point3d (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
moveTo newOriginPoint (Frame3d _ b) = Frame3d newOriginPoint (Basis3d.coerce b)

transformBy ::
  Transform3d.Rigid (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
transformBy transform (Frame3d p0 b) =
  Frame3d (Point3d.transformBy transform p0) (Basis3d.transformBy transform b)

translateBy ::
  Vector3d (space @ units) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateBy = Transform3d.translateByImpl transformBy

translateIn ::
  Direction3d space ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateIn = Transform3d.translateInImpl transformBy

translateAlong ::
  Axis3d (space @ units) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateAlong = Transform3d.translateAlongImpl transformBy

rotateAround ::
  Axis3d (space @ units) ->
  Angle ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
rotateAround = Transform3d.rotateAroundImpl transformBy

translateByOwn ::
  (Frame3d (space @ units) defines1 -> Vector3d (space @ units)) ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateByOwn = Transform3d.translateByOwnImpl transformBy

translateInOwn ::
  (Frame3d (space @ units) defines1 -> Direction3d space) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateInOwn = Transform3d.translateInOwnImpl transformBy

translateAlongOwn ::
  (Frame3d (space @ units) defines1 -> Axis3d (space @ units)) ->
  Qty units ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
translateAlongOwn = Transform3d.translateAlongOwnImpl transformBy

rotateAroundOwn ::
  (Frame3d (space @ units) defines1 -> Axis3d (space @ units)) ->
  Angle ->
  Frame3d (space @ units) defines1 ->
  Frame3d (space @ units) defines2
rotateAroundOwn = Transform3d.rotateAroundOwnImpl transformBy
