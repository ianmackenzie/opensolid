module OpenSolid.Convention3d
  ( Convention3d (Convention3d)
  , yUp
  , zUp
  , xDirection
  , yDirection
  , zDirection
  , xAxis
  , yAxis
  , zAxis
  )
where

import {-# SOURCE #-} OpenSolid.Basis3d qualified as Basis3d
import OpenSolid.Bootstrap
import OpenSolid.CoordinateSystem
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Primitives (Axis3d (Axis3d), Basis3d, Direction3d, Frame3d (Frame3d))

{-| A coordinate convention in 3D space.

Defined by specifying the meaning of the X, Y and Z directions
in terms of forward, backward, leftward, rightward, upward and downward.
-}
data Convention3d where
  Convention3d ::
    (forall space defines. Basis3d space defines -> Direction3d space) ->
    (forall space defines. Basis3d space defines -> Direction3d space) ->
    (forall space defines. Basis3d space defines -> Direction3d space) ->
    Convention3d

instance FFI Convention3d where
  representation = FFI.classRepresentation "Convention3d"

{-| A coordinate system where X is leftward, Y is upward, and Z is forward.

This is the coordinate system used by (among other things) the glTF file format.
-}
yUp :: Convention3d
yUp = Convention3d Basis3d.leftwardDirection Basis3d.upwardDirection Basis3d.forwardDirection

{-| A coordinate system where X is rightward, Y is forward and Z is upward.

This is the coordinate system used by (among other things) the Blender animation package.
-}
zUp :: Convention3d
zUp = Convention3d Basis3d.rightwardDirection Basis3d.forwardDirection Basis3d.upwardDirection

-- | Get the X direction of a given basis for a particular coordinate convention.
xDirection :: Basis3d space defines -> Convention3d -> Direction3d space
xDirection basis (Convention3d dx _ _) = dx basis

-- | Get the Y direction of a given basis for a particular coordinate convention.
yDirection :: Basis3d space defines -> Convention3d -> Direction3d space
yDirection basis (Convention3d _ dy _) = dy basis

-- | Get the Z direction of a given basis for a particular coordinate convention.
zDirection :: Basis3d space defines -> Convention3d -> Direction3d space
zDirection basis (Convention3d _ _ dz) = dz basis

-- | Get the X axis of a given frame for a particular coordinate convention.
xAxis :: Frame3d (space @ units) defines -> Convention3d -> Axis3d (space @ units)
xAxis (Frame3d originPoint basis) convention = Axis3d originPoint (xDirection basis convention)

-- | Get the Y axis of a given frame for a particular coordinate convention.
yAxis :: Frame3d (space @ units) defines -> Convention3d -> Axis3d (space @ units)
yAxis (Frame3d originPoint basis) convention = Axis3d originPoint (yDirection basis convention)

-- | Get the Z axis of a given frame for a particular coordinate convention.
zAxis :: Frame3d (space @ units) defines -> Convention3d -> Axis3d (space @ units)
zAxis (Frame3d originPoint basis) convention = Axis3d originPoint (zDirection basis convention)
