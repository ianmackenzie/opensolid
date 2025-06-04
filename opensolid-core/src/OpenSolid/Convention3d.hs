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

import OpenSolid.Bootstrap
import OpenSolid.CoordinateSystem
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import {-# SOURCE #-} OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.Primitives (Axis3d (Axis3d), Direction3d, Frame3d (Frame3d), Orientation3d)

{-| A coordinate convention in 3D space.

Defined by specifying the meaning of the X, Y and Z directions
in terms of forward, backward, leftward, rightward, upward and downward.
-}
data Convention3d where
  Convention3d ::
    (forall space. Orientation3d space -> Direction3d space) ->
    (forall space. Orientation3d space -> Direction3d space) ->
    (forall space. Orientation3d space -> Direction3d space) ->
    Convention3d

instance FFI Convention3d where
  representation = FFI.classRepresentation "Convention3d"

{-| A coordinate system where X is leftward, Y is upward, and Z is forward.

This is the coordinate system used by (among other things) the glTF file format.
-}
yUp :: Convention3d
yUp =
  Convention3d
    Orientation3d.leftwardDirection
    Orientation3d.upwardDirection
    Orientation3d.forwardDirection

{-| A coordinate system where X is rightward, Y is forward and Z is upward.

This is the coordinate system used by (among other things) the Blender animation package.
-}
zUp :: Convention3d
zUp =
  Convention3d
    Orientation3d.rightwardDirection
    Orientation3d.forwardDirection
    Orientation3d.upwardDirection

-- | Get the X direction of a given orientation for a particular coordinate convention.
xDirection :: Orientation3d space -> Convention3d -> Direction3d space
xDirection orientation (Convention3d dx _ _) = dx orientation

-- | Get the Y direction of a given orientation for a particular coordinate convention.
yDirection :: Orientation3d space -> Convention3d -> Direction3d space
yDirection orientation (Convention3d _ dy _) = dy orientation

-- | Get the Z direction of a given orientation for a particular coordinate convention.
zDirection :: Orientation3d space -> Convention3d -> Direction3d space
zDirection orientation (Convention3d _ _ dz) = dz orientation

-- | Get the X axis of a given frame for a particular coordinate convention.
xAxis :: Frame3d (space @ units) defines -> Convention3d -> Axis3d (space @ units)
xAxis (Frame3d originPoint orientation) convention =
  Axis3d originPoint (xDirection orientation convention)

-- | Get the Y axis of a given frame for a particular coordinate convention.
yAxis :: Frame3d (space @ units) defines -> Convention3d -> Axis3d (space @ units)
yAxis (Frame3d originPoint orientation) convention =
  Axis3d originPoint (yDirection orientation convention)

-- | Get the Z axis of a given frame for a particular coordinate convention.
zAxis :: Frame3d (space @ units) defines -> Convention3d -> Axis3d (space @ units)
zAxis (Frame3d originPoint orientation) convention =
  Axis3d originPoint (zDirection orientation convention)
