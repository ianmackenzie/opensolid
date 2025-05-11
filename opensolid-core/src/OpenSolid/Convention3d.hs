module OpenSolid.Convention3d
  ( Convention3d (..)
  , yUp
  , zUp
  , xAxis
  , yAxis
  , zAxis
  )
where

import OpenSolid.Bootstrap
import OpenSolid.CoordinateSystem
import {-# SOURCE #-} OpenSolid.Direction3d qualified as Direction3d
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import {-# SOURCE #-} OpenSolid.Point3d qualified as Point3d
import OpenSolid.Primitives (Axis3d (Axis3d), Direction3d)

{-| A coordinate convention in 3D space.

Defined by specifying the meaning of the X, Y and Z directions
in terms of forward, backward, leftward, rightward, upward and downward.
-}
data Convention3d space = Convention3d
  { xDirection :: Direction3d space
  , yDirection :: Direction3d space
  , zDirection :: Direction3d space
  }

instance FFI (Convention3d space) where
  representation = FFI.classRepresentation "Convention3d"

{-| A coordinate system where X is leftward, Y is upward, and Z is forward.

This is the coordinate system used by (among other things) the glTF file format.
-}
yUp :: Convention3d space
yUp =
  Convention3d
    { xDirection = Direction3d.leftward
    , yDirection = Direction3d.upward
    , zDirection = Direction3d.forward
    }

{-| A coordinate system where X is rightward, Y is forward and Z is upward.

This is the coordinate system used by (among other things) the Blender animation package.
-}
zUp :: Convention3d space
zUp =
  Convention3d
    { xDirection = Direction3d.rightward
    , yDirection = Direction3d.forward
    , zDirection = Direction3d.upward
    }

-- | Get the X axis for a particular coordinate convention.
xAxis :: Convention3d space -> Axis3d (space @ units)
xAxis convention = Axis3d Point3d.origin (xDirection convention)

-- | Get the Y axis for a particular coordinate convention.
yAxis :: Convention3d space -> Axis3d (space @ units)
yAxis convention = Axis3d Point3d.origin (yDirection convention)

-- | Get the Z axis for a particular coordinate convention.
zAxis :: Convention3d space -> Axis3d (space @ units)
zAxis convention = Axis3d Point3d.origin (zDirection convention)
