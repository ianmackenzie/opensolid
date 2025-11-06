module OpenSolid.Convention3d
  ( Convention3d (..)
  , yUp
  , zUp
  , custom
  , xDirection
  , yDirection
  , zDirection
  , xAxis
  , yAxis
  , zAxis
  )
where

import OpenSolid.Arithmetic
import OpenSolid.CoordinateSystem
import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Number (Number)
import {-# SOURCE #-} OpenSolid.Orientation3d qualified as Orientation3d
import OpenSolid.Primitives
  ( Axis3d (Axis3d)
  , Direction3d (Direction3d, Unit3d)
  , Frame3d (Frame3d)
  , Orientation3d (Orientation3d)
  )

{-| A coordinate convention in 3D space.

This defines which of X, Y and Z mean 'forward' or 'upward' or 'rightward'.
-}
data Convention3d where
  Convention3d ::
    { xr :: Number
    , xf :: Number
    , xu :: Number
    , yr :: Number
    , yf :: Number
    , yu :: Number
    , zr :: Number
    , zf :: Number
    , zu :: Number
    } ->
    Convention3d

instance FFI Convention3d where
  representation = FFI.classRepresentation "Convention3d"

{-| A convention where positive X is leftward, positive Y is upward, and positive Z is forward.

This is the convention used by (among other things) the glTF file format.
-}
yUp :: Convention3d
yUp =
  Convention3d
    { xr = -1.0
    , xf = 0.0
    , xu = 0.0
    , yr = 0.0
    , yf = 0.0
    , yu = 1.0
    , zr = 0.0
    , zf = 1.0
    , zu = 0.0
    }

{-| A convention where positive X is rightward, positive Y is forward and positive Z is upward.

This is the convention used by (among other things) the Blender animation package.
-}
zUp :: Convention3d
zUp =
  Convention3d
    { xr = 1.0
    , xf = 0.0
    , xu = 0.0
    , yr = 0.0
    , yf = 1.0
    , yu = 0.0
    , zr = 0.0
    , zf = 0.0
    , zu = 1.0
    }

custom ::
  (forall space. Orientation3d space -> Direction3d space) ->
  (forall space. Orientation3d space -> Direction3d space) ->
  (forall space. Orientation3d space -> Direction3d space) ->
  Convention3d
custom givenXDirection givenYDirection givenZDirection = do
  let Direction3d xr xf xu = givenXDirection Orientation3d.world
  let Direction3d yr yf yu = givenYDirection Orientation3d.world
  let Direction3d zr zf zu = givenZDirection Orientation3d.world
  Convention3d{xr, xf, xu, yr, yf, yu, zr, zf, zu}

-- | Get the X direction of a given orientation for a particular coordinate convention.
xDirection :: Orientation3d space -> Convention3d -> Direction3d space
xDirection (Orientation3d dr df du) Convention3d{xr, xf, xu} = Unit3d (dr .*. xr + df .*. xf + du .*. xu)

-- | Get the Y direction of a given orientation for a particular coordinate convention.
yDirection :: Orientation3d space -> Convention3d -> Direction3d space
yDirection (Orientation3d dr df du) Convention3d{yr, yf, yu} = Unit3d (dr .*. yr + df .*. yf + du .*. yu)

-- | Get the Z direction of a given orientation for a particular coordinate convention.
zDirection :: Orientation3d space -> Convention3d -> Direction3d space
zDirection (Orientation3d dr df du) Convention3d{zr, zf, zu} = Unit3d (dr .*. zr + df .*. zf + du .*. zu)

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
