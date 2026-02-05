module OpenSolid.Convention3D
  ( Convention3D (..)
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

import OpenSolid.FFI (FFI)
import OpenSolid.FFI qualified as FFI
import OpenSolid.Prelude
import OpenSolid.Primitives
  ( Axis3D (Axis3D)
  , Direction3D (Direction3D, Unit3D)
  , Frame3D (Frame3D)
  , Orientation3D (Orientation3D)
  )
import OpenSolid.World3D qualified as World3D

{-| A coordinate convention in 3D space.

This defines which of X, Y and Z mean 'forward' or 'upward' or 'rightward'.
-}
data Convention3D = Convention3D
  { xr :: Number
  , xf :: Number
  , xu :: Number
  , yr :: Number
  , yf :: Number
  , yu :: Number
  , zr :: Number
  , zf :: Number
  , zu :: Number
  }

instance FFI Convention3D where
  representation = FFI.classRepresentation "Convention3D"

{-| A convention where positive X is leftward, positive Y is upward, and positive Z is forward.

This is the convention used by (among other things) the glTF file format.
-}
yUp :: Convention3D
yUp =
  Convention3D
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
zUp :: Convention3D
zUp =
  Convention3D
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
  (forall space. Orientation3D space -> Direction3D space) ->
  (forall space. Orientation3D space -> Direction3D space) ->
  (forall space. Orientation3D space -> Direction3D space) ->
  Convention3D
custom givenXDirection givenYDirection givenZDirection = do
  let Direction3D xr xf xu = givenXDirection World3D.forwardOrientation
  let Direction3D yr yf yu = givenYDirection World3D.forwardOrientation
  let Direction3D zr zf zu = givenZDirection World3D.forwardOrientation
  Convention3D{xr, xf, xu, yr, yf, yu, zr, zf, zu}

-- | Get the X direction of a given orientation for a particular coordinate convention.
xDirection :: Orientation3D space -> Convention3D -> Direction3D space
xDirection (Orientation3D dr df du) Convention3D{xr, xf, xu} =
  Unit3D (dr * xr + df * xf + du * xu)

-- | Get the Y direction of a given orientation for a particular coordinate convention.
yDirection :: Orientation3D space -> Convention3D -> Direction3D space
yDirection (Orientation3D dr df du) Convention3D{yr, yf, yu} =
  Unit3D (dr * yr + df * yf + du * yu)

-- | Get the Z direction of a given orientation for a particular coordinate convention.
zDirection :: Orientation3D space -> Convention3D -> Direction3D space
zDirection (Orientation3D dr df du) Convention3D{zr, zf, zu} =
  Unit3D (dr * zr + df * zf + du * zu)

-- | Get the X axis of a given frame for a particular coordinate convention.
xAxis :: Frame3D global local -> Convention3D -> Axis3D global
xAxis (Frame3D originPoint orientation) convention =
  Axis3D originPoint (xDirection orientation convention)

-- | Get the Y axis of a given frame for a particular coordinate convention.
yAxis :: Frame3D global local -> Convention3D -> Axis3D global
yAxis (Frame3D originPoint orientation) convention =
  Axis3D originPoint (yDirection orientation convention)

-- | Get the Z axis of a given frame for a particular coordinate convention.
zAxis :: Frame3D global local -> Convention3D -> Axis3D global
zAxis (Frame3D originPoint orientation) convention =
  Axis3D originPoint (zDirection orientation convention)
