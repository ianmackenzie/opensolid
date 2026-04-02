module OpenSolid.Orientation2D
  ( Orientation2D
  , horizontal
  , vertical
  , fromXDirection
  , fromYDirection
  , rotateLeft
  , rotateRight
  , rotateBy
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
  , random
  )
where

import OpenSolid.Angle (Angle)
import OpenSolid.Direction2D (Direction2D)
import OpenSolid.Direction2D qualified as Direction2D
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2D, Orientation2D (Orientation2D))
import OpenSolid.Random qualified as Random

xDirection :: Orientation2D -> Direction2D
xDirection (Orientation2D i _) = i

yDirection :: Orientation2D -> Direction2D
yDirection (Orientation2D _ j) = j

horizontal :: Orientation2D
horizontal = Orientation2D Direction2D.x Direction2D.y

vertical :: Orientation2D
vertical = Orientation2D Direction2D.y -Direction2D.x

fromXDirection :: Direction2D -> Orientation2D
fromXDirection i = Orientation2D i (Direction2D.rotateLeft i)

fromYDirection :: Direction2D -> Orientation2D
fromYDirection j = Orientation2D (Direction2D.rotateRight j) j

rotateLeft :: Orientation2D -> Orientation2D
rotateLeft (Orientation2D i j) =
  Orientation2D (Direction2D.rotateLeft i) (Direction2D.rotateLeft j)

rotateRight :: Orientation2D -> Orientation2D
rotateRight (Orientation2D i j) =
  Orientation2D (Direction2D.rotateRight i) (Direction2D.rotateRight j)

rotateBy :: Angle -> Orientation2D -> Orientation2D
rotateBy angle (Orientation2D i j) =
  Orientation2D (Direction2D.rotateBy angle i) (Direction2D.rotateBy angle j)

placeIn :: Frame2D frameUnits -> Orientation2D -> Orientation2D
placeIn globalOrientation (Orientation2D i j) =
  Orientation2D
    (Direction2D.placeIn globalOrientation i)
    (Direction2D.placeIn globalOrientation j)

relativeTo :: Frame2D frameUnits -> Orientation2D -> Orientation2D
relativeTo globalOrientation (Orientation2D i j) =
  Orientation2D
    (Direction2D.relativeTo globalOrientation i)
    (Direction2D.relativeTo globalOrientation j)

-- | Generate a random orientation.
random :: Random.Generator Orientation2D
random = Random.map fromXDirection Direction2D.random
