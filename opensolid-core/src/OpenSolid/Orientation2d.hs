module OpenSolid.Orientation2d
  ( Orientation2d
  , coerce
  , xy
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
import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Frame2d, Orientation2d (Orientation2d))
import OpenSolid.Random qualified as Random

xDirection :: Orientation2d space -> Direction2d space
xDirection (Orientation2d i _) = i

yDirection :: Orientation2d space -> Direction2d space
yDirection (Orientation2d _ j) = j

coerce :: Orientation2d space1 -> Orientation2d space2
coerce (Orientation2d i j) = Orientation2d (Direction2d.coerce i) (Direction2d.coerce j)

xy :: Orientation2d space
xy = Orientation2d Direction2d.x Direction2d.y

fromXDirection :: Direction2d space -> Orientation2d space
fromXDirection i = Orientation2d i (Direction2d.rotateLeft i)

fromYDirection :: Direction2d space -> Orientation2d space
fromYDirection j = Orientation2d (Direction2d.rotateRight j) j

rotateLeft :: Orientation2d space -> Orientation2d space
rotateLeft (Orientation2d i j) =
  Orientation2d (Direction2d.rotateLeft i) (Direction2d.rotateLeft j)

rotateRight :: Orientation2d space -> Orientation2d space
rotateRight (Orientation2d i j) =
  Orientation2d (Direction2d.rotateRight i) (Direction2d.rotateRight j)

rotateBy :: Angle -> Orientation2d space -> Orientation2d space
rotateBy angle (Orientation2d i j) =
  Orientation2d (Direction2d.rotateBy angle i) (Direction2d.rotateBy angle j)

placeIn ::
  Frame2d (global @ frameUnits) (Defines space) ->
  Orientation2d space ->
  Orientation2d global
placeIn globalOrientation (Orientation2d i j) =
  Orientation2d
    (Direction2d.placeIn globalOrientation i)
    (Direction2d.placeIn globalOrientation j)

relativeTo ::
  Frame2d (global @ frameUnits) (Defines space) ->
  Orientation2d global ->
  Orientation2d space
relativeTo globalOrientation (Orientation2d i j) =
  Orientation2d
    (Direction2d.relativeTo globalOrientation i)
    (Direction2d.relativeTo globalOrientation j)

-- | Generate a random orientation.
random :: Random.Generator (Orientation2d global)
random = Random.map fromXDirection Direction2d.random
