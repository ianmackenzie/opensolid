module OpenSolid.Orientation2d
  ( Orientation2d
  , coerce
  , xy
  , fromXDirection
  , fromYDirection
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
  , inverse
  )
where

import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Orientation2d (Orientation2d))

xDirection :: Orientation2d space defines -> Direction2d space
xDirection (Orientation2d i _) = i

yDirection :: Orientation2d space defines -> Direction2d space
yDirection (Orientation2d _ j) = j

coerce :: Orientation2d space1 defines1 -> Orientation2d space2 defines2
coerce (Orientation2d i j) = Orientation2d (Direction2d.coerce i) (Direction2d.coerce j)

xy :: Orientation2d space defines
xy = Orientation2d Direction2d.x Direction2d.y

fromXDirection :: Direction2d space -> Orientation2d space defines
fromXDirection i = Orientation2d i (Direction2d.rotateLeft i)

fromYDirection :: Direction2d space -> Orientation2d space defines
fromYDirection j = Orientation2d (Direction2d.rotateRight j) j

placeIn ::
  Orientation2d global (Defines space) ->
  Orientation2d space (Defines local) ->
  Orientation2d global (Defines local)
placeIn globalOrientation (Orientation2d i j) =
  Orientation2d
    (Direction2d.placeIn globalOrientation i)
    (Direction2d.placeIn globalOrientation j)

relativeTo ::
  Orientation2d global (Defines space) ->
  Orientation2d global (Defines local) ->
  Orientation2d space (Defines local)
relativeTo globalOrientation (Orientation2d i j) =
  Orientation2d
    (Direction2d.relativeTo globalOrientation i)
    (Direction2d.relativeTo globalOrientation j)

inverse :: Orientation2d global (Defines local) -> Orientation2d local (Defines global)
inverse orientation = xy |> relativeTo orientation
