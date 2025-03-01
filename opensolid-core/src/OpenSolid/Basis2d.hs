module OpenSolid.Basis2d
  ( Basis2d
  , coerce
  , xy
  , fromXDirection
  , fromYDirection
  , xDirection
  , yDirection
  , placeIn
  , relativeTo
  , placeInBasis
  , relativeToBasis
  , inverse
  )
where

import OpenSolid.Direction2d (Direction2d)
import OpenSolid.Direction2d qualified as Direction2d
import OpenSolid.Prelude
import OpenSolid.Primitives (Basis2d (Basis2d), Frame2d (Frame2d))

xDirection :: Basis2d space defines -> Direction2d space
xDirection (Basis2d i _) = i

yDirection :: Basis2d space defines -> Direction2d space
yDirection (Basis2d _ j) = j

coerce :: Basis2d space defines1 -> Basis2d space defines2
coerce (Basis2d i j) = Basis2d i j

xy :: Basis2d space defines
xy = Basis2d Direction2d.x Direction2d.y

fromXDirection :: Direction2d space -> Basis2d space defines
fromXDirection i = Basis2d i (Direction2d.rotateLeft i)

fromYDirection :: Direction2d space -> Basis2d space defines
fromYDirection j = Basis2d (Direction2d.rotateRight j) j

placeIn ::
  Frame2d (global @ units) (Defines space) ->
  Basis2d space (Defines local) ->
  Basis2d global (Defines local)
placeIn (Frame2d _ globalBasis) = placeInBasis globalBasis

relativeTo ::
  Frame2d (global @ units) (Defines space) ->
  Basis2d global (Defines local) ->
  Basis2d space (Defines local)
relativeTo (Frame2d _ globalBasis) = relativeToBasis globalBasis

placeInBasis ::
  Basis2d global (Defines space) ->
  Basis2d space (Defines local) ->
  Basis2d global (Defines local)
placeInBasis globalBasis (Basis2d i j) =
  Basis2d
    (Direction2d.placeInBasis globalBasis i)
    (Direction2d.placeInBasis globalBasis j)

relativeToBasis ::
  Basis2d global (Defines space) ->
  Basis2d global (Defines local) ->
  Basis2d space (Defines local)
relativeToBasis globalBasis (Basis2d i j) =
  Basis2d
    (Direction2d.relativeToBasis globalBasis i)
    (Direction2d.relativeToBasis globalBasis j)

inverse :: Basis2d global (Defines local) -> Basis2d local (Defines global)
inverse basis = xy |> relativeToBasis basis
