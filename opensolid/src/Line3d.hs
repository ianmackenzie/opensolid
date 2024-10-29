-- Avoid errors when running Fourmolu
{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE PatternSynonyms #-}

module Line3d
  ( from
  , pattern Line3d
  , Line3d (startPoint, endPoint)
  )
where

import Curve3d (Curve3d)
import Curve3d.Internal qualified
import OpenSolid
import Point3d (Point3d)

from :: Point3d (space @ units) -> Point3d (space @ units) -> Curve3d (space @ units)
from = Curve3d.Internal.Line

pattern Line3d :: Line3d (space @ units) -> Curve3d (space @ units)
pattern Line3d line <- (extractLine -> Just line)

data Line3d (coordinateSystem :: CoordinateSystem) where
  Line3d_ ::
    { startPoint :: Point3d (space @ units)
    , endPoint :: Point3d (space @ units)
    } ->
    Line3d (space @ units)

extractLine :: Curve3d (space @ units) -> Maybe (Line3d (space @ units))
extractLine curve = case curve of
  Curve3d.Internal.Line p1 p2 -> Just (Line3d_ p1 p2)
  _ -> Nothing
