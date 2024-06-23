module Uv
  ( Parameter (U, V)
  , Space
  , Coordinates
  , Point
  , Direction
  , Bounds
  , domain
  , Derivatives
  , derivatives
  , map
  )
where

import Bounds2d (Bounds2d)
import Bounds2d qualified
import Direction2d (Direction2d)
import OpenSolid
import Point2d (Point2d)
import Range qualified

data Parameter = U | V deriving (Eq, Show, Ord)

data Space

type Coordinates = Space @ Unitless

type Point = Point2d Coordinates

type Bounds = Bounds2d Coordinates

type Direction = Direction2d Space

domain :: Bounds
domain = Bounds2d.xy Range.unit Range.unit

data Derivatives a
  = Derivatives a ~(Derivatives a) ~(Derivatives a)

derivatives :: a -> (a -> a) -> (a -> a) -> Derivatives a
derivatives f du dv = do
  let derivativesU = derivatives (du f) du dv
  Derivatives f derivativesU (derivativesV derivativesU (dv f) dv)

derivativesV :: Derivatives a -> a -> (a -> a) -> Derivatives a
derivativesV derivativesU fv dv = do
  let derivativesUV = derivative V derivativesU
  Derivatives fv derivativesUV (derivativesV derivativesUV (dv fv) dv)

value :: Derivatives a -> a
value (Derivatives v _ _) = v

derivative :: Parameter -> Derivatives a -> Derivatives a
derivative U (Derivatives _ du _) = du
derivative V (Derivatives _ _ dv) = dv

map :: (a -> b) -> Derivatives a -> Derivatives b
map function unmapped = do
  let mappedValue = function (value unmapped)
  let mappedU = map function (derivative U unmapped)
  let mappedV = mapV mappedU function (derivative V unmapped)
  Derivatives mappedValue mappedU mappedV

mapV :: Derivatives b -> (a -> b) -> Derivatives a -> Derivatives b
mapV mappedU function unmappedV = do
  let mappedValue = function (value unmappedV)
  let mappedUV = derivative V mappedU
  let mappedVV = mapV mappedUV function (derivative V unmappedV)
  Derivatives mappedValue mappedUV mappedVV
