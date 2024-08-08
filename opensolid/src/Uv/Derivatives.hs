module Uv.Derivatives
  ( Derivatives
  , init
  , map
  , value
  , derivative
  )
where

import Uv (Parameter (U, V))

data Derivatives a
  = Derivatives a ~(Derivatives a) ~(Derivatives a)

init :: a -> (Parameter -> a -> a) -> Derivatives a
init f df = do
  let derivativesU = init (df U f) df
  Derivatives f derivativesU (initV derivativesU (df V f) (df V))

initV :: Derivatives a -> a -> (a -> a) -> Derivatives a
initV derivativesU fv dv = do
  let derivativesUV = derivative V derivativesU
  Derivatives fv derivativesUV (initV derivativesUV (dv fv) dv)

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
