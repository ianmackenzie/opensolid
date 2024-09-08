module Uv.Derivatives
  ( Derivatives
  , init
  , map
  , get
  )
where

import OpenSolid
import Text qualified
import Uv (Parameter (U, V))
import Prelude qualified

data Derivatives a
  = Derivatives a ~(Derivatives a) ~(Derivatives a)

instance Show (Derivatives a) where
  show _ = Text.unpack "<Derivatives>"

instance Composition (Derivatives a) Parameter (Derivatives a) where
  (Derivatives _ du _) >> U = du
  (Derivatives _ _ dv) >> V = dv

init :: a -> (Parameter -> a -> a) -> Derivatives a
init f df = do
  let derivativesU = init (df U f) df
  Derivatives f derivativesU (initV derivativesU (df V f) (df V))

initV :: Derivatives a -> a -> (a -> a) -> Derivatives a
initV derivativesU fv dv = do
  let derivativesUV = derivativesU >> V
  Derivatives fv derivativesUV (initV derivativesUV (dv fv) dv)

get :: Derivatives a -> a
get (Derivatives f _ _) = f

map :: (a -> b) -> Derivatives a -> Derivatives b
map function unmapped = do
  let mappedValue = function (get unmapped)
  let mappedU = map function (unmapped >> U)
  let mappedV = mapV mappedU function (unmapped >> V)
  Derivatives mappedValue mappedU mappedV

mapV :: Derivatives b -> (a -> b) -> Derivatives a -> Derivatives b
mapV mappedU function unmappedV = do
  let mappedValue = function (get unmappedV)
  let mappedUV = mappedU >> V
  let mappedVV = mapV mappedUV function (unmappedV >> V)
  Derivatives mappedValue mappedUV mappedVV
