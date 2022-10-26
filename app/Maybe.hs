module Maybe (
    map,
    map2,
    withDefault,
    (>>=),
) where

import qualified Data.Maybe
import OpenSolid
import qualified Prelude

map :: (a -> b) -> Maybe a -> Maybe b
map =
    Prelude.fmap

map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 function maybeA maybeB = Maybe.do
    valueA <- maybeA
    valueB <- maybeB
    Just (function valueA valueB)

withDefault :: a -> Maybe a -> a
withDefault value maybe =
    Data.Maybe.fromMaybe value maybe

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) =
    (Prelude.>>=)
