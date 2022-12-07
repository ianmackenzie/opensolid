module Maybe (
    map,
    map2,
    withDefault,
    (>>=),
    orErr,
) where

import OpenSolid
import qualified Prelude

map :: (a -> b) -> Maybe a -> Maybe b
map = fmap

map2 :: (a -> b -> c) -> Maybe a -> Maybe b -> Maybe c
map2 function maybeA maybeB = do
    valueA <- maybeA
    valueB <- maybeB
    Just (function valueA valueB)

withDefault :: a -> Maybe a -> a
withDefault _ (Just value) = value
withDefault value Nothing = value

(>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
(>>=) = (Prelude.>>=)

orErr :: x -> Maybe a -> Result x a
orErr _ (Just value) = Ok value
orErr err Nothing = Err err
