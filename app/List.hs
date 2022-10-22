module List (
    map,
    compact,
    collect,
    combine,
    concat,
    foldl,
    foldr,
    reverse,
) where

import qualified Data.List
import qualified Data.Maybe
import OpenSolid

map :: (a -> b) -> List a -> List b
map =
    Data.List.map

compact :: List (Maybe a) -> List a
compact =
    Data.Maybe.catMaybes

collect :: (a -> Maybe b) -> List a -> List b
collect =
    Data.Maybe.mapMaybe

combine :: (a -> List b) -> List a -> List b
combine function list =
    list >>= function

concat :: List (List a) -> List a
concat =
    Data.List.concat

foldl :: (b -> a -> b) -> b -> List a -> b
foldl =
    Data.List.foldl'

foldr :: (a -> b -> b) -> b -> List a -> b
foldr =
    Data.List.foldr

reverse :: List a -> List a
reverse =
    Data.List.reverse
