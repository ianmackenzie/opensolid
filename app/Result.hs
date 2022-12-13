module Result (
    map,
    map2,
    andThen,
    withDefault,
    mapErr,
    (>>=),
    orErr,
) where

import OpenSolid

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Err _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map function (Ok value) = Ok (function value)
map _ (Err err) = Err err

map2 :: (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 function result1 result2 = do
    value1 <- result1
    value2 <- result2
    Ok (function value1 value2)

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function (Ok value) = function value
andThen _ (Err err) = Err err

mapErr :: (x -> y) -> Result x a -> Result y a
mapErr _ (Ok value) = Ok value
mapErr function (Err err) = Err (function err)

orErr :: y -> Result x a -> Result y a
orErr _ (Ok value) = Ok value
orErr err (Err _) = Err err
