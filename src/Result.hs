module Result
  ( map
  , andThen
  , withDefault
  , mapErr
  , (>>=)
  , orErr
  )
where

import OpenSolid

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Error _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map function (Ok value) = Ok (function value)
map _ (Error err) = Error err

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function (Ok value) = function value
andThen _ (Error err) = Error err

mapErr :: (x -> y) -> Result x a -> Result y a
mapErr _ (Ok value) = Ok value
mapErr function (Error err) = Error (function err)

orErr :: y -> Result x a -> Result y a
orErr _ (Ok value) = Ok value
orErr err (Error _) = Error err
