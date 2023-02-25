module Result
  ( (>>=)
  , map
  , andThen
  , withDefault
  , mapError
  )
where

import OpenSolid

(>>=) :: Result x a -> (a -> Result x b) -> Result x b
Ok value >>= function = function value
Error error >>= _ = Error error

withDefault :: a -> Result x a -> a
withDefault _ (Ok value) = value
withDefault fallback (Error _) = fallback

map :: (a -> value) -> Result x a -> Result x value
map function (Ok value) = Ok (function value)
map _ (Error err) = Error err

andThen :: (a -> Result x b) -> Result x a -> Result x b
andThen function (Ok value) = function value
andThen _ (Error err) = Error err

mapError :: IsError y => (x -> y) -> Result x a -> Result y a
mapError _ (Ok value) = Ok value
mapError function (Error err) = Error (function err)
