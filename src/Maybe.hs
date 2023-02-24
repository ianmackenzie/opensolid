module Maybe
  ( map
  , withDefault
  , orErr
  )
where

import OpenSolid
import Prelude qualified

map :: (a -> b) -> Maybe a -> Maybe b
map = Prelude.fmap

withDefault :: a -> Maybe a -> a
withDefault _ (Just value) = value
withDefault value Nothing = value

orErr :: x -> Maybe a -> Result x a
orErr _ (Just value) = Ok value
orErr err Nothing = Error err
